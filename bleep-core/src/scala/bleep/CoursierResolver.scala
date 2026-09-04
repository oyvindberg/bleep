package bleep

import bleep.depcheck.CheckEvictions
import bleep.internal.codecs.*
import bleep.internal.coursierDeps.{configurationOrThrow, fullDetailedArtifactsOrThrow}
import bleep.internal.FileUtils
import coursier.cache.CacheDefaults
import coursier.core.*
import coursier.error.CoursierError
import coursier.ivy.IvyRepository
import coursier.maven.SbtMavenRepository
import coursier.params.ResolutionParams
import coursier.params.rule.SameVersion
import coursier.util.ModuleMatcher
import coursier.util.{Artifact, Task}
import coursier.version.VersionConstraint
import coursier.{Artifacts, Fetch, Resolution}
import io.circe.*
import io.circe.syntax.*
import ryddig.Logger

import java.io.File
import java.nio.file.{Files, Path}
import scala.collection.immutable.SortedSet

trait CoursierResolver {
  val params: CoursierResolver.Params
  def withParams(newParams: CoursierResolver.Params): CoursierResolver

  final def updatedParams(f: CoursierResolver.Params => CoursierResolver.Params): CoursierResolver =
    withParams(f(params))

  // uncached, raw result from coursier
  def direct(
      deps: SortedSet[model.Dep],
      versionCombo: model.VersionCombo,
      libraryVersionSchemes: SortedSet[model.LibraryVersionScheme],
      ignoreEvictionErrors: model.IgnoreEvictionErrors
  ): Either[CoursierError, Fetch.Result]

  def resolve(
      deps: SortedSet[model.Dep],
      versionCombo: model.VersionCombo,
      libraryVersionSchemes: SortedSet[model.LibraryVersionScheme],
      ignoreEvictionErrors: model.IgnoreEvictionErrors
  ): Either[CoursierError, CoursierResolver.Result]

  final def resolve(
      deps: Iterable[model.Dep],
      versionCombo: model.VersionCombo,
      libraryVersionSchemes: SortedSet[model.LibraryVersionScheme],
      ignoreEvictionErrors: model.IgnoreEvictionErrors
  ): Either[CoursierError, CoursierResolver.Result] =
    resolve(SortedSet.empty[model.Dep] ++ deps, versionCombo, libraryVersionSchemes, ignoreEvictionErrors)

  final def force(
      deps: Set[model.Dep],
      versionCombo: model.VersionCombo,
      libraryVersionSchemes: SortedSet[model.LibraryVersionScheme],
      context: String,
      ignoreEvictionErrors: model.IgnoreEvictionErrors
  ): CoursierResolver.Result =
    resolve(deps, versionCombo, libraryVersionSchemes, ignoreEvictionErrors) match {
      case Left(err)    => throw new BleepException.ResolveError(err, context)
      case Right(value) => value
    }
}

object CoursierResolver {
  case class Params(
      overrideCacheFolder: Option[File],
      downloadSources: Boolean,
      authentications: Option[model.Authentications],
      repos: List[model.Repository],
      /** Maven BOMs applied as version constraints to the resolution (dependencyManagement imports). Carried in Params rather than as a per-call argument so
        * the per-project override travels through every wrapper via `withParams`.
        *
        * NOTE: `Cached.computeHash`/`collisionHash` are hand-rolled selective hashes, NOT a hash of this whole case class. Any field added here that affects
        * resolution output MUST be mixed into both, or the disk cache will serve results computed without it — which is exactly how the first version of this
        * field shipped broken.
        */
      boms: SortedSet[model.Dep]
  )
  object Params {
    implicit val encoder: Encoder[Params] =
      Encoder.forProduct5[Params, Option[File], Boolean, Option[model.Authentications], List[model.Repository], SortedSet[model.Dep]](
        "overrideCacheFolder",
        "downloadSources",
        "authentications",
        "repos",
        "boms"
      )(x => (x.overrideCacheFolder, x.downloadSources, x.authentications, x.repos, x.boms))

    // `boms` was added after the on-disk resolve cache format existed, so it must decode as absent-means-empty: an entry written before the field carried no
    // boms, which is exactly the empty set. Making it required instead would reject every pre-existing cache file.
    implicit val decoder: Decoder[Params] =
      Decoder.instance { c =>
        for {
          overrideCacheFolder <- c.get[Option[File]]("overrideCacheFolder")
          downloadSources <- c.get[Boolean]("downloadSources")
          authentications <- c.get[Option[model.Authentications]]("authentications")
          repos <- c.get[List[model.Repository]]("repos")
          boms <- c.getOrElse[SortedSet[model.Dep]]("boms")(SortedSet.empty)
        } yield Params(overrideCacheFolder, downloadSources, authentications, repos, boms)
      }
  }

  trait Factory {
    def apply(pre: Prebootstrapped, config: model.BleepConfig, buildFile: model.BuildFile): CoursierResolver
  }

  object Factory {
    object default extends Factory {
      def apply(pre: Prebootstrapped, config: model.BleepConfig, buildFile: model.BuildFile): CoursierResolver = {
        lazy val replacements = model.Replacements.paths(pre.buildPaths.buildDir)

        val resolvers = buildFile.resolvers.values.map {
          case model.Repository.Maven(name, uri)        => model.Repository.Maven(name, replacements.fill.uri(uri))
          case model.Repository.MavenFolder(name, path) => model.Repository.MavenFolder(name, replacements.fill.path(path))
          case model.Repository.Ivy(name, uri)          => model.Repository.Ivy(name, replacements.fill.uri(uri))
        }

        val downloadSources = pre.buildPaths.variant match {
          case model.BuildVariant.Normal       => false
          case model.BuildVariant.BSP          => true
          case model.BuildVariant.Rewritten(_) => false
        }

        val credentialProvider = new CredentialProvider(pre.logger, config.authentications)
        val params = Params(None, downloadSources, config.authentications, resolvers, boms = SortedSet.empty)
        val direct = new Direct(pre.logger, pre.cacheLogger, params, credentialProvider)
        val cached = new Cached(pre.logger, direct, pre.userPaths.resolveCacheDir)
        new TemplatedVersions(cached, Some(buildFile.$version), Some(pre.buildPaths.buildDir))
      }
    }
  }

  // this is a simplified version of the original `Fetch.Result` with a json codec
  case class Result(
      fullDetailedArtifacts: Seq[(Dependency, Publication, Artifact, Option[File])],
      fullExtraArtifacts: Seq[(Artifact, Option[File])]
  ) {
    def detailedArtifacts: Seq[(Dependency, Publication, Artifact, File)] =
      fullDetailedArtifacts.collect { case (dep, pub, art, Some(file)) => (dep, pub, art, file) }

    def files: Seq[File] =
      detailedArtifacts.map(_._4)

    def jarFiles: List[File] =
      detailedArtifacts
        .collect { case (_, pub, _, file) if pub.ext == Extension.jar && pub.classifier != Classifier.sources && pub.classifier != Classifier.javadoc => file }
        .distinct
        .toList

    def jars: List[Path] =
      jarFiles.map(_.toPath)
  }

  private object Result {
    // format: off
    implicit val codecModule: Codec[Module] =
      Codec.forProduct3[Module, Organization, ModuleName, Map[String, String]]("organization", "name", "attributes")(Module.apply)(mod => (mod.organization, mod.name, mod.attributes))

    implicit val codecPublication: Codec[Publication] =
      Codec.forProduct4[Publication, String, Type, Extension, Classifier]("name", "type", "ext", "classifier")(Publication.apply)(mod => (mod.name, mod.`type`, mod.ext, mod.classifier))

    implicit val codecDependency: Codec[Dependency] =
      Codec.forProduct7[Dependency, Module, String, Configuration, Set[(Organization, ModuleName)], Publication, Boolean, Boolean]("module", "version", "configuration", "exclusions", "publication", "optional", "transitive")((module, version, configuration, exclusions, publication, optional, transitive) => Dependency(module, VersionConstraint(version), VariantSelector.ConfigurationBased(configuration), exclusions, publication, optional, transitive))(x => (x.module, x.versionConstraint.asString, x.configurationOrThrow, x.minimizedExclusions.toSet(), x.publication, x.optional, x.transitive))

    implicit val codecAuthentication: Codec[Authentication] =
      Codec.forProduct7[Authentication, Option[String], Option[String], Seq[(String, String)], Boolean, Option[String], Boolean, Boolean]("user", "passwordOpt", "httpHeaders", "optional", "realmOpt", "httpsOnly", "passOnRedirect")(Authentication.apply)(x => (x.userOpt, x.passwordOpt, x.httpHeaders, x.optional, x.realmOpt, x.httpsOnly, x.passOnRedirect))

    // break circular structure
    private implicit lazy val encoderMap: Encoder[Map[String, Artifact]] =
      Encoder.instance(a => Encoder.encodeMap(using KeyEncoder.encodeKeyString, codecArtifact).apply(a))

    private implicit lazy val decoderMap: Decoder[Map[String, Artifact]] =
      Decoder.instance(c => Decoder.decodeMap(using KeyDecoder.decodeKeyString, codecArtifact).apply(c))

    implicit lazy val codecArtifact: Codec[Artifact] =
      Codec.forProduct6[Artifact, String, Map[String, String], Map[String, Artifact], Boolean, Boolean, Option[Authentication]]("url", "checksumUrls", "extra", "changing", "optional", "authentication")(Artifact.apply)(x => (x.url, x.checksumUrls, x.extra, x.changing, x.optional, x.authentication))

    implicit val codecResult: Codec[Result] =
      Codec.forProduct2[Result, Seq[(Dependency, Publication, Artifact, Option[File])], Seq[(Artifact, Option[File])]]("fullDetailedArtifacts", "fullExtraArtifacts")(Result.apply)(x => (x.fullDetailedArtifacts, x.fullExtraArtifacts))
    // format: on
  }

  def coursierRepos(
      repos: List[model.Repository],
      authentications: Option[model.Authentications],
      credentialProvider: CredentialProvider,
      logger: Logger
  ): List[Repository] = {
    def tryResolvePrivateAuth(scheme: model.PrivateRepoScheme, uri: java.net.URI): Option[coursier.core.Authentication] =
      try Some(credentialProvider.resolve(scheme, uri))
      catch {
        case e: BleepException =>
          logger.warn(s"Could not acquire credentials for ${scheme.displayName} resolver ${uri}: ${e.message}")
          logger.warn(s"  Dependencies from this resolver will fail. Run '${scheme.cliCommand} auth login' to authenticate.")
          None
      }

    (repos ++ constants.DefaultRepos).map {
      case bleep.model.Repository.MavenFolder(_, path) =>
        SbtMavenRepository(path.toUri.toString)
      case bleep.model.Repository.Maven(_, uri) =>
        val (resolvedUri, auth) = model.PrivateRepoScheme.fromUri(uri) match {
          case Some(scheme) =>
            (scheme.toHttpsUri(uri), tryResolvePrivateAuth(scheme, uri))
          case None =>
            val staticAuth = authentications.flatMap(
              _.configs.get(uri).collect { case model.AuthEntry.Static(a) => a }
            )
            (uri, staticAuth)
        }
        SbtMavenRepository(resolvedUri.toString).withAuthentication(auth)
      case bleep.model.Repository.Ivy(_, uri) =>
        val (resolvedUri, auth) = model.PrivateRepoScheme.fromUri(uri) match {
          case Some(scheme) =>
            (scheme.toHttpsUri(uri), tryResolvePrivateAuth(scheme, uri))
          case None =>
            val staticAuth = authentications.flatMap(
              _.configs.get(uri).collect { case model.AuthEntry.Static(a) => a }
            )
            (uri, staticAuth)
        }
        IvyRepository.fromPattern(resolvedUri.toString +: coursier.ivy.Pattern.default).withAuthentication(auth)
    }
  }

  case class InvalidVersionCombo(message: String) extends CoursierError(message)

  /** A dependency was declared without a version (`org:name`), and no BOM in `boms:` manages its version. Raised BEFORE the fetch so the user gets an
    * actionable configuration error instead of coursier retrying an empty-version download and reporting a cryptic `…:"" not found`.
    */
  case class MissingBomVersion(message: String) extends CoursierError(message)

  class Direct(logger: Logger, val cacheLogger: BleepCacheLogger, val params: Params, credentialProvider: CredentialProvider) extends CoursierResolver {

    val fileCache = BleepFileCache.at(params.overrideCacheFolder.getOrElse(CacheDefaults.location)).withLogger(cacheLogger)
    lazy val repos = coursierRepos(params.repos, params.authentications, credentialProvider, logger)

    /** The version pins the declared BOMs imply, Maven-style. Computed once per resolver instance — params (and with them the BOM set) are immutable here. */
    lazy val bomPins: BomPins.Pins =
      if (params.boms.isEmpty) scala.collection.immutable.SortedMap.empty
      else BomPins(params.boms, fetchPomFile)

    private def fetchPomFile(g: String, a: String, v: String): Option[Path] = {
      val dep = Dependency(Module(Organization(g), ModuleName(a), Map.empty), VersionConstraint(v))
        .withTransitive(false)
        .withPublication(Publication(a, Type.pom, Extension.pom, Classifier.empty))
      Fetch[Task](fileCache)
        .withRepositories(repos)
        .withDependencies(Seq(dep))
        .withArtifactTypes(Set(Type.pom))
        .eitherResult() match {
        case Right(res) => res.files.headOption.map(_.toPath)
        case Left(_)    => None
      }
    }

    override def withParams(newParams: Params): CoursierResolver =
      new Direct(logger, cacheLogger, newParams, credentialProvider)

    override def direct(
        bleepDeps: SortedSet[model.Dep],
        versionCombo: model.VersionCombo,
        libraryVersionSchemes: SortedSet[model.LibraryVersionScheme],
        ignoreEvictionErrors: model.IgnoreEvictionErrors
    ): Either[CoursierError, Fetch.Result] = {
      val maybeCoursierDependencies: Either[CoursierError, List[Dependency]] =
        asCoursierDeps(bleepDeps, versionCombo)

      def go(deps: List[Dependency], bomDeps: List[Dependency], remainingAttempts: Int): Either[CoursierError, Fetch.Result] = {
        val newClassifiers = if (params.downloadSources) List(Classifier.sources) else Nil

        // SIP-51: For Scala 2.13 and 3, don't force scala-library version to match scala.version.
        // This allows dependency resolution to upgrade scala-library when dependencies need it,
        // implementing backwards-only binary compatibility. For older Scala versions (2.12, etc),
        // we still force the version to maintain forward binary compatibility.
        //
        // Matches sbt's behavior (https://github.com/sbt/sbt/pull/7480):
        // autoScalaLibrary.value && !ScalaArtifacts.isScala3(sv) && !Classpaths.isScala213(sv)

        // SIP-51: For Scala 2.13 and 3 (pre-3.8), add a SameVersion rule to ensure all Scala artifacts
        // (scala-library, scala-reflect, scala-compiler) stay at the same version. This is
        // necessary because of Scala's inlining - having different versions of these artifacts
        // can cause runtime errors.
        //
        // For Scala 3.8+, scala-reflect and scala-compiler are NOT part of the Scala 2 stdlib
        // anymore (the stdlib is compiled with Scala 3), so the SameVersion rule must NOT include
        // them — it would try to force scala-library:3.8.x == scala-reflect:2.13.x which is impossible.
        val scalaArtifactsSameVersionRule = SameVersion(
          Set(
            ModuleMatcher(Organization("org.scala-lang"), ModuleName("scala-library")),
            ModuleMatcher(Organization("org.scala-lang"), ModuleName("scala-reflect")),
            ModuleMatcher(Organization("org.scala-lang"), ModuleName("scala-compiler"))
          )
        )

        val resolutionParams = versionCombo.asScala match {
          case Some(sv) if sv.scalaVersion.is38OrLater =>
            // Scala 3.8+: stdlib is now a Scala 3 artifact. Don't force version, don't add
            // SameVersion rule (scala-reflect/scala-compiler don't exist as 3.x artifacts).
            ResolutionParams()
              .withForceScalaVersion(false)
              .withScalaVersionOpt0(Some(VersionConstraint(sv.scalaVersion.scalaVersion)))
          case Some(sv) if sv.scalaVersion.is3Or213 =>
            // Scala 2.13/3 (pre-3.8): disable forceScalaVersion to allow scala-library upgrades.
            // scala-library IS added as an explicit dependency with scalaVersion, which acts as
            // a floor constraint. The SameVersion rule ensures all Scala artifacts stay aligned.
            ResolutionParams()
              .withForceScalaVersion(false)
              .withScalaVersionOpt0(Some(VersionConstraint(sv.scalaVersion.scalaVersion)))
              .addRule(scalaArtifactsSameVersionRule)
          case _ =>
            // Scala 2.12 and older: force the scala version for forward binary compatibility
            ResolutionParams()
              .withForceScalaVersion(versionCombo.asScala.nonEmpty)
              .withScalaVersionOpt0(versionCombo.asScala.map(x => VersionConstraint(x.scalaVersion.scalaVersion)))
        }

        // BOM pins force transitive versions the way Maven's dependencyManagement does — except for
        // modules the build declares directly, where Maven lets the explicit declaration win.
        val directModules: Set[Module] = deps.iterator.map(_.module).toSet
        val pinnedResolutionParams = bomPins.foldLeft(resolutionParams) { case (rp, ((g, a), v)) =>
          val module = Module(Organization(g), ModuleName(a), Map.empty)
          if (directModules(module)) rp else rp.addForceVersion0((module, VersionConstraint(v)))
        }

        (try
          Fetch[Task](fileCache)
            .withArtifacts(Artifacts.apply(fileCache).withResolution(Resolution.apply()))
            .withRepositories(repos)
            .withDependencies(deps)
            // Deliberately NOT withForceOverrideVersions: that coursier path trips over BOM entries
            // carrying exclusions (see BomPins). Maven's pin-the-transitives semantics come from the
            // forceVersion entries computed above instead; the non-forced BOM contributes defaults
            // for versionless requests and the managed exclusions.
            .addBoms(bomDeps.map(_.asBomDependency)*)
            .withResolutionParams(pinnedResolutionParams)
            .withMainArtifacts(true)
            .addClassifiers(newClassifiers*)
            .eitherResult()
        catch {
          case e: RuntimeException =>
            // coursier's conflict-error FORMATTING can itself crash (its tree walk chokes on modules
            // that BOM-managed exclusions removed), hiding the actual conflict. Log everything we
            // know before letting it fly, or the failure is undebuggable through the BSP channel.
            logger
              .withContext("deps", deps.map(_.module.repr).mkString(", "))
              .withContext("boms", params.boms.map(_.repr).mkString(", "))
              .withContext("pins", bomPins.size)
              .error("dependency resolution failed while reporting its own error", e)
            throw e
        }) match {
          case Left(coursierError) if remainingAttempts > 0 =>
            val newRemainingAttempts = remainingAttempts - 1
            cacheLogger.retrying(coursierError, newRemainingAttempts)
            go(deps, bomDeps, newRemainingAttempts)
          case other => other
        }
      }

      // A version-less dependency (`org:name`) resolves only if a BOM supplies its version, and the authority on what a BOM supplies is coursier's own `addBoms`
      // below — NOT our `bomPins`, which is a re-derivation that can miss, say, a version written through a property it cannot interpolate. So only the
      // provably-wrong case is caught here: a version-less dependency with NO BOMs imported at all, where nothing could possibly manage it. When BOMs ARE
      // present we defer to coursier; if it still cannot fill the version, its resolution error surfaces rather than a false rejection from us.
      // Judged on the MODEL deps, not the coursier ones: `version.isEmpty` means the user wrote `org:name` without a version, which is the only thing this
      // catches. A coursier dependency can carry an empty version constraint for reasons that are not the user's doing (an inter-project or bleep-injected dep
      // resolved elsewhere), and flagging those would break ordinary resolutions.
      def checkManaged(): Either[CoursierError, Unit] =
        if (params.boms.nonEmpty) Right(())
        else {
          val coords = bleepDeps.iterator.filter(_.version.isEmpty).map(_.repr).toList
          if (coords.isEmpty) Right(())
          else {
            val what = if (coords.size == 1) "A dependency is" else s"${coords.size} dependencies are"
            Left(
              MissingBomVersion(
                s"$what declared without a version, and no BOM is imported to supply one: ${coords.mkString(", ")}. " +
                  "Give it a version, or import a BOM in `boms:` that provides one."
              )
            )
          }
        }

      for {
        deps <- maybeCoursierDependencies
        _ <- checkManaged()
        bomDeps <- asCoursierDeps(params.boms, versionCombo)
        res <- go(deps, bomDeps, remainingAttempts = 3)
        _ <- CheckEvictions(versionCombo, deps, libraryVersionSchemes.toList, res, logger, Some(ignoreEvictionErrors))
      } yield res
    }

    override def resolve(
        bleepDeps: SortedSet[model.Dep],
        versionCombo: model.VersionCombo,
        libraryVersionSchemes: SortedSet[model.LibraryVersionScheme],
        ignoreEvictionErrors: model.IgnoreEvictionErrors
    ): Either[CoursierError, CoursierResolver.Result] =
      direct(bleepDeps, versionCombo, libraryVersionSchemes, ignoreEvictionErrors)
        .map(res => CoursierResolver.Result(res.fullDetailedArtifactsOrThrow, res.fullExtraArtifacts))
  }

  def asCoursierDeps(bleepDeps: SortedSet[model.Dep], versionCombo: model.VersionCombo): Either[CoursierError, List[Dependency]] =
    bleepDeps.foldLeft[Either[CoursierError, List[Dependency]]](Right(Nil)) {
      case (e @ Left(_), _)       => e
      case (Right(acc), bleepDep) =>
        bleepDep.asDependency(versionCombo) match {
          case Left(errorMessage) => Left(InvalidVersionCombo(errorMessage))
          case Right(coursierDep) => Right(coursierDep :: acc)
        }
    }

  // this is a performance cache, the real cache is the coursier folder
  private class Cached(logger: Logger, underlying: CoursierResolver, in: Path) extends CoursierResolver {
    override val params = underlying.params

    override def withParams(newParams: Params): CoursierResolver =
      new Cached(logger, underlying.withParams(newParams), in)

    override def resolve(
        deps: SortedSet[model.Dep],
        versionCombo: model.VersionCombo,
        libraryVersionSchemes: SortedSet[model.LibraryVersionScheme],
        ignoreEvictionErrors: model.IgnoreEvictionErrors
    ): Either[CoursierError, CoursierResolver.Result] =
      if (deps.exists(_.version.endsWith("-SNAPSHOT"))) underlying.resolve(deps, versionCombo, libraryVersionSchemes, ignoreEvictionErrors)
      else {
        // Fast hash computation - avoids JSON encoding entirely
        val digest = Cached.computeHash(deps, underlying.params, versionCombo, libraryVersionSchemes)
        val cachePath = in / s"$digest.json"

        val cachedResult: Option[Result] =
          if (Files.exists(cachePath)) {
            Cached.readLeanCache(logger, cachePath, deps, underlying.params, versionCombo, libraryVersionSchemes) match {
              case Some(result) => Some(result)
              case None         =>
                Files.delete(cachePath)
                None
            }
          } else None

        cachedResult match {
          case Some(value) => Right(value)
          case None        =>
            val depNames = deps.map(_.baseModuleName.value)
            val ctxLogger = logger.withContext("cachePath", cachePath).withContext("depNames", depNames).withContext("versionCombo", versionCombo.toString)
            ctxLogger.debug(s"coursier cache miss")
            underlying.resolve(deps, versionCombo, libraryVersionSchemes, ignoreEvictionErrors).map {
              case changingResult if changingResult.fullDetailedArtifacts.exists { case (_, _, artifact, _) => artifact.changing } =>
                ctxLogger.info("Not caching because result is changing")
                changingResult
              case result =>
                Cached.writeLeanCache(logger, cachePath, deps, underlying.params, versionCombo, libraryVersionSchemes, result)
                result
            }
        }
      }

    override def direct(
        deps: SortedSet[model.Dep],
        versionCombo: model.VersionCombo,
        libraryVersionSchemes: SortedSet[model.LibraryVersionScheme],
        ignoreEvictionErrors: model.IgnoreEvictionErrors
    ): Either[CoursierError, Fetch.Result] =
      underlying.direct(deps, versionCombo, libraryVersionSchemes, ignoreEvictionErrors)
  }

  object Cached {
    import scala.util.hashing.MurmurHash3

    // Fast hash computation using MurmurHash3 - no string/JSON allocation
    def computeHash(
        deps: SortedSet[model.Dep],
        params: Params,
        versionCombo: model.VersionCombo,
        libraryVersionSchemes: SortedSet[model.LibraryVersionScheme]
    ): Int = {
      var h = MurmurHash3.arraySeed
      // Deps are sorted, iterate deterministically
      deps.foreach { dep =>
        h = MurmurHash3.mix(h, dep.organization.value.hashCode)
        h = MurmurHash3.mix(h, dep.baseModuleName.value.hashCode)
        h = MurmurHash3.mix(h, dep.version.hashCode)
        h = MurmurHash3.mix(h, dep.configuration.value.hashCode)
        dep.exclusions.value.foreach { case (org, mods) =>
          h = MurmurHash3.mix(h, org.value.hashCode)
          mods.values.foreach(mod => h = MurmurHash3.mix(h, mod.value.hashCode))
        }
      }
      h = MurmurHash3.mix(h, versionCombo.toString.hashCode)
      h = MurmurHash3.mix(h, if (params.downloadSources) 1 else 0)
      params.repos.foreach(repo => h = MurmurHash3.mix(h, repo.toString.hashCode))
      params.boms.foreach { bom =>
        h = MurmurHash3.mix(h, bom.organization.value.hashCode)
        h = MurmurHash3.mix(h, bom.baseModuleName.value.hashCode)
        h = MurmurHash3.mix(h, bom.version.hashCode)
      }
      libraryVersionSchemes.foreach { lvs =>
        h = MurmurHash3.mix(h, lvs.dep.organization.value.hashCode)
        h = MurmurHash3.mix(h, lvs.dep.baseModuleName.value.hashCode)
        h = MurmurHash3.mix(h, lvs.scheme.value.hashCode)
      }
      MurmurHash3.finalizeHash(h, deps.size + libraryVersionSchemes.size + params.boms.size)
    }

    // Lean cache format - stores only what's needed for reconstruction
    case class LeanArtifact(
        org: String,
        name: String,
        version: String,
        classifier: String, // empty string for no classifier
        ext: String,
        path: String
    )
    object LeanArtifact {
      implicit val codec: Codec[LeanArtifact] =
        Codec.forProduct6[LeanArtifact, String, String, String, String, String, String](
          "o",
          "n",
          "v",
          "c",
          "e",
          "p"
        )(LeanArtifact.apply)(x => (x.org, x.name, x.version, x.classifier, x.ext, x.path))
    }

    case class LeanCache(
        // Store hash of request for collision detection
        requestHash: Int,
        artifacts: List[LeanArtifact]
    )
    object LeanCache {
      implicit val codec: Codec[LeanCache] =
        Codec.forProduct2[LeanCache, Int, List[LeanArtifact]]("h", "a")(LeanCache.apply)(x => (x.requestHash, x.artifacts))
    }

    // Secondary hash for collision detection — uses different seed but MUST hash
    // the same fields as computeHash to avoid false positives
    private def collisionHash(
        deps: SortedSet[model.Dep],
        params: Params,
        versionCombo: model.VersionCombo,
        libraryVersionSchemes: SortedSet[model.LibraryVersionScheme]
    ): Int = {
      var h = 0x9e3779b9 // golden ratio bits — different seed than computeHash
      deps.foreach { dep =>
        h = MurmurHash3.mix(h, dep.organization.value.hashCode)
        h = MurmurHash3.mix(h, dep.baseModuleName.value.hashCode)
        h = MurmurHash3.mix(h, dep.version.hashCode)
        h = MurmurHash3.mix(h, dep.configuration.value.hashCode)
        dep.exclusions.value.foreach { case (org, mods) =>
          h = MurmurHash3.mix(h, org.value.hashCode)
          mods.values.foreach(mod => h = MurmurHash3.mix(h, mod.value.hashCode))
        }
      }
      h = MurmurHash3.mix(h, versionCombo.toString.hashCode)
      h = MurmurHash3.mix(h, if (params.downloadSources) 1 else 0)
      params.repos.foreach(repo => h = MurmurHash3.mix(h, repo.toString.hashCode))
      params.boms.foreach { bom =>
        h = MurmurHash3.mix(h, bom.organization.value.hashCode)
        h = MurmurHash3.mix(h, bom.baseModuleName.value.hashCode)
        h = MurmurHash3.mix(h, bom.version.hashCode)
      }
      libraryVersionSchemes.foreach { lvs =>
        h = MurmurHash3.mix(h, lvs.dep.organization.value.hashCode)
        h = MurmurHash3.mix(h, lvs.dep.baseModuleName.value.hashCode)
        h = MurmurHash3.mix(h, lvs.scheme.value.hashCode)
      }
      MurmurHash3.finalizeHash(h, deps.size + libraryVersionSchemes.size + params.boms.size)
    }

    def readLeanCache(
        logger: Logger,
        cachePath: Path,
        deps: SortedSet[model.Dep],
        params: Params,
        versionCombo: model.VersionCombo,
        libraryVersionSchemes: SortedSet[model.LibraryVersionScheme]
    ): Option[Result] = {
      val content = Files.readString(cachePath)
      parser.decode[LeanCache](content) match {
        case Right(lean) if lean.requestHash == collisionHash(deps, params, versionCombo, libraryVersionSchemes) =>
          // Reconstruct Result from lean format
          val detailedArtifacts = lean.artifacts.map { la =>
            val module = Module(Organization(la.org), ModuleName(la.name), Map.empty)
            val dep = Dependency(module, VersionConstraint(la.version))
            val pub = Publication(
              la.name,
              if (la.ext == "jar") Type.jar else Type(la.ext),
              Extension(la.ext),
              if (la.classifier.isEmpty) Classifier.empty else Classifier(la.classifier)
            )
            val artifact =
              Artifact(java.nio.file.Path.of(la.path).toUri.toASCIIString, Map.empty, Map.empty, changing = false, optional = false, authentication = None)
            val file = if (la.path.nonEmpty) Some(new File(la.path)) else None
            (dep, pub, artifact, file)
          }
          val result = Result(detailedArtifacts, Seq.empty)
          if (result.files.forall(_.exists())) Some(result)
          else {
            logger.info("coursier cache references missing files, re-resolving")
            None
          }
        case Right(_) =>
          logger.debug("coursier cache collision detected")
          None
        case Left(err) =>
          logger.warn(s"coursier cache corrupted: ${err.getMessage}")
          None
      }
    }

    def writeLeanCache(
        logger: Logger,
        cachePath: Path,
        deps: SortedSet[model.Dep],
        params: Params,
        versionCombo: model.VersionCombo,
        libraryVersionSchemes: SortedSet[model.LibraryVersionScheme],
        result: Result
    ): Unit = {
      val leanArtifacts = result.fullDetailedArtifacts.map { case (dep, pub, _, fileOpt) =>
        LeanArtifact(
          org = dep.module.organization.value,
          name = dep.module.name.value,
          version = dep.versionConstraint.asString,
          classifier = if (pub.classifier == Classifier.empty) "" else pub.classifier.value,
          ext = pub.ext.value,
          path = fileOpt.map(_.getAbsolutePath).getOrElse("")
        )
      }.toList

      val lean = LeanCache(
        requestHash = collisionHash(deps, params, versionCombo, libraryVersionSchemes),
        artifacts = leanArtifacts
      )

      FileUtils.writeString(logger, None, cachePath, lean.asJson.noSpaces)
    }

    // Request type kept for TestResolver compatibility (in-memory cache + JSON serialization)
    case class Request(
        wanted: SortedSet[model.Dep],
        params: Params,
        versionCombo: model.VersionCombo,
        libraryVersionSchemes: SortedSet[model.LibraryVersionScheme]
    )

    object Request {
      implicit val codec: Codec[Request] =
        Codec.forProduct4[Request, SortedSet[model.Dep], Params, model.VersionCombo, SortedSet[model.LibraryVersionScheme]](
          "wanted",
          "params",
          "forceScalaVersion",
          "libraryVersionSchemes"
        )(Request.apply)(x => (x.wanted, x.params, x.versionCombo, x.libraryVersionSchemes))
    }
  }

  class TemplatedVersions(underlying: CoursierResolver, maybeWantedBleepVersion: Option[model.BleepVersion], buildDir: Option[Path]) extends CoursierResolver {
    override val params = underlying.params

    override def withParams(newParams: Params): CoursierResolver =
      new TemplatedVersions(underlying.withParams(newParams), maybeWantedBleepVersion, buildDir)

    override def resolve(
        deps: SortedSet[model.Dep],
        versionCombo: model.VersionCombo,
        libraryVersionSchemes: SortedSet[model.LibraryVersionScheme],
        ignoreEvictionErrors: model.IgnoreEvictionErrors
    ): Either[CoursierError, Result] = {
      val rewritten = rewriteDeps(deps, versionCombo)
      val (devDeps, normalDeps) = rewritten.partition(isDevDep)
      // Resolve dev deps from class dirs (either from buildDir or JVM classpath)
      val devClassDirs = devDeps.toList.flatMap(BleepDevDeps.resolveAllClassDirs)

      val coursierResult =
        if (normalDeps.isEmpty) Right(Result(Seq.empty, Seq.empty))
        else underlying.resolve(normalDeps, versionCombo, libraryVersionSchemes, ignoreEvictionErrors)

      coursierResult.map { result =>
        if (devClassDirs.isEmpty) result
        else {
          val syntheticEntries = devClassDirs.map { classDir =>
            val module = coursier.core.Module(
              Organization("build.bleep"),
              ModuleName(classDir.getParent.getFileName.toString),
              Map.empty
            )
            val dep = Dependency(module, VersionConstraint("dev"))
            val pub = Publication("", Type.jar, Extension.jar, Classifier.empty)
            val artifact =
              Artifact(classDir.toUri.toASCIIString, Map.empty, Map.empty, changing = false, optional = false, authentication = None)
            (dep, pub, artifact, Some(classDir.toFile))
          }
          Result(result.fullDetailedArtifacts ++ syntheticEntries, result.fullExtraArtifacts)
        }
      }
    }

    override def direct(
        deps: SortedSet[model.Dep],
        versionCombo: model.VersionCombo,
        libraryVersionSchemes: SortedSet[model.LibraryVersionScheme],
        ignoreEvictionErrors: model.IgnoreEvictionErrors
    ): Either[CoursierError, Fetch.Result] =
      underlying.direct(rewriteDeps(deps, versionCombo), versionCombo, libraryVersionSchemes, ignoreEvictionErrors)

    private def isDevDep(dep: model.Dep): Boolean =
      dep.organization.value == "build.bleep" && BleepDevDeps.isDevVersion(dep.version)

    def rewriteDeps(deps: SortedSet[model.Dep], versionCombo: model.VersionCombo): SortedSet[model.Dep] = {
      val replacements = model.Replacements.versions(maybeWantedBleepVersion, versionCombo, includeEpoch = true, includeBinVersion = true, buildDir = buildDir)
      val rewrittenDeps = deps.map(replacements.fill.dep)
      rewrittenDeps
    }
  }
}
