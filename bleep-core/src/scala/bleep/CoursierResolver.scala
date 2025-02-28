package bleep

import bleep.depcheck.CheckEvictions
import bleep.internal.codecs.*
import bleep.internal.{FileUtils, ShortenAndSortJson}
import coursier.cache.{CacheDefaults, FileCache}
import coursier.core.*
import coursier.error.CoursierError
import coursier.ivy.IvyRepository
import coursier.maven.SbtMavenRepository
import coursier.params.ResolutionParams
import coursier.util.{Artifact, Task}
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
      libraryVersionSchemes: SortedSet[model.LibraryVersionScheme]
  ): Either[CoursierError, Fetch.Result]

  def resolve(
      deps: SortedSet[model.Dep],
      versionCombo: model.VersionCombo,
      libraryVersionSchemes: SortedSet[model.LibraryVersionScheme]
  ): Either[CoursierError, CoursierResolver.Result]

  final def resolve(
      deps: Iterable[model.Dep],
      versionCombo: model.VersionCombo,
      libraryVersionSchemes: SortedSet[model.LibraryVersionScheme]
  ): Either[CoursierError, CoursierResolver.Result] =
    resolve(SortedSet.empty[model.Dep] ++ deps, versionCombo, libraryVersionSchemes)

  final def force(
      deps: Set[model.Dep],
      versionCombo: model.VersionCombo,
      libraryVersionSchemes: SortedSet[model.LibraryVersionScheme],
      context: String
  ): CoursierResolver.Result =
    resolve(deps, versionCombo, libraryVersionSchemes) match {
      case Left(err)    => throw new BleepException.ResolveError(err, context)
      case Right(value) => value
    }
}

object CoursierResolver {
  case class Params(
      overrideCacheFolder: Option[File],
      downloadSources: Boolean,
      authentications: Option[model.Authentications],
      repos: List[model.Repository]
  )
  object Params {
    implicit val codec: Codec[Params] =
      Codec.forProduct4[Params, Option[File], Boolean, Option[model.Authentications], List[model.Repository]](
        "overrideCacheFolder",
        "downloadSources",
        "authentications",
        "repos"
      )(
        Params.apply
      )(x => (x.overrideCacheFolder, x.downloadSources, x.authentications, x.repos))
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

        val params = Params(None, downloadSources, config.authentications, resolvers)
        val direct = new Direct(pre.logger, pre.cacheLogger, params)
        val cached = new Cached(pre.logger, direct, pre.userPaths.resolveCacheDir)
        new TemplatedVersions(cached, Some(buildFile.$version))
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
      Codec.forProduct7[Dependency, Module, String, Configuration, Set[(Organization, ModuleName)], Publication, Boolean, Boolean]("module", "version", "configuration", "exclusions", "publication", "optional", "transitive")(Dependency.apply)(x => (x.module, x.version, x.configuration, x.minimizedExclusions.toSet(), x.publication, x.optional, x.transitive))

    implicit val codecAuthentication: Codec[Authentication] =
      Codec.forProduct7[Authentication, String, Option[String], Seq[(String, String)], Boolean, Option[String], Boolean, Boolean]("user", "passwordOpt", "httpHeaders", "optional", "realmOpt", "httpsOnly", "passOnRedirect")(Authentication.apply)(x => (x.user, x.passwordOpt, x.httpHeaders, x.optional, x.realmOpt, x.httpsOnly, x.passOnRedirect))

    // break circular structure
    private implicit lazy val encoderMap: Encoder[Map[String, Artifact]] =
      Encoder.instance(a => Encoder.encodeMap(KeyEncoder.encodeKeyString, codecArtifact).apply(a))

    private implicit lazy val decoderMap: Decoder[Map[String, Artifact]] =
      Decoder.instance(c => Decoder.decodeMap(KeyDecoder.decodeKeyString, codecArtifact).apply(c))

    implicit lazy val codecArtifact: Codec[Artifact] =
      Codec.forProduct6[Artifact, String, Map[String, String], Map[String, Artifact], Boolean, Boolean, Option[Authentication]]("url", "checksumUrls", "extra", "changing", "optional", "authentication")(Artifact.apply)(x => (x.url, x.checksumUrls, x.extra, x.changing, x.optional, x.authentication))

    implicit val codecResult: Codec[Result] =
      Codec.forProduct2[Result, Seq[(Dependency, Publication, Artifact, Option[File])], Seq[(Artifact, Option[File])]]("fullDetailedArtifacts", "fullExtraArtifacts")(Result.apply)(x => (x.fullDetailedArtifacts, x.fullExtraArtifacts))
    // format: on
  }

  def coursierRepos(repos: List[model.Repository], authentications: Option[model.Authentications]): List[Repository] =
    (repos ++ constants.DefaultRepos).map {
      case bleep.model.Repository.MavenFolder(_, path) =>
        SbtMavenRepository(path.toUri.toString)
      case bleep.model.Repository.Maven(_, uri) =>
        SbtMavenRepository(uri.toString).withAuthentication(authentications.flatMap(_.configs.get(uri)))
      case bleep.model.Repository.Ivy(_, uri) =>
        IvyRepository.fromPattern(uri.toString +: coursier.ivy.Pattern.default).withAuthentication(authentications.flatMap(_.configs.get(uri)))
    }

  case class InvalidVersionCombo(message: String) extends CoursierError(message)

  class Direct(logger: Logger, val cacheLogger: BleepCacheLogger, val params: Params) extends CoursierResolver {

    val fileCache = FileCache[Task](params.overrideCacheFolder.getOrElse(CacheDefaults.location)).withLogger(cacheLogger)
    val repos = coursierRepos(params.repos, params.authentications)

    override def withParams(newParams: Params): CoursierResolver =
      new Direct(logger, cacheLogger, newParams)

    override def direct(
        bleepDeps: SortedSet[model.Dep],
        versionCombo: model.VersionCombo,
        libraryVersionSchemes: SortedSet[model.LibraryVersionScheme]
    ): Either[CoursierError, Fetch.Result] = {
      val maybeCoursierDependencies: Either[CoursierError, List[Dependency]] =
        asCoursierDeps(bleepDeps, versionCombo)

      def go(deps: List[Dependency], remainingAttempts: Int): Either[CoursierError, Fetch.Result] = {
        val newClassifiers = if (params.downloadSources) List(Classifier.sources) else Nil

        Fetch[Task](fileCache)
          .withArtifacts(Artifacts.apply(fileCache).withResolution(Resolution.apply()))
          .withRepositories(repos)
          .withDependencies(deps)
          .withResolutionParams(
            ResolutionParams()
              .withForceScalaVersion(versionCombo.asScala.nonEmpty)
              .withScalaVersionOpt(versionCombo.asScala.map(_.scalaVersion.scalaVersion))
          )
          .withMainArtifacts(true)
          .addClassifiers(newClassifiers: _*)
          .eitherResult() match {
          case Left(coursierError) if remainingAttempts > 0 =>
            val newRemainingAttempts = remainingAttempts - 1
            cacheLogger.retrying(coursierError, newRemainingAttempts)
            go(deps, newRemainingAttempts)
          case other => other
        }
      }

      for {
        deps <- maybeCoursierDependencies
        res <- go(deps, remainingAttempts = 3)
        _ <- CheckEvictions(versionCombo, deps, libraryVersionSchemes.toList, res, logger)
      } yield res
    }

    override def resolve(
        bleepDeps: SortedSet[model.Dep],
        versionCombo: model.VersionCombo,
        libraryVersionSchemes: SortedSet[model.LibraryVersionScheme]
    ): Either[CoursierError, CoursierResolver.Result] =
      direct(bleepDeps, versionCombo, libraryVersionSchemes)
        .map(res => CoursierResolver.Result(res.fullDetailedArtifacts, res.fullExtraArtifacts))
  }

  def asCoursierDeps(bleepDeps: SortedSet[model.Dep], versionCombo: model.VersionCombo): Either[CoursierError, List[Dependency]] =
    bleepDeps.foldLeft[Either[CoursierError, List[Dependency]]](Right(Nil)) {
      case (e @ Left(_), _) => e
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
        libraryVersionSchemes: SortedSet[model.LibraryVersionScheme]
    ): Either[CoursierError, CoursierResolver.Result] =
      if (deps.exists(_.version.endsWith("-SNAPSHOT"))) underlying.resolve(deps, versionCombo, libraryVersionSchemes)
      else {
        val request = Cached.Request(deps, underlying.params, versionCombo, libraryVersionSchemes)
        val digest = request.asJson.foldWith(ShortenAndSortJson(Nil)).noSpaces.hashCode // should hopefully be stable
        val cachePath = in / s"$digest.json"

        val cachedResult: Option[Result] =
          if (Files.exists(cachePath)) {
            parser.decode[Cached.Both](Files.readString(cachePath)) match {
              // collision detection is done here: handle it by just overwriting the file
              case Right(Cached.Both(`request`, result)) if result.files.forall(_.exists()) =>
                Some(result)
              case _ =>
                logger.warn(s"coursier cache collision. deleting")
                Files.delete(cachePath)
                None
            }
          } else None

        cachedResult match {
          case Some(value) => Right(value)
          case None =>
            val depNames = deps.map(_.baseModuleName.value)
            val ctxLogger = logger.withContext("cachePath", cachePath).withContext("depNames", depNames).withContext("versionCombo", versionCombo.toString)
            ctxLogger.debug(s"coursier cache miss")
            underlying.resolve(deps, versionCombo, libraryVersionSchemes).map {
              case changingResult if changingResult.fullDetailedArtifacts.exists { case (_, _, artifact, _) => artifact.changing } =>
                ctxLogger.info("Not caching because result is changing")
                changingResult
              case result =>
                FileUtils.writeString(logger, None, cachePath, Cached.Both(request, result).asJson.noSpaces)
                result
            }
        }
      }

    override def direct(
        deps: SortedSet[model.Dep],
        versionCombo: model.VersionCombo,
        libraryVersionSchemes: SortedSet[model.LibraryVersionScheme]
    ): Either[CoursierError, Fetch.Result] =
      underlying.direct(deps, versionCombo, libraryVersionSchemes)
  }

  object Cached {
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

    case class Both(request: Request, result: Result)
    object Both {
      implicit val codec: Codec[Both] = Codec.forProduct2[Both, Request, Result]("request", "result")(Both.apply)(x => (x.request, x.result))
    }
  }

  class TemplatedVersions(underlying: CoursierResolver, maybeWantedBleepVersion: Option[model.BleepVersion]) extends CoursierResolver {
    override val params = underlying.params

    override def withParams(newParams: Params): CoursierResolver =
      new TemplatedVersions(underlying.withParams(newParams), maybeWantedBleepVersion)

    override def resolve(
        deps: SortedSet[model.Dep],
        versionCombo: model.VersionCombo,
        libraryVersionSchemes: SortedSet[model.LibraryVersionScheme]
    ): Either[CoursierError, Result] =
      underlying.resolve(rewriteDeps(deps, versionCombo), versionCombo, libraryVersionSchemes)

    override def direct(
        deps: SortedSet[model.Dep],
        versionCombo: model.VersionCombo,
        libraryVersionSchemes: SortedSet[model.LibraryVersionScheme]
    ): Either[CoursierError, Fetch.Result] =
      underlying.direct(rewriteDeps(deps, versionCombo), versionCombo, libraryVersionSchemes)

    def rewriteDeps(deps: SortedSet[model.Dep], versionCombo: model.VersionCombo): SortedSet[model.Dep] = {
      val replacements = model.Replacements.versions(maybeWantedBleepVersion, versionCombo, includeEpoch = true, includeBinVersion = true)
      val rewrittenDeps = deps.map(replacements.fill.dep)
      rewrittenDeps
    }
  }
}
