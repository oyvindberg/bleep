package bleep

import bleep.internal.codecs._
import bleep.internal.{FileUtils, ShortenAndSortJson}
import bleep.logging.Logger
import coursier.Fetch
import coursier.cache.{CacheDefaults, FileCache}
import coursier.core._
import coursier.error.CoursierError
import coursier.ivy.IvyRepository
import coursier.maven.MavenRepository
import coursier.params.ResolutionParams
import coursier.util.{Artifact, Task}
import io.circe._
import io.circe.syntax._

import java.io.File
import java.nio.file.{Files, Path}
import scala.collection.immutable.SortedSet

trait CoursierResolver {
  val params: CoursierResolver.Params

  def resolve(deps: SortedSet[model.Dep], versionCombo: model.VersionCombo): Either[CoursierError, CoursierResolver.Result]

  final def resolve(deps: Iterable[model.Dep], versionCombo: model.VersionCombo): Either[CoursierError, CoursierResolver.Result] =
    resolve(SortedSet.empty[model.Dep] ++ deps, versionCombo)

  final def force(deps: Set[model.Dep], versionCombo: model.VersionCombo, context: String): CoursierResolver.Result =
    resolve(deps, versionCombo) match {
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
        val params = Params(None, downloadSources = true, config.authentications, resolvers)
        val direct = new Direct(new BleepCacheLogger(pre.logger), params)
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
        MavenRepository(path.toUri.toString)
      case bleep.model.Repository.Maven(_, uri) =>
        MavenRepository(uri.toString).withAuthentication(authentications.flatMap(_.configs.get(uri)))
      case bleep.model.Repository.Ivy(_, uri) =>
        IvyRepository.fromPattern(uri.toString +: coursier.ivy.Pattern.default).withAuthentication(authentications.flatMap(_.configs.get(uri)))
    }

  case class InvalidVersionCombo(message: String) extends CoursierError(message)

  class Direct(val logger: BleepCacheLogger, val params: Params) extends CoursierResolver {

    val fileCache = FileCache[Task](params.overrideCacheFolder.getOrElse(CacheDefaults.location)).withLogger(logger)
    val repos = coursierRepos(params.repos, params.authentications)

    override def resolve(bleepDeps: SortedSet[model.Dep], versionCombo: model.VersionCombo): Either[CoursierError, CoursierResolver.Result] = {
      val maybeCoursierDependencies: Either[CoursierError, List[Dependency]] =
        bleepDeps.foldLeft[Either[CoursierError, List[Dependency]]](Right(Nil)) {
          case (e @ Left(_), _) => e
          case (Right(acc), bleepDep) =>
            bleepDep.asDependency(versionCombo) match {
              case Left(errorMessage) => Left(InvalidVersionCombo(errorMessage))
              case Right(coursierDep) => Right(coursierDep :: acc)
            }
        }

      def go(deps: List[Dependency], remainingAttempts: Int): Either[CoursierError, Fetch.Result] = {
        val newClassifiers = if (params.downloadSources) List(Classifier.sources) else Nil

        Fetch[Task](fileCache)
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
            logger.retrying(coursierError, newRemainingAttempts)
            go(deps, newRemainingAttempts)
          case other => other
        }
      }

      for {
        deps <- maybeCoursierDependencies
        res <- go(deps, remainingAttempts = 3)
      } yield CoursierResolver.Result(res.fullDetailedArtifacts, res.fullExtraArtifacts)
    }
  }

  // this is a performance cache, the real cache is the coursier folder
  private class Cached(logger: Logger, underlying: CoursierResolver, in: Path) extends CoursierResolver {
    override val params = underlying.params
    override def resolve(deps: SortedSet[model.Dep], versionCombo: model.VersionCombo): Either[CoursierError, CoursierResolver.Result] =
      if (deps.exists(_.version.endsWith("-SNAPSHOT"))) underlying.resolve(deps, versionCombo)
      else {
        val request = Cached.Request(deps, underlying.params, versionCombo)
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
            val ctxLogger = logger.withContext(cachePath).withContext(depNames).withContext(versionCombo.toString)
            ctxLogger.debug(s"coursier cache miss")
            underlying.resolve(deps, versionCombo).map {
              case changingResult if changingResult.fullDetailedArtifacts.exists { case (_, _, artifact, _) => artifact.changing } =>
                ctxLogger.info("Not caching because result is changing")
                changingResult
              case result =>
                FileUtils.writeString(cachePath, Cached.Both(request, result).asJson.noSpaces)
                result
            }
        }
      }
  }

  object Cached {
    case class Request(wanted: SortedSet[model.Dep], params: Params, versionCombo: model.VersionCombo)

    object Request {

      implicit val codec: Codec[Request] =
        Codec.forProduct3[Request, SortedSet[model.Dep], Params, model.VersionCombo]("wanted", "params", "forceScalaVersion")(Request.apply)(x =>
          (x.wanted, x.params, x.versionCombo)
        )
    }

    case class Both(request: Request, result: Result)
    object Both {
      implicit val codec: Codec[Both] = Codec.forProduct2[Both, Request, Result]("request", "result")(Both.apply)(x => (x.request, x.result))
    }
  }

  class TemplatedVersions(outer: CoursierResolver, maybeWantedBleepVersion: Option[model.BleepVersion]) extends CoursierResolver {
    override val params = outer.params
    override def resolve(deps: SortedSet[model.Dep], versionCombo: model.VersionCombo): Either[CoursierError, Result] = {
      val replacements = model.Replacements.versions(maybeWantedBleepVersion, versionCombo, includeEpoch = true, includeBinVersion = true)
      val rewrittenDeps = deps.map(replacements.fill.dep)
      outer.resolve(rewrittenDeps, versionCombo)
    }
  }
}
