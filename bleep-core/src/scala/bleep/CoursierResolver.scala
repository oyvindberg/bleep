package bleep

import bleep.internal.codecs._
import bleep.internal.FileUtils
import bleep.logging.Logger
import coursier.Fetch
import coursier.cache.{ArtifactError, FileCache}
import coursier.core._
import coursier.error.{CoursierError, FetchError, ResolutionError}
import coursier.ivy.IvyRepository
import coursier.maven.MavenRepository
import coursier.params.ResolutionParams
import coursier.util.{Artifact, Task}
import io.circe._
import io.circe.syntax._

import java.io.File
import java.nio.file.{Files, Path}

trait CoursierResolver {
  val params: CoursierResolver.Params

  def resolve(deps: Set[model.Dep], versionCombo: model.VersionCombo): Either[CoursierError, CoursierResolver.Result]

  final def force(deps: Set[model.Dep], versionCombo: model.VersionCombo, context: String): CoursierResolver.Result =
    resolve(deps, versionCombo) match {
      case Left(err)    => throw new BleepException.ResolveError(err, context)
      case Right(value) => value
    }
}

object CoursierResolver {
  case class Params(
      downloadSources: Boolean,
      authentications: Option[model.Authentications],
      repos: List[model.Repository]
  )
  object Params {
    implicit val codec: Codec[Params] =
      Codec.forProduct3[Params, Boolean, Option[model.Authentications], List[model.Repository]]("downloadSources", "authentications", "repos")(
        Params.apply
      )(x => (x.downloadSources, x.authentications, x.repos))
  }

  trait Factory {
    def apply(pre: Prebootstrapped, config: model.BleepConfig, buildFile: model.BuildFile): CoursierResolver
  }

  object Factory {
    object default extends Factory {
      def apply(pre: Prebootstrapped, config: model.BleepConfig, buildFile: model.BuildFile): CoursierResolver =
        CoursierResolver(
          buildFile.resolvers.values,
          pre.logger,
          downloadSources = true,
          pre.userPaths.coursierCacheDir,
          config.authentications,
          Some(buildFile.$version)
        )
    }
  }

  def apply(
      repos: List[model.Repository],
      logger: Logger,
      downloadSources: Boolean,
      cacheIn: Path,
      authentications: Option[model.Authentications],
      wantedBleepVersion: Option[model.BleepVersion]
  ): CoursierResolver = {
    val params = Params(downloadSources, authentications, repos)
    val direct = new Direct(new BleepCacheLogger(logger), params)
    val cached = new Cached(logger, direct, cacheIn)
    new TemplatedVersions(cached, wantedBleepVersion)
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
      case bleep.model.Repository.Folder(_, path) =>
        // Repository.Folder is derived from sbt.librarymanagement.FileRepository, which can be both ivy and maven.
        MavenRepository(path.toString)
      case bleep.model.Repository.Maven(_, uri) =>
        MavenRepository(uri.toString).withAuthentication(authentications.flatMap(_.configs.get(uri)))
      case bleep.model.Repository.Ivy(_, uri) =>
        IvyRepository.fromPattern(uri.toString +: coursier.ivy.Pattern.default).withAuthentication(authentications.flatMap(_.configs.get(uri)))
    }

  private class Direct(val logger: BleepCacheLogger, val params: Params) extends CoursierResolver {

    val fileCache = FileCache[Task]().withLogger(logger)

    override def resolve(deps: Set[model.Dep], versionCombo: model.VersionCombo): Either[CoursierError, CoursierResolver.Result] = {
      def go(remainingAttempts: Int): Either[CoursierError, Fetch.Result] = {
        val newClassifiers = if (params.downloadSources) List(Classifier.sources) else Nil

        Fetch[Task](fileCache)
          .withRepositories(coursierRepos(params.repos, params.authentications))
          .withDependencies(deps.toList.sorted.map(_.asDependency(versionCombo).orThrowText))
          .withResolutionParams(
            ResolutionParams()
              .withForceScalaVersion(versionCombo.asScala.nonEmpty)
              .withScalaVersionOpt(versionCombo.asScala.map(_.scalaVersion.scalaVersion))
          )
          .withMainArtifacts(true)
          .addClassifiers(newClassifiers: _*)
          .eitherResult() match {
          case Left(x: ResolutionError.CantDownloadModule) if remainingAttempts > 0 && x.perRepositoryErrors.exists(_.contains("concurrent download")) =>
            go(remainingAttempts - 1)
          case Left(x: FetchError.DownloadingArtifacts) if remainingAttempts > 0 && x.errors.exists { case (_, artifactError) =>
                artifactError.isInstanceOf[ArtifactError.Recoverable]
              } =>
            go(remainingAttempts - 1)
          case other => other
        }
      }

      go(remainingAttempts = 3).map(res => CoursierResolver.Result(res.fullDetailedArtifacts, res.fullExtraArtifacts))
    }
  }

  private class Cached(logger: Logger, underlying: Direct, in: Path) extends CoursierResolver {
    override val params = underlying.params
    override def resolve(deps: Set[model.Dep], versionCombo: model.VersionCombo): Either[CoursierError, CoursierResolver.Result] =
      if (deps.exists(_.version.endsWith("-SNAPSHOT"))) underlying.resolve(deps, versionCombo)
      else {
        val request = Cached.Request(underlying.fileCache.location, deps.toList.sortBy(_.toString()), underlying.params, versionCombo)
        val digest = request.asJson.noSpaces.hashCode // both `noSpaces` and `String.hashCode` should hopefully be stable
        val cachePath = in / s"$digest.json"

        val cachedResult: Option[Result] =
          if (Files.exists(cachePath)) {
            parser.decode[Cached.Both](Files.readString(cachePath)) match {
              // collision detection is done here: handle it by just overwriting the file
              case Right(Cached.Both(`request`, result)) if result.files.forall(_.exists()) =>
                Some(result)
              case _ =>
                Files.delete(cachePath)
                None
            }
          } else None

        cachedResult match {
          case Some(value) => Right(value)
          case None =>
            val depNames = deps.map(_.baseModuleName.value)
            val ctxLogger = logger.withContext(cachePath).withContext(depNames)
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

  private object Cached {
    case class Request(cacheLocation: File, wanted: List[model.Dep], params: Params, versionCombo: model.VersionCombo)

    object Request {

      implicit val codec: Codec[Request] =
        Codec.forProduct4[Request, File, List[model.Dep], Params, model.VersionCombo]("cacheLocation", "wanted", "params", "forceScalaVersion")(Request.apply)(
          x => (x.cacheLocation, x.wanted, x.params, x.versionCombo)
        )
    }

    case class Both(request: Request, result: Result)
    object Both {
      implicit val codec: Codec[Both] = Codec.forProduct2[Both, Request, Result]("request", "result")(Both.apply)(x => (x.request, x.result))
    }
  }

  class TemplatedVersions(outer: CoursierResolver, maybeWantedBleepVersion: Option[model.BleepVersion]) extends CoursierResolver {
    override val params = outer.params
    override def resolve(deps: Set[model.Dep], versionCombo: model.VersionCombo): Either[CoursierError, Result] = {
      val replacements = model.Replacements.versions(maybeWantedBleepVersion, versionCombo, includeEpoch = true, includeBinVersion = true)
      val rewrittenDeps = deps.map(replacements.fill.dep)
      outer.resolve(rewrittenDeps, versionCombo)
    }
  }
}
