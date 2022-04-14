package bleep

import bleep.internal.FileUtils
import bleep.internal.codecs._
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
import java.net.URI
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

trait CoursierResolver {
  def apply(deps: JsonSet[Dependency], forceScalaVersion: Option[bleep.Versions.Scala]): Either[CoursierError, CoursierResolver.Result]
}

object CoursierResolver {
  def apply(repos: List[model.Repository], logger: Logger, downloadSources: Boolean, directories: UserPaths): CoursierResolver = CoursierResolver(
    repos,
    logger,
    downloadSources,
    Some(directories.cacheDir),
    CoursierResolver.Authentications.fromFile(directories.coursierRepositoriesJson, logger)
  )

  def apply(
      repos: List[model.Repository],
      logger: Logger,
      downloadSources: Boolean,
      cacheIn: Option[Path],
      authentications: Authentications
  ): CoursierResolver = {
    val direct = new Direct(repos, downloadSources, authentications)
    cacheIn match {
      case Some(cacheIn) => new Cached(logger, direct, cacheIn)
      case None          => direct
    }
  }

  final case class Authentications(configs: Map[URI, Authentication])
  object Authentications {
    val empty: Authentications = Authentications(Map.empty)
    private type Elem = Map[URI, Authentication]

    implicit val authenticationCodec: Codec[Authentication] =
      Codec.forProduct7[Authentication, Option[String], Option[String], Option[Map[String, String]], Option[Boolean], Option[String], Option[Boolean], Option[
        Boolean
      ]](
        "user",
        "password",
        "headers",
        "optional",
        "realm",
        "httpsOnly",
        "passOnRedirect"
      )((user, pass, headers, optional, realm, httpsOnly, redirect) =>
        Authentication(
          user.getOrElse(""),
          pass,
          headers.map(_.toList).getOrElse(Nil),
          optional.getOrElse(false),
          realm,
          httpsOnly.getOrElse(true),
          redirect.getOrElse(true)
        )
      )(x => (Some(x.user), x.passwordOpt, Some(x.httpHeaders.toMap), Some(x.optional), x.realmOpt, Some(x.httpsOnly), Some(x.passOnRedirect)))

    implicit val keyEncoder: KeyEncoder[URI] = KeyEncoder.encodeKeyString.contramap(_.toString)
    implicit val keyDecoder: KeyDecoder[URI] = KeyDecoder.decodeKeyString.map(URI.create)

    implicit val resolverConfigCodec: Codec[Authentications] =
      Codec.from(Decoder[Elem], Encoder[Elem]).iemap(m => Right(Authentications(m)))(_.configs)

    def fromFile(path: Path, logger: Logger): Authentications =
      if (Files.exists(path)) {
        val json = Files.readString(path, StandardCharsets.UTF_8)
        io.circe.parser
          .decode[Authentications](json)
          .fold(
            err => {
              logger.warn(s"Failed to parse resolver authentication from $path, [$err]")
              empty
            },
            identity
          )
      } else {
        logger.info(s"No authentication information found in $path")
        empty
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
      Codec.forProduct7[Dependency, Module, String, Configuration, Set[(Organization, ModuleName)], Publication, Boolean, Boolean]("module", "version", "configuration", "exclusions", "publication", "optional", "transitive")(Dependency.apply)(x => (x.module, x.version, x.configuration, x.exclusions, x.publication, x.optional, x.transitive))

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

  private class Direct(val repos: List[model.Repository], downloadSources: Boolean, resolverConfigs: Authentications) extends CoursierResolver {
    val fileCache = FileCache[Task]()

    override def apply(deps: JsonSet[Dependency], forceScalaVersion: Option[bleep.Versions.Scala]): Either[CoursierError, CoursierResolver.Result] = {
      def go(remainingAttempts: Int): Either[CoursierError, Fetch.Result] = {
        val newClassifiers = if (downloadSources) List(Classifier.sources) else Nil

        val value = Fetch[Task](fileCache)
          .withDependencies(deps.values.toList)
          .withResolutionParams(
            ResolutionParams()
              .withForceScalaVersion(forceScalaVersion.nonEmpty)
              .withScalaVersionOpt(forceScalaVersion.map(_.scalaVersion))
          )
          .addRepositories((repos ++ constants.DefaultRepos).map {
            case bleep.model.Repository.Maven(uri) =>
              MavenRepository(uri.toString).withAuthentication(resolverConfigs.configs.get(uri))
            case bleep.model.Repository.Ivy(uri) =>
              IvyRepository.fromPattern(uri.toString +: coursier.ivy.Pattern.default).withAuthentication(resolverConfigs.configs.get(uri))
          }: _*)
          .withMainArtifacts(true)
          .addClassifiers(newClassifiers: _*)
        value
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
    override def apply(deps: JsonSet[Dependency], forceScalaVersion: Option[bleep.Versions.Scala]): Either[CoursierError, CoursierResolver.Result] =
      if (deps.values.exists(_.version.endsWith("-SNAPSHOT"))) underlying(deps, forceScalaVersion)
      else {
        val request = Cached.Request(underlying.fileCache.location, deps, underlying.repos, forceScalaVersion.map(_.scalaVersion))
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
            val depNames = deps.map(_.module.name.value).values
            val ctxLogger = logger.withContext(cachePath).withContext(depNames)
            ctxLogger.debug(s"coursier cache miss")
            underlying(deps, forceScalaVersion).map {
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
    case class Request(cacheLocation: File, wanted: JsonSet[Dependency], repos: List[model.Repository], forceScalaVersion: Option[String])
    object Request {
      import Result.codecDependency
      implicit val ordering: Ordering[Dependency] =
        Ordering.by(_.toString())

      implicit val codec: Codec[Request] =
        Codec.forProduct4[Request, File, JsonSet[Dependency], List[model.Repository], Option[String]]("", "wanted", "repos", "forceScalaVersion")(
          Request.apply
        )(x => (x.cacheLocation, x.wanted, x.repos, x.forceScalaVersion))
    }

    case class Both(request: Request, result: Result)
    object Both {
      implicit val codec: Codec[Both] = Codec.forProduct2[Both, Request, Result]("request", "result")(Both.apply)(x => (x.request, x.result))
    }
  }
}
