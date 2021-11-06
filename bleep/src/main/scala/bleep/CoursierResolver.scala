package bleep

import bleep.internal.codecs._
import coursier.cache.{ArtifactError, FileCache}
import coursier.core._
import coursier.error.{FetchError, ResolutionError}
import coursier.util.{Artifact, Task}
import coursier.{Fetch, MavenRepository}
import io.circe.syntax.EncoderOps
import io.circe.{parser, Codec, Decoder, Encoder, KeyDecoder, KeyEncoder}

import java.io.File
import java.net.URI
import java.nio.file.{Files, Path}
import scala.concurrent.{ExecutionContext, Future}

trait CoursierResolver {
  def apply(deps: JsonSet[Dependency], repositories: JsonSet[URI]): Future[CoursierResolver.Result]
}

object CoursierResolver {
  def apply(ec: ExecutionContext, downloadSources: Boolean, cacheIn: Option[Path]): CoursierResolver =
    cacheIn.foldLeft(new Direct(ec, downloadSources): CoursierResolver) { case (cr, path) => new Cached(cr, path, ec) }

  // this is a simplified version of the original `Fetch.Result` with a json codec
  case class Result(
      fullDetailedArtifacts: Seq[(Dependency, Publication, Artifact, Option[File])],
      fullExtraArtifacts: Seq[(Artifact, Option[File])]
  ) {
    def detailedArtifacts: Seq[(Dependency, Publication, Artifact, File)] =
      fullDetailedArtifacts.collect { case (dep, pub, art, Some(file)) =>
        (dep, pub, art, file)
      }

    def fullArtifacts: Seq[(Artifact, Option[File])] = {
      val artifacts = fullDetailedArtifacts.map { case (_, _, a, f) => (a, f) } ++ fullExtraArtifacts
      artifacts.distinct
    }
    def artifacts: Seq[(Artifact, File)] =
      fullArtifacts.collect { case (art, Some(file)) => (art, file) }

    def files: Seq[File] =
      artifacts.map(_._2).distinct
  }

  private object Result {
    // format: off
    implicit val codecOrganization: Codec[Organization] = Codec.from(Decoder[String].map(Organization.apply), Encoder[String].contramap(_.value))
    implicit val codecModuleName: Codec[ModuleName] = Codec.from(Decoder[String].map(ModuleName.apply), Encoder[String].contramap(_.value))
    implicit val codecType: Codec[Type] = Codec.from(Decoder[String].map(Type.apply), Encoder[String].contramap(_.value))
    implicit val codecExtension: Codec[Extension] = Codec.from(Decoder[String].map(Extension.apply), Encoder[String].contramap(_.value))
    implicit val codecClassifier: Codec[Classifier] = Codec.from(Decoder[String].map(Classifier.apply), Encoder[String].contramap(_.value))
    implicit val codecConfiguration: Codec[Configuration] = Codec.from(Decoder[String].map(Configuration.apply), Encoder[String].contramap(_.value))

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

  private class Direct(ec: ExecutionContext, downloadSources: Boolean) extends CoursierResolver {
    val fileCache = FileCache[Task]()

    override def apply(deps: JsonSet[Dependency], repositories: JsonSet[URI]): Future[CoursierResolver.Result] = {
      def go(remainingAttempts: Int): Future[Fetch.Result] = {
        val newClassifiers = if (downloadSources) List(Classifier.sources) else Nil

        Fetch[Task](fileCache)
          .withDependencies(deps.values.toList)
          .addRepositories(repositories.values.toList.map(uri => MavenRepository(uri.toString)): _*)
          .withMainArtifacts(true)
          .addClassifiers(newClassifiers: _*)
          .ioResult
          .future()(ec)
          .recoverWith {
            case x: ResolutionError.CantDownloadModule if remainingAttempts > 0 && x.perRepositoryErrors.exists(_.contains("concurrent download")) =>
              go(remainingAttempts - 1)
            case x: FetchError.DownloadingArtifacts if remainingAttempts > 0 && x.errors.exists { case (_, artifactError) =>
                  artifactError.isInstanceOf[ArtifactError.Recoverable]
                } =>
              go(remainingAttempts - 1)
          }(ec)
      }

      go(remainingAttempts = 3).map(res => CoursierResolver.Result(res.fullDetailedArtifacts, res.fullExtraArtifacts))(ec)
    }
  }

  private class Cached(underlying: CoursierResolver, in: Path, ec: ExecutionContext) extends CoursierResolver {
    override def apply(deps: JsonSet[Dependency], repositories: JsonSet[URI]): Future[Result] =
      if (deps.values.exists(_.version.endsWith("-SNAPSHOT"))) underlying(deps, repositories)
      else {
        val request = Cached.Request(deps, repositories)
        val digest = request.asJson.noSpaces.hashCode // both `noSpaces` and `String.hashCode` should hopefully be stable
        val cachePath = in / s"$digest.json"

        val cachedResult: Option[Result] =
          if (Files.exists(cachePath)) {
            parser.decode[Cached.Both](Files.readString(cachePath)) match {
              // collision detection is done here: handle it by just overwriting the file
              case Right(Cached.Both(`request`, result)) if result.files.forall(_.exists()) =>
                println(s"read $cachePath")
                Some(result)
              case _ =>
                println(s"deleted $cachePath")
                Files.delete(cachePath)
                None
            }
          } else None

        cachedResult match {
          case Some(value) => Future.successful(value)
          case None =>
            underlying(deps, repositories).map { result =>
              Files.createDirectories(cachePath.getParent)
              Files.writeString(cachePath, Cached.Both(request, result).asJson.noSpaces)
              println(s"wrote cache $cachePath")
              result
            }(ec)
        }
      }
  }

  private object Cached {
    case class Request(wanted: JsonSet[Dependency], repositories: JsonSet[URI])
    object Request {
      import Result.codecDependency

      implicit val ordering: Ordering[Dependency] =
        Ordering.by(_.toString())

      implicit val codec: Codec[Request] =
        Codec.forProduct2[Request, JsonSet[Dependency], JsonSet[URI]]("wanted", "repositories")(Request.apply)(x => (x.wanted, x.repositories))
    }

    case class Both(request: Request, result: Result)
    object Both {
      implicit val codec: Codec[Both] = Codec.forProduct2[Both, Request, Result]("request", "result")(Both.apply)(x => (x.request, x.result))
    }
  }
}
