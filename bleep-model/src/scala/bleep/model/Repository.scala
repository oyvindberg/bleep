package bleep.model

import io.circe.syntax.EncoderOps
import io.circe.{Decoder, DecodingFailure, Encoder, Json}

import java.net.URI
import java.nio.file.Path

sealed trait Repository {
  def name: Option[ResolverName]
}

object Repository {
  case class Maven(name: Option[ResolverName], uri: URI) extends Repository

  case class MavenFolder(name: Option[ResolverName], path: Path) extends Repository

  case class Ivy(name: Option[ResolverName], uri: URI) extends Repository

  implicit val repoEncoder: Encoder[Repository] =
    Encoder.instance {
      case Repository.Maven(None, uri)              => uri.asJson
      case Repository.Maven(Some(name), uri)        => Json.obj("type" -> "maven".asJson, "uri" -> uri.asJson, "name" -> name.value.asJson)
      case Repository.Ivy(None, uri)                => Json.obj("type" -> "ivy".asJson, "uri" -> uri.asJson)
      case Repository.Ivy(Some(name), uri)          => Json.obj("type" -> "ivy".asJson, "uri" -> uri.asJson, "name" -> name.value.asJson)
      case Repository.MavenFolder(None, path)       => Json.obj("type" -> "maven-folder".asJson, "path" -> Json.fromString(path.toString))
      case Repository.MavenFolder(Some(name), path) =>
        Json.obj("type" -> "maven-folder".asJson, "path" -> Json.fromString(path.toString), "name" -> name.value.asJson)
    }

  implicit val repoDecoder: Decoder[Repository] = {
    val simple: Decoder[Repository] =
      Decoder[URI].map(uri => Repository.Maven(None, uri))

    val full: Decoder[Repository] =
      Decoder.instance { c =>
        for {
          name <- c.downField("name").as[Option[String]].map(_.map(ResolverName.apply))
          tpe <- c.downField("type").as[Option[String]]
          res <- tpe match {
            case Some("ivy")          => c.downField("uri").as[URI].map(uri => Ivy(name, uri))
            case Some("maven")        => c.downField("uri").as[URI].map(uri => Maven(name, uri))
            case Some("maven-folder") => c.downField("path").as[String].map(path => MavenFolder(name, Path.of(path)))
            case _                    => Left(DecodingFailure("expected 'type'", c.history))
          }
        } yield res
      }

    simple.or(full)
  }
}
