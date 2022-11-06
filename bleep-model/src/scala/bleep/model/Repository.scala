package bleep.model

import io.circe.syntax.EncoderOps
import io.circe.{Decoder, DecodingFailure, Encoder, Json}

import java.net.URI
import java.nio.file.Path

sealed trait Repository

object Repository {
  case class Maven(name: Option[String], uri: URI) extends Repository

  case class Folder(name: Option[String], path: Path) extends Repository

  case class Ivy(name: Option[String], uri: URI) extends Repository

  implicit val repoEncoder: Encoder[Repository] =
    Encoder.instance {
      case Repository.Maven(None, uri)         => uri.asJson
      case Repository.Maven(Some(name), uri)   => Json.obj("type" -> "maven".asJson, "uri" -> uri.asJson, "name" -> name.asJson)
      case Repository.Ivy(None, uri)           => Json.obj("type" -> "ivy".asJson, "uri" -> uri.asJson)
      case Repository.Ivy(Some(name), uri)     => Json.obj("type" -> "ivy".asJson, "uri" -> uri.asJson, "name" -> name.asJson)
      case Repository.Folder(None, path)       => Json.obj("type" -> "path".asJson, "path" -> Json.fromString(path.toString))
      case Repository.Folder(Some(name), path) => Json.obj("type" -> "path".asJson, "path" -> Json.fromString(path.toString), "name" -> name.asJson)
    }

  implicit val repoDecoder: Decoder[Repository] = {
    val simple: Decoder[Repository] =
      Decoder[URI].map(uri => Repository.Maven(None, uri))

    val full: Decoder[Repository] =
      Decoder.instance { c =>
        for {
          name <- c.downField("name").as[Option[String]]
          tpe <- c.downField("type").as[Option[String]]
          res <- tpe match {
            case Some("ivy")   => c.downField("uri").as[URI].map(uri => Ivy(name, uri))
            case Some("maven") => c.downField("uri").as[URI].map(uri => Maven(name, uri))
            case Some("path")  => c.downField("path").as[String].map(path => Folder(name, Path.of(path)))
            case _             => Left(DecodingFailure("expected 'type'", c.history))
          }
        } yield res
      }

    simple.or(full)
  }
}
