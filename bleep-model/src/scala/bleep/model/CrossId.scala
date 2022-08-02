package bleep.model

import io.circe.{Decoder, Encoder, KeyDecoder, KeyEncoder}

case class CrossId(value: String)

object CrossId {
  def defaultFrom(maybeScalaVersion: Option[VersionScala], maybePlatformId: Option[PlatformId], isFull: Boolean): Option[CrossId] = {
    val maybeVersion = maybeScalaVersion.map {
      case scala if isFull => scala.scalaVersion
      case scala           => scala.binVersion.replace(".", "")
    }
    val maybePlatformIdString = maybePlatformId.map(_.value)
    (maybePlatformIdString, maybeVersion) match {
      case (Some(platformId), Some(binVersion)) => Some(CrossId(s"$platformId$binVersion"))
      case (Some(platformId), None)             => Some(CrossId(platformId))
      case (None, Some(binVersion))             => Some(CrossId(binVersion))
      case (None, None)                         => None
    }
  }

  implicit val ordering: Ordering[CrossId] = Ordering.by(_.value)
  implicit val decodes: Decoder[CrossId] = Decoder[String].map(CrossId.apply)
  implicit val encodes: Encoder[CrossId] = Encoder[String].contramap(_.value)
  implicit val keyDecodes: KeyDecoder[CrossId] = KeyDecoder[String].map(CrossId.apply)
  implicit val keyEncodes: KeyEncoder[CrossId] = KeyEncoder[String].contramap(_.value)
}
