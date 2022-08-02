package bleep.model

import io.circe.{Decoder, Encoder}

case class Version(value: String) extends AnyVal

object Version {
  val dev = Version("dev")
  implicit val ordering: Ordering[Version] = Ordering.by(_.value)
  implicit val encodes: Encoder[Version] = Encoder[String].contramap(_.value)
  implicit val decodes: Decoder[Version] = Decoder[String].map(Version.apply)
}
