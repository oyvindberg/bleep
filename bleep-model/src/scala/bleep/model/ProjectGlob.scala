package bleep.model

import io.circe.{Codec, Decoder, Encoder}

case class ProjectGlob(value: String) extends AnyVal

object ProjectGlob {
  implicit val codec: Codec[ProjectGlob] = Codec.from(Decoder[String].map(apply), Encoder[String].contramap(_.value))
  implicit val ordering: Ordering[ProjectGlob] = Ordering[String].on(_.value)
}
