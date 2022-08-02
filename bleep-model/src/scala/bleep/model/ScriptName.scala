package bleep.model

import io.circe.{Decoder, Encoder, KeyDecoder, KeyEncoder}

case class ScriptName(value: String) extends AnyVal

object ScriptName {
  implicit val decodes: Decoder[ScriptName] = Decoder[String].map(ScriptName.apply)
  implicit val encodes: Encoder[ScriptName] = Encoder[String].contramap(_.value)
  implicit val keyDecodes: KeyDecoder[ScriptName] = KeyDecoder[String].map(ScriptName.apply)
  implicit val keyEncodes: KeyEncoder[ScriptName] = KeyEncoder[String].contramap(_.value)
}
