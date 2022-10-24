package bleep.model

import io.circe.{Decoder, Encoder}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}

case class Jvm(name: String, index: Option[String])

object Jvm {
  val graalvm = Jvm("graalvm-java17:22.2.0", None)
  val system = Jvm("system", None)
  implicit val encodes: Encoder[Jvm] = deriveEncoder
  implicit val decodes: Decoder[Jvm] = deriveDecoder
}
