package bleep.model

import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.{Decoder, Encoder}

case class AnnotationProcessing(enabled: Boolean = false)

object AnnotationProcessing {
  implicit val decodes: Decoder[AnnotationProcessing] = deriveDecoder
  implicit val encodes: Encoder[AnnotationProcessing] = deriveEncoder
}
