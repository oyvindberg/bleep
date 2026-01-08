package bleep.model

import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.{Decoder, Encoder}

case class AnnotationProcessor(
    className: String
    // Future fields can be added here:
    // parameters: JsonMap[String] = JsonMap.empty  // For -Akey=value parameters
    // outputs: Option[String] = None               // For custom output directories
)

object AnnotationProcessor {
  implicit val decodes: Decoder[AnnotationProcessor] = deriveDecoder
  implicit val encodes: Encoder[AnnotationProcessor] = deriveEncoder

  implicit val ordering: Ordering[AnnotationProcessor] = Ordering.by(_.className)
}
