package bleep.model

import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.{Decoder, Encoder}

case class Java(
    options: Options,
    annotationProcessors: JsonSet[AnnotationProcessor] = JsonSet.empty
) extends SetLike[Java] {
  override def intersect(other: Java): Java =
    Java(
      options = options.intersect(other.options),
      annotationProcessors = annotationProcessors.intersect(other.annotationProcessors)
    )

  override def removeAll(other: Java): Java =
    Java(
      options = options.removeAll(other.options),
      annotationProcessors = annotationProcessors.removeAll(other.annotationProcessors)
    )

  override def union(other: Java): Java =
    Java(
      options = options.union(other.options),
      annotationProcessors = annotationProcessors.union(other.annotationProcessors)
    )

  def isEmpty: Boolean =
    this match {
      case Java(options, annotationProcessors) => options.isEmpty && annotationProcessors.isEmpty
    }
}

object Java {
  implicit val decodes: Decoder[Java] = deriveDecoder
  implicit val encodes: Encoder[Java] = deriveEncoder
}
