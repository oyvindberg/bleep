package bleep.model

import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.{Decoder, Encoder}

case class Java(options: Options, annotationProcessing: Option[AnnotationProcessing] = None) extends SetLike[Java] {
  override def intersect(other: Java): Java =
    Java(
      options = options.intersect(other.options),
      annotationProcessing = (annotationProcessing, other.annotationProcessing) match {
        case (Some(a), Some(b)) => Some(AnnotationProcessing(a.enabled && b.enabled))
        case _                  => None
      }
    )

  override def removeAll(other: Java): Java =
    Java(
      options = options.removeAll(other.options),
      annotationProcessing = (annotationProcessing, other.annotationProcessing) match {
        case (Some(a), Some(b)) if a == b => None
        case _                            => annotationProcessing
      }
    )

  override def union(other: Java): Java =
    Java(
      options = options.union(other.options),
      annotationProcessing = other.annotationProcessing.orElse(annotationProcessing)
    )

  def isEmpty: Boolean =
    this match {
      case Java(options, annotationProcessing) => options.isEmpty && annotationProcessing.isEmpty
    }
}

object Java {
  implicit val decodes: Decoder[Java] = deriveDecoder
  implicit val encodes: Encoder[Java] = deriveEncoder
}
