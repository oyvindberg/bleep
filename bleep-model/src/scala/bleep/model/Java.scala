package bleep.model

import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.{Decoder, Encoder}

/** Java compilation settings.
  *
  * @param options
  *   javac options
  * @param annotationProcessing
  *   annotation processing configuration
  * @param ecjVersion
  *   optional ECJ (Eclipse Compiler for Java) version. If set, uses ECJ instead of javac.
  */
case class Java(
    options: Options,
    annotationProcessing: Option[AnnotationProcessing] = None,
    ecjVersion: Option[VersionEcj] = None
) extends SetLike[Java] {
  override def intersect(other: Java): Java =
    Java(
      options = options.intersect(other.options),
      annotationProcessing = (annotationProcessing, other.annotationProcessing) match {
        case (Some(a), Some(b)) => Some(AnnotationProcessing(a.enabled && b.enabled))
        case _                  => None
      },
      ecjVersion = if (ecjVersion == other.ecjVersion) ecjVersion else None
    )

  override def removeAll(other: Java): Java =
    Java(
      options = options.removeAll(other.options),
      annotationProcessing = (annotationProcessing, other.annotationProcessing) match {
        case (Some(a), Some(b)) if a == b => None
        case _                            => annotationProcessing
      },
      ecjVersion = if (ecjVersion == other.ecjVersion) None else ecjVersion
    )

  override def union(other: Java): Java =
    Java(
      options = options.union(other.options),
      annotationProcessing = other.annotationProcessing.orElse(annotationProcessing),
      ecjVersion = ecjVersion.orElse(other.ecjVersion)
    )

  def isEmpty: Boolean =
    this match {
      case Java(options, annotationProcessing, ecjVersion) =>
        options.isEmpty && annotationProcessing.isEmpty && ecjVersion.isEmpty
    }
}

object Java {
  implicit val decodes: Decoder[Java] = deriveDecoder
  implicit val encodes: Encoder[Java] = deriveEncoder
}
