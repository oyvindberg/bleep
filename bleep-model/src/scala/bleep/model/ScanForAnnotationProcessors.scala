package bleep.model

import io.circe.{Decoder, DecodingFailure, Encoder, Json}

/** Whether bleep looks through a project's dependencies for annotation processors, as javac does when it is given no processor path.
  *
  *   - `true`: scan, and fail when there is none. Written by hand, it is there for a processor
  *   - `if-present`: scan, and run what is there, which may be nothing. What a build tool which runs javac's default does, maven say, so imports use it
  *   - `false`: do not scan
  */
sealed abstract class ScanForAnnotationProcessors(val scans: Boolean)

object ScanForAnnotationProcessors {
  case object Yes extends ScanForAnnotationProcessors(scans = true)
  case object IfPresent extends ScanForAnnotationProcessors(scans = true)
  case object No extends ScanForAnnotationProcessors(scans = false)

  val IfPresentValue = "if-present"

  implicit val encodes: Encoder[ScanForAnnotationProcessors] = Encoder.instance {
    case Yes       => Json.True
    case No        => Json.False
    case IfPresent => Json.fromString(IfPresentValue)
  }

  implicit val decodes: Decoder[ScanForAnnotationProcessors] = Decoder.instance { c =>
    c.value.asBoolean match {
      case Some(true)  => Right(Yes)
      case Some(false) => Right(No)
      case None        =>
        c.value.asString match {
          case Some(IfPresentValue) => Right(IfPresent)
          case _                    => Left(DecodingFailure(s"expected true, false or $IfPresentValue", c.history))
        }
    }
  }
}
