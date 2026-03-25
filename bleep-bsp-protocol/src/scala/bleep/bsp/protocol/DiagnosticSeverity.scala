package bleep.bsp.protocol

import io.circe.{Codec, Decoder, Encoder}

/** Severity level for compiler diagnostics. */
sealed trait DiagnosticSeverity {
  def wireValue: String
  def isError: Boolean
}

object DiagnosticSeverity {
  case object Error extends DiagnosticSeverity {
    val wireValue = "error"
    val isError = true
  }
  case object Warning extends DiagnosticSeverity {
    val wireValue = "warning"
    val isError = false
  }
  case object Info extends DiagnosticSeverity {
    val wireValue = "info"
    val isError = false
  }

  def fromString(s: String): DiagnosticSeverity =
    s.toLowerCase match {
      case "error"   => Error
      case "warning" => Warning
      case "info"    => Info
      case other     => throw new IllegalArgumentException(s"Unknown diagnostic severity: $other")
    }

  implicit val encoder: Encoder[DiagnosticSeverity] = Encoder.encodeString.contramap(_.wireValue)
  implicit val decoder: Decoder[DiagnosticSeverity] = Decoder.decodeString.map(fromString)
  implicit val codec: Codec[DiagnosticSeverity] = Codec.from(decoder, encoder)
}
