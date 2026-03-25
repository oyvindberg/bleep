package bleep.bsp.protocol

import io.circe._

/** Log message severity level — replaces stringly-typed "info"/"warn"/"error"/"debug". */
sealed trait LogLevel {
  def wireValue: String
}

object LogLevel {
  case object Info extends LogLevel { val wireValue: String = "info" }
  case object Warn extends LogLevel { val wireValue: String = "warn" }
  case object Error extends LogLevel { val wireValue: String = "error" }
  case object Debug extends LogLevel { val wireValue: String = "debug" }

  def fromString(s: String): LogLevel = s.toLowerCase match {
    case "info"  => Info
    case "warn"  => Warn
    case "error" => Error
    case "debug" => Debug
    case other   => throw new IllegalArgumentException(s"Unknown LogLevel: $other")
  }

  implicit val encoder: Encoder[LogLevel] = Encoder.encodeString.contramap(_.wireValue)
  implicit val decoder: Decoder[LogLevel] = Decoder.decodeString.emap {
    case "info"  => Right(Info)
    case "warn"  => Right(Warn)
    case "error" => Right(Error)
    case "debug" => Right(Debug)
    case other   => Left(s"Unknown LogLevel: $other")
  }
  implicit val codec: Codec[LogLevel] = Codec.from(decoder, encoder)
}
