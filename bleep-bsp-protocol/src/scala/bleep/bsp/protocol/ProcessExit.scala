package bleep.bsp.protocol

import io.circe._

/** How a process terminated — mutually exclusive exit code vs signal.
  *
  * Replaces the impossible state of `(exitCode: Option[Int], signal: Option[Int])` where both could be Some or both None.
  */
sealed trait ProcessExit
object ProcessExit {

  /** Process exited normally with given exit code */
  case class ExitCode(code: Int) extends ProcessExit

  /** Process was killed by a signal (Unix). Signal number is the raw signal, not 128+signal. */
  case class Signal(signal: Int) extends ProcessExit

  /** Process exit information not available (e.g., exception caught before process ran) */
  case object Unknown extends ProcessExit

  implicit val encoder: Encoder[ProcessExit] = Encoder.instance {
    case ExitCode(code) => Json.obj("type" -> Json.fromString("ExitCode"), "code" -> Json.fromInt(code))
    case Signal(signal) => Json.obj("type" -> Json.fromString("Signal"), "signal" -> Json.fromInt(signal))
    case Unknown        => Json.obj("type" -> Json.fromString("Unknown"))
  }

  implicit val decoder: Decoder[ProcessExit] = Decoder.instance { cursor =>
    cursor.downField("type").as[String].flatMap {
      case "ExitCode" => cursor.downField("code").as[Int].map(ExitCode.apply)
      case "Signal"   => cursor.downField("signal").as[Int].map(Signal.apply)
      case "Unknown"  => Right(Unknown)
      case other      => Left(DecodingFailure(s"Unknown ProcessExit type: $other", cursor.history))
    }
  }

  implicit val codec: Codec[ProcessExit] = Codec.from(decoder, encoder)
}
