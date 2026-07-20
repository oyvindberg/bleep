package bleep.bsp.protocol

import io.circe._

/** Status of a compilation result.
  *
  * Replaces stringly-typed "success"/"failed"/"error"/"skipped"/"cancelled" in CompileFinished events.
  */
sealed trait CompileStatus {
  def wireValue: String
  def isSuccess: Boolean

  /** The project did not compile and that is a problem to report: `Failed` (compiler diagnostics) or `Error` (the compile task threw). Deliberately NOT the
    * negation of [[isSuccess]] — `Skipped` and `Cancelled` are also not-success but are not failures, so counting `!isSuccess` would over-report. Every "did
    * this compile go wrong" check must use this rather than testing for `Failed` alone, which silently drops `Error`.
    */
  def isFailure: Boolean
}

object CompileStatus {
  case object Success extends CompileStatus {
    val wireValue: String = "success"
    val isSuccess: Boolean = true
    val isFailure: Boolean = false
  }
  case object Failed extends CompileStatus {
    val wireValue: String = "failed"
    val isSuccess: Boolean = false
    val isFailure: Boolean = true
  }
  case object Error extends CompileStatus {
    val wireValue: String = "error"
    val isSuccess: Boolean = false
    val isFailure: Boolean = true
  }
  case object Skipped extends CompileStatus {
    val wireValue: String = "skipped"
    val isSuccess: Boolean = false
    val isFailure: Boolean = false
  }
  case object Cancelled extends CompileStatus {
    val wireValue: String = "cancelled"
    val isSuccess: Boolean = false
    val isFailure: Boolean = false
  }

  def fromString(s: String): CompileStatus = s.toLowerCase match {
    case "success"   => Success
    case "failed"    => Failed
    case "error"     => Error
    case "skipped"   => Skipped
    case "cancelled" => Cancelled
    case _           => Error
  }

  implicit val encoder: Encoder[CompileStatus] = Encoder.encodeString.contramap(_.wireValue)
  implicit val decoder: Decoder[CompileStatus] = Decoder.decodeString.map(fromString)
  implicit val codec: Codec[CompileStatus] = Codec.from(decoder, encoder)
}
