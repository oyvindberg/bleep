package bleep.bsp.protocol

import io.circe._

/** Output stream channel — replaces `isError: Boolean` with semantic meaning.
  *
  * Makes it impossible to confuse stdout/stderr at the type level.
  */
sealed trait OutputChannel {
  def wireValue: String
  def isStderr: Boolean
}

object OutputChannel {
  case object Stdout extends OutputChannel {
    val wireValue: String = "stdout"
    val isStderr: Boolean = false
  }
  case object Stderr extends OutputChannel {
    val wireValue: String = "stderr"
    val isStderr: Boolean = true
  }

  def fromIsError(isError: Boolean): OutputChannel =
    if (isError) Stderr else Stdout

  implicit val encoder: Encoder[OutputChannel] = Encoder.encodeString.contramap(_.wireValue)
  implicit val decoder: Decoder[OutputChannel] = Decoder.decodeString.emap {
    case "stdout" => Right(Stdout)
    case "stderr" => Right(Stderr)
    case other    => Left(s"Unknown OutputChannel: $other")
  }
  implicit val codec: Codec[OutputChannel] = Codec.from(decoder, encoder)
}
