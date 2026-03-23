package bleep.bsp.protocol

import io.circe._

/** Why compilation is being triggered — replaces stringly-typed "clean-build"/"incremental"/etc. */
sealed trait CompileReason {
  def wireValue: String
}

object CompileReason {
  case object CleanBuild extends CompileReason { val wireValue: String = "clean-build" }
  case object EmptyOutput extends CompileReason { val wireValue: String = "empty-output" }
  case object Incremental extends CompileReason { val wireValue: String = "incremental" }
  case object UpToDate extends CompileReason { val wireValue: String = "up-to-date" }

  def fromString(s: String): CompileReason = s match {
    case "clean-build"  => CleanBuild
    case "empty-output" => EmptyOutput
    case "incremental"  => Incremental
    case "up-to-date"   => UpToDate
    case other          => throw new IllegalArgumentException(s"Unknown CompileReason: $other")
  }

  implicit val encoder: Encoder[CompileReason] = Encoder.encodeString.contramap(_.wireValue)
  implicit val decoder: Decoder[CompileReason] = Decoder.decodeString.emap { s =>
    scala.util.Try(fromString(s)).toEither.left.map(_.getMessage)
  }
  implicit val codec: Codec[CompileReason] = Codec.from(decoder, encoder)
}
