package bleep.bsp.protocol

import io.circe._

/** Compilation sub-phase.
  *
  * Replaces stringly-typed "reading-analysis"/"analyzing"/"compiling"/"saving-analysis" in CompilePhaseChanged events.
  */
sealed trait CompilePhase {
  def wireValue: String
}

object CompilePhase {
  case object ReadingAnalysis extends CompilePhase {
    val wireValue: String = "reading-analysis"
  }
  case object Analyzing extends CompilePhase {
    val wireValue: String = "analyzing"
  }
  case object Compiling extends CompilePhase {
    val wireValue: String = "compiling"
  }
  case object SavingAnalysis extends CompilePhase {
    val wireValue: String = "saving-analysis"
  }

  def fromString(s: String): CompilePhase = s.toLowerCase match {
    case "reading-analysis" => ReadingAnalysis
    case "analyzing"        => Analyzing
    case "compiling"        => Compiling
    case "saving-analysis"  => SavingAnalysis
    case _                  => Compiling
  }

  implicit val encoder: Encoder[CompilePhase] = Encoder.encodeString.contramap(_.wireValue)
  implicit val decoder: Decoder[CompilePhase] = Decoder.decodeString.map(fromString)
  implicit val codec: Codec[CompilePhase] = Codec.from(decoder, encoder)
}
