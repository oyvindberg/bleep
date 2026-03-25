package bleep.bsp.protocol

import io.circe._

/** Project lifecycle state — replaces stringly-typed state strings in ProjectStateChanged events. */
sealed trait ProjectState {
  def wireValue: String
}

object ProjectState {
  case object Pending extends ProjectState { val wireValue: String = "pending" }
  case object Compiling extends ProjectState { val wireValue: String = "compiling" }
  case object Compiled extends ProjectState { val wireValue: String = "compiled" }
  case object CompileFailed extends ProjectState { val wireValue: String = "compile_failed" }
  case object Linking extends ProjectState { val wireValue: String = "linking" }
  case object Linked extends ProjectState { val wireValue: String = "linked" }
  case object Discovering extends ProjectState { val wireValue: String = "discovering" }
  case object Testing extends ProjectState { val wireValue: String = "testing" }
  case object Completed extends ProjectState { val wireValue: String = "completed" }
  case object Skipped extends ProjectState { val wireValue: String = "skipped" }

  def fromString(s: String): ProjectState = s match {
    case "pending"        => Pending
    case "compiling"      => Compiling
    case "compiled"       => Compiled
    case "compile_failed" => CompileFailed
    case "linking"        => Linking
    case "linked"         => Linked
    case "discovering"    => Discovering
    case "testing"        => Testing
    case "completed"      => Completed
    case "skipped"        => Skipped
    case other            => throw new IllegalArgumentException(s"Unknown ProjectState: $other")
  }

  implicit val encoder: Encoder[ProjectState] = Encoder.encodeString.contramap(_.wireValue)
  implicit val decoder: Decoder[ProjectState] = Decoder.decodeString.emap { s =>
    scala.util.Try(fromString(s)).toEither.left.map(_.getMessage)
  }
  implicit val codec: Codec[ProjectState] = Codec.from(decoder, encoder)
}
