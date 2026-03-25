package bleep.bsp.protocol

import io.circe._

/** Status of an individual test result.
  *
  * Shared across the BSP boundary so both server (bleep-bsp) and client (bleep-core) use the same typed representation instead of stringly-typed status
  * strings.
  */
sealed trait TestStatus {
  def isFailure: Boolean

  /** Wire format string for JSON serialization (backward compatible with existing protocol) */
  def wireValue: String
}

object TestStatus {
  case object Passed extends TestStatus {
    val isFailure: Boolean = false
    val wireValue: String = "passed"
  }
  case object Failed extends TestStatus {
    val isFailure: Boolean = true
    val wireValue: String = "failed"
  }
  case object Error extends TestStatus {
    val isFailure: Boolean = true
    val wireValue: String = "error"
  }
  case object Skipped extends TestStatus {
    val isFailure: Boolean = false
    val wireValue: String = "skipped"
  }
  case object Ignored extends TestStatus {
    val isFailure: Boolean = false
    val wireValue: String = "ignored"
  }
  case object Cancelled extends TestStatus {
    val isFailure: Boolean = true
    val wireValue: String = "cancelled"
  }
  case object AssumptionFailed extends TestStatus {
    val isFailure: Boolean = false
    val wireValue: String = "assumption-failed"
  }
  case object Pending extends TestStatus {
    val isFailure: Boolean = false
    val wireValue: String = "pending"
  }
  case object Timeout extends TestStatus {
    val isFailure: Boolean = true
    val wireValue: String = "timeout"
  }

  def fromString(s: String): TestStatus = s.toLowerCase match {
    case "passed" | "success" => Passed
    case "failed" | "failure" => Failed
    case "error"              => Error
    case "skipped"            => Skipped
    case "ignored"            => Ignored
    case "cancelled"          => Cancelled
    case "assumption-failed"  => AssumptionFailed
    case "pending"            => Pending
    case "timeout"            => Timeout
    case other                => throw new IllegalArgumentException(s"Unknown TestStatus: '$other'")
  }

  implicit val encoder: Encoder[TestStatus] = Encoder.encodeString.contramap(_.wireValue)
  implicit val decoder: Decoder[TestStatus] = Decoder.decodeString.map(fromString)
  implicit val codec: Codec[TestStatus] = Codec.from(decoder, encoder)
}
