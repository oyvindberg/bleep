package bleep.testing

import io.circe._
import io.circe.generic.semiauto._
import io.circe.syntax._

/** Protocol messages for communication between bleep and forked test JVMs.
  *
  * Uses simple JSON-over-stdin/stdout for portability and debuggability.
  */
object TestProtocol {

  // === Commands (parent -> forked JVM) ===

  /** Commands sent from bleep to the forked test runner */
  sealed trait TestCommand

  object TestCommand {

    /** Run a test suite */
    case class RunSuite(
        className: String,
        framework: String,
        args: List[String]
    ) extends TestCommand

    /** Gracefully shut down the forked JVM */
    case object Shutdown extends TestCommand

    /** Get a thread dump from the forked JVM */
    case object GetThreadDump extends TestCommand

    implicit val runSuiteEncoder: Encoder[RunSuite] = deriveEncoder
    implicit val runSuiteDecoder: Decoder[RunSuite] = deriveDecoder

    implicit val encoder: Encoder[TestCommand] = Encoder.instance {
      case rs: RunSuite  => Json.obj("type" -> "RunSuite".asJson, "data" -> rs.asJson)
      case Shutdown      => Json.obj("type" -> "Shutdown".asJson)
      case GetThreadDump => Json.obj("type" -> "GetThreadDump".asJson)
    }

    implicit val decoder: Decoder[TestCommand] = Decoder.instance { cursor =>
      cursor.downField("type").as[String].flatMap {
        case "RunSuite"      => cursor.downField("data").as[RunSuite]
        case "Shutdown"      => Right(Shutdown)
        case "GetThreadDump" => Right(GetThreadDump)
        case other           => Left(DecodingFailure(s"Unknown command type: $other", cursor.history))
      }
    }
  }

  // === Responses (forked JVM -> parent) ===

  /** Responses sent from the forked test runner back to bleep */
  sealed trait TestResponse

  object TestResponse {

    /** Test runner is ready to receive commands */
    case object Ready extends TestResponse

    /** A test has started */
    case class TestStarted(
        suite: String,
        test: String
    ) extends TestResponse

    /** A test has finished */
    case class TestFinished(
        suite: String,
        test: String,
        status: String, // passed, failed, error, skipped, ignored, cancelled, pending
        durationMs: Long,
        message: Option[String],
        throwable: Option[String]
    ) extends TestResponse

    /** A test suite has completed */
    case class SuiteDone(
        suite: String,
        passed: Int,
        failed: Int,
        skipped: Int,
        ignored: Int,
        durationMs: Long
    ) extends TestResponse

    /** Log output from test */
    case class Log(
        level: String,
        message: String
    ) extends TestResponse

    /** An error occurred in the test runner itself */
    case class Error(
        message: String,
        throwable: Option[String]
    ) extends TestResponse

    /** Thread dump from the forked JVM */
    case class ThreadDump(
        threads: List[ThreadInfo]
    ) extends TestResponse

    /** Information about a single thread */
    case class ThreadInfo(
        name: String,
        state: String,
        stackTrace: List[String]
    )

    implicit val threadInfoEncoder: Encoder[ThreadInfo] = deriveEncoder
    implicit val threadInfoDecoder: Decoder[ThreadInfo] = deriveDecoder

    implicit val threadDumpEncoder: Encoder[ThreadDump] = deriveEncoder
    implicit val threadDumpDecoder: Decoder[ThreadDump] = deriveDecoder

    implicit val testStartedEncoder: Encoder[TestStarted] = deriveEncoder
    implicit val testStartedDecoder: Decoder[TestStarted] = deriveDecoder

    implicit val testFinishedEncoder: Encoder[TestFinished] = deriveEncoder
    implicit val testFinishedDecoder: Decoder[TestFinished] = deriveDecoder

    implicit val suiteDoneEncoder: Encoder[SuiteDone] = deriveEncoder
    implicit val suiteDoneDecoder: Decoder[SuiteDone] = deriveDecoder

    implicit val logEncoder: Encoder[Log] = deriveEncoder
    implicit val logDecoder: Decoder[Log] = deriveDecoder

    implicit val errorEncoder: Encoder[Error] = deriveEncoder
    implicit val errorDecoder: Decoder[Error] = deriveDecoder

    implicit val encoder: Encoder[TestResponse] = Encoder.instance {
      case Ready            => Json.obj("type" -> "Ready".asJson)
      case ts: TestStarted  => Json.obj("type" -> "TestStarted".asJson, "data" -> ts.asJson)
      case tf: TestFinished => Json.obj("type" -> "TestFinished".asJson, "data" -> tf.asJson)
      case sd: SuiteDone    => Json.obj("type" -> "SuiteDone".asJson, "data" -> sd.asJson)
      case l: Log           => Json.obj("type" -> "Log".asJson, "data" -> l.asJson)
      case e: Error         => Json.obj("type" -> "Error".asJson, "data" -> e.asJson)
      case td: ThreadDump   => Json.obj("type" -> "ThreadDump".asJson, "data" -> td.asJson)
    }

    implicit val decoder: Decoder[TestResponse] = Decoder.instance { cursor =>
      cursor.downField("type").as[String].flatMap {
        case "Ready"        => Right(Ready)
        case "TestStarted"  => cursor.downField("data").as[TestStarted]
        case "TestFinished" => cursor.downField("data").as[TestFinished]
        case "SuiteDone"    => cursor.downField("data").as[SuiteDone]
        case "Log"          => cursor.downField("data").as[Log]
        case "Error"        => cursor.downField("data").as[Error]
        case "ThreadDump"   => cursor.downField("data").as[ThreadDump]
        case other          => Left(DecodingFailure(s"Unknown response type: $other", cursor.history))
      }
    }
  }

  // === Encoding/Decoding utilities ===

  /** Encode a command to a single line of JSON */
  def encodeCommand(cmd: TestCommand): String =
    cmd.asJson.noSpaces

  /** Decode a command from a JSON line */
  def decodeCommand(line: String): Either[io.circe.Error, TestCommand] =
    io.circe.parser.decode[TestCommand](line)

  /** Encode a response to a single line of JSON */
  def encodeResponse(resp: TestResponse): String =
    resp.asJson.noSpaces

  /** Decode a response from a JSON line */
  def decodeResponse(line: String): Either[io.circe.Error, TestResponse] =
    io.circe.parser.decode[TestResponse](line)
}
