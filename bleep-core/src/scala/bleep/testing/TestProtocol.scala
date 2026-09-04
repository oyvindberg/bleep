package bleep.testing

import bleep.bsp.protocol.SuiteOutcome
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

    /** Run a test suite.
      *
      * `selection` carries the execution decision, not just a name — see [[FrameworkSelection]] for why the fork is told rather than asked to work it out.
      */
    case class RunSuite(
        className: String,
        selection: FrameworkSelection,
        args: List[String]
    ) extends TestCommand

    /** Run a whole project's JUnit-Platform suites in ONE launcher execution, at a bleep-chosen degree of parallelism.
      *
      * This is the maven-surefire shape for JUnit: all of a module's classes go through one `launcher.execute()`, so anything scoped to that one execution — an
      * application booted for the run, a shared `LauncherSessionListener` registration — is set up once and reused across every class, instead of rebuilt per
      * class. `parallelism` is bleep's decision, not junit's: 1 serialises (what a singleton-per-JVM application needs), N runs N classes at once. junit's
      * engine is the executor of that number, nothing more. Only the JUnit-Platform runner honours this; sbt test-interface frameworks are driven
      * suite-by-suite on bleep's own threads, where there is no cross-suite execution scope to preserve.
      */
    case class RunSuites(
        classNames: List[String],
        parallelism: Int,
        selection: FrameworkSelection,
        args: List[String]
    ) extends TestCommand

    /** Cancel one in-flight suite by class name without touching the fork or its siblings.
      *
      * Only meaningful when the fork is running several suites at once (a per-project shared session): it interrupts just that suite's thread, so the other
      * suites sharing the JVM keep running. An exclusive fork cancels by having its socket closed instead — there is only the one suite, and the whole process
      * goes. See [[bleep.model.TestJvmMode]].
      */
    case class CancelSuite(className: String) extends TestCommand

    /** Gracefully shut down the forked JVM */
    case object Shutdown extends TestCommand

    /** Get a thread dump from the forked JVM */
    case object GetThreadDump extends TestCommand

    /** The wire strings, read from the Java enum the fork parses with, so there is exactly one definition of them. */
    private object RunnerWire {
      val JUnitPlatform: String = runner.TestProtocol.RunnerKind.JUNIT_PLATFORM.wire()
      val SbtTestInterface: String = runner.TestProtocol.RunnerKind.SBT_TEST_INTERFACE.wire()
    }

    /** Hand-written rather than derived, because the wire form is flat while the model is a sum: `runner` names the mechanism and `frameworkClass` is present
      * exactly when that mechanism needs one. The Java parser in the fork enforces the same pairing, and both sides reject a mismatch rather than defaulting.
      */
    implicit val runSuiteEncoder: Encoder[RunSuite] = Encoder.instance { rs =>
      val (runner, frameworkClass) = rs.selection match {
        case FrameworkSelection.JUnitPlatform(_)         => (RunnerWire.JUnitPlatform, None)
        case FrameworkSelection.SbtTestInterface(_, cls) => (RunnerWire.SbtTestInterface, Some(cls))
        case FrameworkSelection.PlatformRunner(name)     =>
          // Unreachable by construction: the test handler routes JS/Native suites to their own runners before any fork exists. If it ever is reached, the
          // fork cannot run this suite and a placeholder command would only move the failure somewhere less obvious.
          sys.error(s"$name is run by its platform's own runner, not by a forked JVM — it must never be sent over TestProtocol (suite ${rs.className})")
      }
      Json.obj(
        "className" -> rs.className.asJson,
        "framework" -> rs.selection.displayName.asJson,
        "runner" -> runner.asJson,
        "frameworkClass" -> frameworkClass.asJson,
        "args" -> rs.args.asJson
      )
    }

    implicit val runSuiteDecoder: Decoder[RunSuite] = Decoder.instance { cursor =>
      for {
        className <- cursor.downField("className").as[String]
        displayName <- cursor.downField("framework").as[String]
        runner <- cursor.downField("runner").as[String]
        frameworkClass <- cursor.downField("frameworkClass").as[Option[String]]
        args <- cursor.downField("args").as[List[String]]
        selection <- (runner, frameworkClass) match {
          case (RunnerWire.JUnitPlatform, _)            => Right(FrameworkSelection.JUnitPlatform(displayName))
          case (RunnerWire.SbtTestInterface, Some(cls)) => Right(FrameworkSelection.SbtTestInterface(displayName, cls))
          case (RunnerWire.SbtTestInterface, None)      => Left(DecodingFailure(s"${RunnerWire.SbtTestInterface} requires frameworkClass", cursor.history))
          case (other, _)                               => Left(DecodingFailure(s"Unknown runner: $other", cursor.history))
        }
      } yield RunSuite(className, selection, args)
    }

    // Only JUnit-Platform suites are ever batched into one execution (that is the runner with a cross-class scope worth preserving), so the wire form carries
    // the one runner + display name shared by all the classes, plus the class list and bleep's parallelism.
    implicit val runSuitesEncoder: Encoder[RunSuites] = Encoder.instance { rs =>
      // A batch's classes all share one framework, so one runner + (for sbt) one frameworkClass covers them. JUnit Platform needs no frameworkClass; sbt does.
      val (runner, frameworkClass) = rs.selection match {
        case FrameworkSelection.JUnitPlatform(_)         => (RunnerWire.JUnitPlatform, None)
        case FrameworkSelection.SbtTestInterface(_, cls) => (RunnerWire.SbtTestInterface, Some(cls))
        case other => sys.error(s"RunSuites is for JUnit-Platform or sbt-test-interface; got $other for ${rs.classNames.mkString(", ")}")
      }
      Json.obj(
        "classNames" -> rs.classNames.asJson,
        "parallelism" -> rs.parallelism.asJson,
        "framework" -> rs.selection.displayName.asJson,
        "runner" -> runner.asJson,
        "frameworkClass" -> frameworkClass.asJson,
        "args" -> rs.args.asJson
      )
    }

    implicit val runSuitesDecoder: Decoder[RunSuites] = Decoder.instance { cursor =>
      for {
        classNames <- cursor.downField("classNames").as[List[String]]
        parallelism <- cursor.downField("parallelism").as[Int]
        displayName <- cursor.downField("framework").as[String]
        runner <- cursor.downField("runner").as[String]
        frameworkClass <- cursor.downField("frameworkClass").as[Option[String]]
        args <- cursor.downField("args").as[List[String]]
        selection <- (runner, frameworkClass) match {
          case (RunnerWire.JUnitPlatform, _)            => Right(FrameworkSelection.JUnitPlatform(displayName))
          case (RunnerWire.SbtTestInterface, Some(cls)) => Right(FrameworkSelection.SbtTestInterface(displayName, cls))
          case (RunnerWire.SbtTestInterface, None) => Left(DecodingFailure(s"${RunnerWire.SbtTestInterface} RunSuites requires frameworkClass", cursor.history))
          case (other, _)                          => Left(DecodingFailure(s"Unknown runner for RunSuites: $other", cursor.history))
        }
      } yield RunSuites(classNames, parallelism, selection, args)
    }

    implicit val encoder: Encoder[TestCommand] = Encoder.instance {
      case rs: RunSuite    => Json.obj("type" -> "RunSuite".asJson, "data" -> rs.asJson)
      case rs: RunSuites   => Json.obj("type" -> "RunSuites".asJson, "data" -> rs.asJson)
      case cs: CancelSuite => Json.obj("type" -> "CancelSuite".asJson, "className" -> cs.className.asJson)
      case Shutdown        => Json.obj("type" -> "Shutdown".asJson)
      case GetThreadDump   => Json.obj("type" -> "GetThreadDump".asJson)
    }

    implicit val decoder: Decoder[TestCommand] = Decoder.instance { cursor =>
      cursor.downField("type").as[String].flatMap {
        case "RunSuite"      => cursor.downField("data").as[RunSuite]
        case "RunSuites"     => cursor.downField("data").as[RunSuites]
        case "CancelSuite"   => cursor.downField("className").as[String].map(CancelSuite(_))
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
        throwable: Option[String],
        // Where in the suite the failure was raised, when the forked runner could recover it from the throwable.
        // Absent for passing tests, for failures thrown outside the suite class, and from runners that do not report it.
        location: Option[bleep.bsp.protocol.BleepBspProtocol.SourceLocation]
    ) extends TestResponse

    /** A test suite has completed. `outcome` is reconstructed from the flat wire fields (the Java forked runner emits a `kind` discriminator plus counts) into
      * the [[SuiteOutcome]] ADT so nothing downstream re-derives meaning from an all-zero count tuple.
      */
    case class SuiteDone(
        suite: String,
        outcome: SuiteOutcome,
        durationMs: Long
    ) extends TestResponse

    /** Log output from test */
    case class Log(
        level: String,
        message: String,
        suite: Option[String]
    ) extends TestResponse

    /** An error occurred in the test runner itself */
    case class Error(
        message: String,
        throwable: Option[String]
    ) extends TestResponse

    /** The single batched execution (a `RunSuites`) has returned. Every class's own terminal (`SuiteDone`/error) has already been sent and demultiplexed to its
      * suite; this just tells the parent the one execute is over, so it can stop reading without counting terminals against a class list.
      */
    case object BatchComplete extends TestResponse

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

    // The wire is flat (kind discriminator + counts + optional message/throwable), matching the
    // hand-rolled JSON the Java forked runner emits; the outcome ADT is (re)constructed here.
    implicit val suiteDoneEncoder: Encoder[SuiteDone] = Encoder.instance { sd =>
      val base = Json.obj(
        "suite" -> sd.suite.asJson,
        "outcome" -> SuiteOutcome.tagOf(sd.outcome).asJson,
        "passed" -> sd.outcome.passedCount.asJson,
        "failed" -> sd.outcome.failedCount.asJson,
        "skipped" -> sd.outcome.skippedCount.asJson,
        "ignored" -> sd.outcome.ignoredCount.asJson,
        "durationMs" -> sd.durationMs.asJson
      )
      sd.outcome match {
        case SuiteOutcome.Errored(message, throwable) =>
          base.deepMerge(Json.obj("message" -> message.asJson, "throwable" -> throwable.asJson))
        case _ => base
      }
    }
    implicit val suiteDoneDecoder: Decoder[SuiteDone] = Decoder.instance { c =>
      for {
        suite <- c.downField("suite").as[String]
        // `outcome` is optional: a forked test-runner jar that predates the SuiteOutcome ADT (a
        // version-skewed test classpath — e.g. an older bleep-test-runner resolved from the build)
        // emits only the flat counts. Reconstruct the outcome from them rather than failing to
        // decode. This is the forked-runner analog of the version-tolerant BSP SuiteFinished codec.
        kind <- c.downField("outcome").as[Option[String]]
        passed <- c.getOrElse("passed")(0)
        failed <- c.getOrElse("failed")(0)
        skipped <- c.getOrElse("skipped")(0)
        ignored <- c.getOrElse("ignored")(0)
        durationMs <- c.downField("durationMs").as[Long]
        message <- c.downField("message").as[Option[String]]
        throwable <- c.downField("throwable").as[Option[String]]
      } yield {
        // `fromWire` already routes a tag it doesn't recognise through the same `degrade` policy the
        // BSP SuiteFinished codec uses, so both boundaries treat version skew identically.
        val outcome = kind match {
          case Some(k) => SuiteOutcome.fromWire(k, passed, failed, skipped, ignored, message, throwable)
          case None    => SuiteOutcome.degrade(None, passed, failed, skipped, ignored)
        }
        SuiteDone(suite, outcome, durationMs)
      }
    }

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
      case BatchComplete    => Json.obj("type" -> "BatchComplete".asJson)
    }

    implicit val decoder: Decoder[TestResponse] = Decoder.instance { cursor =>
      cursor.downField("type").as[String].flatMap {
        case "Ready"         => Right(Ready)
        case "TestStarted"   => cursor.downField("data").as[TestStarted]
        case "TestFinished"  => cursor.downField("data").as[TestFinished]
        case "SuiteDone"     => cursor.downField("data").as[SuiteDone]
        case "Log"           => cursor.downField("data").as[Log]
        case "Error"         => cursor.downField("data").as[Error]
        case "ThreadDump"    => cursor.downField("data").as[ThreadDump]
        case "BatchComplete" => Right(BatchComplete)
        case other           => Left(DecodingFailure(s"Unknown response type: $other", cursor.history))
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
