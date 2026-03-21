package bleep.bsp

import bleep.bsp.Outcome.KillReason
import bleep.bsp.protocol.TestStatus

/** Shared types for non-JVM test runners (Kotlin/JS, Kotlin/Native, Scala.js, Scala Native).
  *
  * These types were previously duplicated identically across KotlinTestRunner, ScalaJsTestRunner, and ScalaNativeTestRunner.
  */
object TestRunnerTypes {

  /** Test event handler for receiving test execution events from non-JVM runners. */
  trait TestEventHandler {
    def onTestStarted(suite: String, test: String): Unit
    def onTestFinished(suite: String, test: String, status: TestStatus, durationMs: Long, message: Option[String]): Unit
    def onSuiteStarted(suite: String): Unit
    def onSuiteFinished(suite: String, passed: Int, failed: Int, skipped: Int): Unit
    def onOutput(suite: String, line: String, isError: Boolean): Unit
    def onRunnerEvent(event: RunnerEvent): Unit = ()
  }

  /** Events from the test runner process about execution lifecycle. */
  sealed trait RunnerEvent
  object RunnerEvent {
    case object Started extends RunnerEvent
    case class Killed(reason: KillReason) extends RunnerEvent
    case class ProcessExited(exitCode: Int) extends RunnerEvent
    case class ProcessCrashed(signal: String, exitCode: Int) extends RunnerEvent
    case class Error(message: String, cause: Option[Throwable]) extends RunnerEvent
  }

  /** Result of test execution. */
  case class TestResult(
      passed: Int,
      failed: Int,
      skipped: Int,
      ignored: Int,
      terminationReason: TerminationReason
  ) {
    def isSuccess: Boolean = failed == 0 && terminationReason == TerminationReason.Completed
  }

  /** Why the test run terminated. */
  sealed trait TerminationReason {
    def description: String
  }
  object TerminationReason {
    case object Completed extends TerminationReason { val description = "completed normally" }
    case class Killed(reason: KillReason) extends TerminationReason {
      val description: String = reason match {
        case KillReason.UserRequest    => "cancelled by user"
        case KillReason.Timeout        => "timed out"
        case KillReason.ParentDying    => "parent process dying"
        case KillReason.ServerShutdown => "server shutting down"
        case KillReason.DeadClient     => "client disconnected"
      }
    }
    case class Crashed(signal: Int) extends TerminationReason {
      val description: String = signal match {
        case 11 => "crashed (SIGSEGV - segmentation fault)"
        case 6  => "crashed (SIGABRT - aborted)"
        case 9  => "killed (SIGKILL)"
        case 15 => "terminated (SIGTERM)"
        case n  => s"crashed (signal $n)"
      }
    }
    case class ExitCode(code: Int) extends TerminationReason {
      val description = s"exited with code $code"
    }
    case class Error(message: String) extends TerminationReason {
      val description = s"error: $message"
    }
    case class TruncatedOutput(suite: String) extends TerminationReason {
      val description = s"process exited with truncated output (suite '$suite' started but never finished)"
    }
  }

  /** A test suite discovered by a non-JVM runner. */
  case class TestSuite(
      name: String,
      fullyQualifiedName: String
  )

  /** Interpret a process exit code into a TerminationReason.
    *
    * Exit codes > 128 indicate the process was killed by a signal: signal = exitCode - 128. Common signals: 9 = SIGKILL, 11 = SIGSEGV, 6 = SIGABRT, 15 =
    * SIGTERM.
    *
    * @param exitCode
    *   the process exit code
    * @param truncatedSuite
    *   if a suite started but never finished, its name
    */
  def interpretExitCode(exitCode: Int, truncatedSuite: Option[String]): TerminationReason =
    if (exitCode == 0) {
      truncatedSuite match {
        case Some(suite) => TerminationReason.TruncatedOutput(suite)
        case None        => TerminationReason.Completed
      }
    } else if (exitCode > 128) {
      val signal = exitCode - 128
      truncatedSuite match {
        case Some(suite) => TerminationReason.TruncatedOutput(suite)
        case None        => TerminationReason.Crashed(signal)
      }
    } else {
      truncatedSuite match {
        case Some(suite) => TerminationReason.TruncatedOutput(suite)
        case None        => TerminationReason.ExitCode(exitCode)
      }
    }
}
