package bleep.bsp

import bleep.bsp.Outcome.KillReason
import bleep.bsp.protocol.{OutputChannel, TestStatus}
import cats.effect.{Deferred, IO, Ref}
import cats.syntax.all._

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
    def onOutput(suite: String, line: String, channel: OutputChannel): Unit
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

  /** Result of interpreting a process exit code. */
  case class ExitCodeResult(
      adjustedFailed: Int,
      terminationReason: TerminationReason
  )

  /** Interpret a process exit code, fire appropriate runner events, and compute adjusted failed count.
    *
    * Exit codes > 128 indicate the process was killed by a signal: signal = exitCode - 128. Common signals: 9 = SIGKILL, 11 = SIGSEGV, 6 = SIGABRT, 15 =
    * SIGTERM.
    *
    * Condition ordering: signal detection first, then "tests actually ran" (which means the process completed enough to produce results, even if a suite
    * appears unfinished due to parser artifacts), then truncated suite for genuine startup crashes, then exit code handling.
    *
    * @param exitCode
    *   the process exit code
    * @param truncatedSuite
    *   if a suite started but never finished, its name
    * @param passed
    *   number of passed tests so far
    * @param failed
    *   number of failed tests so far
    * @param eventHandler
    *   handler for runner lifecycle events
    */
  def interpretExitCode(
      exitCode: Int,
      truncatedSuite: Option[String],
      passed: Int,
      failed: Int,
      eventHandler: TestEventHandler
  ): ExitCodeResult =
    if (exitCode > 128) {
      val signal = exitCode - 128
      truncatedSuite match {
        case Some(suite) =>
          eventHandler.onRunnerEvent(RunnerEvent.ProcessCrashed(s"truncated output for suite '$suite'", exitCode))
          ExitCodeResult(failed + 1, TerminationReason.TruncatedOutput(suite))
        case None =>
          eventHandler.onRunnerEvent(RunnerEvent.ProcessCrashed(s"signal $signal", exitCode))
          ExitCodeResult(failed, TerminationReason.Crashed(signal))
      }
    } else if (passed > 0 || failed > 0) {
      // Tests ran — non-zero exit just means test failures.
      // An unfinished suite here is a parser artifact (last suite may not have
      // an explicit close event), not actual truncation.
      eventHandler.onRunnerEvent(RunnerEvent.ProcessExited(exitCode))
      ExitCodeResult(failed, TerminationReason.Completed)
    } else if (truncatedSuite.isDefined) {
      // No tests completed but suite started — process died during startup
      val suite = truncatedSuite.get
      eventHandler.onRunnerEvent(RunnerEvent.ProcessCrashed(s"truncated output for suite '$suite'", exitCode))
      ExitCodeResult(failed + 1, TerminationReason.TruncatedOutput(suite))
    } else if (exitCode == 0) {
      eventHandler.onRunnerEvent(RunnerEvent.ProcessExited(0))
      ExitCodeResult(failed, TerminationReason.Completed)
    } else {
      eventHandler.onRunnerEvent(RunnerEvent.ProcessExited(exitCode))
      ExitCodeResult(failed + 1, TerminationReason.ExitCode(exitCode))
    }

  /** Buffer for stderr lines that arrive before any suite starts.
    *
    * Many non-JVM runners emit diagnostic output on stderr before the first suite event. This buffer collects those lines and drains them to the event handler
    * when the first suite starts.
    */
  class StderrBuffer(ref: Ref[IO, List[String]], eventHandler: TestEventHandler) {

    /** Buffer a line of stderr output (prepends for O(1), reversed on drain). */
    def buffer(line: String): IO[Unit] = ref.update(line :: _)

    /** Drain all buffered lines to the event handler for the given suite. */
    def drain(suite: String): IO[Unit] =
      ref.getAndSet(Nil).flatMap { pending =>
        pending.reverse.traverse_(l => IO.delay(eventHandler.onOutput(suite, l, OutputChannel.Stderr)))
      }
  }

  object StderrBuffer {
    def create(eventHandler: TestEventHandler): IO[StderrBuffer] =
      Ref.of[IO, List[String]](Nil).map(ref => new StderrBuffer(ref, eventHandler))
  }

  /** Start a fiber that kills a process when the kill signal fires.
    *
    * This breaks the deadlock where IO.blocking(readLine) can't be cancelled but needs the process to die for the stream to close.
    *
    * @param process
    *   the process to kill
    * @param killSignal
    *   deferred that completes when a kill is requested
    * @param killDescendants
    *   if true, kill all descendant processes first (needed for shell scripts where children inherit the pipe)
    */
  def startKillWatcher(process: Process, killSignal: Deferred[IO, KillReason], killDescendants: Boolean): IO[cats.effect.Fiber[IO, Throwable, Unit]] =
    killSignal.get
      .flatMap(_ =>
        IO.blocking {
          if (killDescendants) {
            process.descendants().forEach(p => p.destroyForcibly(): Unit)
          }
          process.destroyForcibly()
          ()
        }
      )
      .start
}
