package bleep.bsp

import cats.effect.{Deferred, IO}

/** Shared ADT types for explicit outcome tracking.
  *
  * This architecture eliminates cats-effect's implicit cancellation and error channels in favor of explicit ADTs that describe exactly what happened. Every
  * function that can timeout, crash, or be killed MUST return an outcome that expresses that.
  *
  * Key principles:
  *   - NO cats-effect fiber cancellation
  *   - NO shutdown hooks
  *   - ALWAYS return explicit outcomes
  *   - Kill signals via Deferred[IO, KillReason]
  */
object Outcome {

  /** Why a process/task was killed.
    *
    * This is the explicit kill signal - when you want to stop something, complete a Deferred[IO, KillReason] with one of these values. The running code will
    * then include this reason in its Killed outcome.
    */
  sealed trait KillReason
  object KillReason {

    /** User requested cancellation (Ctrl-C, $/cancelRequest) */
    case object UserRequest extends KillReason

    /** Operation exceeded its time limit */
    case object Timeout extends KillReason

    /** Parent process/task is dying and taking children with it */
    case object ParentDying extends KillReason

    /** Server is shutting down */
    case object ServerShutdown extends KillReason

    /** Client connection died (failed to send notification) */
    case object DeadClient extends KillReason
  }

  /** What can happen when we wait for a process to exit.
    *
    * This is the lowest-level outcome - just whether the process exited normally or was killed.
    */
  sealed trait ProcessOutcome
  object ProcessOutcome {

    /** Process exited with the given exit code */
    case class Exited(exitCode: Int) extends ProcessOutcome

    /** Process was killed before it could exit */
    case class Killed(reason: KillReason) extends ProcessOutcome
  }

  /** What can happen when running external code (Node.js, native binary).
    *
    * Includes stdout/stderr because we often need to capture output for error reporting. For Killed outcomes, we include partial output captured before the
    * kill.
    */
  sealed trait RunOutcome
  object RunOutcome {

    /** Process ran to completion with the given exit code */
    case class Completed(exitCode: Int, stdout: String, stderr: String) extends RunOutcome

    /** Process was killed before completion */
    case class Killed(reason: KillReason, partialStdout: String, partialStderr: String) extends RunOutcome

    /** Process crashed with a signal (exitCode > 128 on Unix) */
    case class Crashed(signal: Int, exitCode: Int, stdout: String, stderr: String) extends RunOutcome

    /** Convenience: create from exit code, auto-detecting crash vs normal completion */
    def fromExitCode(exitCode: Int, stdout: String, stderr: String): RunOutcome =
      if (exitCode > 128) Crashed(exitCode - 128, exitCode, stdout, stderr)
      else Completed(exitCode, stdout, stderr)
  }

  /** What can happen when running a test suite.
    *
    * More detailed than RunOutcome because tests have structured results (pass/fail counts, events).
    */
  sealed trait TestSuiteOutcome
  object TestSuiteOutcome {

    /** Suite ran to completion */
    case class Completed(
        passed: Int,
        failed: Int,
        skipped: Int,
        ignored: Int,
        durationMs: Long
    ) extends TestSuiteOutcome

    /** Suite was killed before completion */
    case class Killed(
        reason: KillReason,
        partialPassed: Int,
        partialFailed: Int,
        partialSkipped: Int,
        partialIgnored: Int
    ) extends TestSuiteOutcome

    /** Suite process crashed */
    case class Crashed(
        signal: Int,
        exitCode: Int,
        partialPassed: Int,
        partialFailed: Int,
        stderr: String
    ) extends TestSuiteOutcome

    /** Suite couldn't start (classpath issue, missing binary, etc) */
    case class FailedToStart(error: String, details: Option[String]) extends TestSuiteOutcome
  }

  /** What can happen to a build operation overall.
    *
    * This is the top-level outcome returned to the user/IDE.
    */
  sealed trait BuildOutcome
  object BuildOutcome {

    /** All operations completed successfully */
    case class Success(summary: BuildSummary) extends BuildOutcome

    /** Tests completed but some failed */
    case class TestsFailed(summary: BuildSummary) extends BuildOutcome

    /** Compilation failed */
    case class CompileFailed(errors: List[String]) extends BuildOutcome

    /** Operation was killed */
    case class Killed(reason: KillReason, partialSummary: BuildSummary) extends BuildOutcome

    /** BSP server died unexpectedly */
    case class ServerDied(lastOutput: String) extends BuildOutcome

    /** Couldn't connect to BSP server */
    case class ConnectionFailed(error: String) extends BuildOutcome
  }

  /** Summary of a build/test run */
  case class BuildSummary(
      projectsCompiled: Int,
      testsRun: Int,
      testsPassed: Int,
      testsFailed: Int,
      testsSkipped: Int,
      durationMs: Long
  )

  object BuildSummary {
    val empty: BuildSummary = BuildSummary(0, 0, 0, 0, 0, 0)
  }

  /** Helper to race an IO against a kill signal.
    *
    * This is the standard pattern for making any IO killable: {{{ Outcome.raceKill(killSignal)(myLongRunningIO).map { case Left(result) =>
    * handleNormalResult(result) case Right(reason) => MyOutcome.Killed(reason) } }}}
    */
  def raceKill[A](killSignal: Deferred[IO, KillReason])(io: IO[A]): IO[Either[A, KillReason]] =
    IO.race(io, killSignal.get)

  /** Convert a Deferred kill signal to an IO that completes when killed.
    *
    * Useful for integrating with existing code that expects an IO to race against.
    */
  def asKillIO(killSignal: Deferred[IO, KillReason]): IO[KillReason] =
    killSignal.get

  /** Create a kill signal from a CancellationToken.
    *
    * This bridges the CancellationToken interface to the Deferred-based kill signal. The Deferred will be completed with UserRequest when the CancellationToken
    * is cancelled. Uses the token's onCancel callback for instant notification (no polling).
    */
  def fromCancellationToken(cancellation: bleep.analysis.CancellationToken): IO[Deferred[IO, KillReason]] =
    Deferred[IO, KillReason].flatTap { deferred =>
      IO {
        cancellation.onCancel { () =>
          // Complete the kill signal from the cancellation callback thread.
          // unsafeRunSync is safe here — Deferred.complete is non-blocking.
          deferred.complete(KillReason.UserRequest).attempt.unsafeRunSync()(cats.effect.unsafe.IORuntime.global)
          ()
        }
      }
    }

  /** Create a kill signal that never fires. Use for operations that don't support cancellation. */
  def neverKillSignal: IO[Deferred[IO, KillReason]] =
    Deferred[IO, KillReason]

  /** Bridge a Deferred kill signal to CancellationToken for toolchains that still use CancellationToken.
    *
    * Creates a CancellationToken that gets cancelled when the kill signal fires. This is needed for Scala.js and Scala Native toolchains which predate the
    * Deferred-based kill signal design.
    */
  def bridgeKillSignal(killSignal: Deferred[IO, KillReason]): IO[bleep.analysis.CancellationToken] = {
    import java.util.concurrent.atomic.AtomicBoolean
    import scala.collection.mutable.ListBuffer

    IO.delay {
      val cancelled = new AtomicBoolean(false)
      val callbacks = ListBuffer[() => Unit]()

      new bleep.analysis.CancellationToken {
        def isCancelled: Boolean = cancelled.get()
        def cancel(): Unit =
          if (cancelled.compareAndSet(false, true)) {
            callbacks.synchronized {
              callbacks.foreach(cb => cb())
            }
          }
        def onCancel(callback: () => Unit): Unit =
          callbacks.synchronized {
            if (cancelled.get()) callback()
            else callbacks += callback
          }
      }
    }.flatTap { token =>
      killSignal.get.flatMap(_ => IO.delay(token.cancel())).start.void
    }
  }

  /** Race a link/compile IO against a kill signal, handling CancellationException from toolchains.
    *
    * This is the standard pattern for linking operations: race against kill, map the killed outcome, and handle CancellationException from toolchains that use
    * CancellationToken internally.
    */
  def raceLinkWork(
      killSignal: Deferred[IO, KillReason],
      platformName: String
  )(work: IO[(TaskDag.TaskResult, TaskDag.LinkResult)]): IO[(TaskDag.TaskResult, TaskDag.LinkResult)] =
    raceKill(killSignal)(work)
      .map {
        case Left(result)  => result
        case Right(reason) => (TaskDag.TaskResult.Killed(reason), TaskDag.LinkResult.Cancelled)
      }
      .handleErrorWith {
        case _: java.util.concurrent.CancellationException =>
          killSignal.tryGet.map {
            case Some(reason) => (TaskDag.TaskResult.Killed(reason), TaskDag.LinkResult.Cancelled)
            case None         => (TaskDag.TaskResult.Killed(KillReason.UserRequest), TaskDag.LinkResult.Cancelled)
          }
        case ex =>
          IO.pure(
            (
              TaskDag.TaskResult.Error(s"$platformName linker crashed: ${ex.getMessage}", bleep.bsp.protocol.ProcessExit.Unknown),
              TaskDag.LinkResult.Failure(ex.getMessage, List.empty)
            )
          )
      }
}
