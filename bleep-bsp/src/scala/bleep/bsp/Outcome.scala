package bleep.bsp

import bleep.bsp.protocol.KillReason
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

  /** Outcome of `runInFreshThread`: every outcome is a value — success, cancellation, or thrown error all flow through this ADT rather than through CE's error
    * or fiber-cancellation channels. Mirrors `RunOutcome` for processes.
    */
  sealed trait ThreadOutcome[+A]
  object ThreadOutcome {
    case class Completed[A](result: A) extends ThreadOutcome[A]
    case class Cancelled(reason: KillReason) extends ThreadOutcome[Nothing]
    case class Crashed(throwable: Throwable) extends ThreadOutcome[Nothing]
  }

  /** Run a synchronous, potentially long-blocking computation on a fresh dedicated thread, returning `IO[ThreadOutcome[A]]`.
    *
    * The work runs on a newly-started thread (named, daemon, optionally with a context classloader). A `Deferred[IO, KillReason]` is built from the
    * `CancellationToken` (via `Outcome.fromCancellationToken`) and raced against the work; if the kill signal fires first, the thread is interrupted and we
    * return `Cancelled(reason)`. If the work completes normally, `Completed(result)`. If the work throws, `Crashed(throwable)`. No exceptions ever leak out of
    * the returned IO's error channel — every outcome is a value.
    *
    * Use this in preference to `IO.interruptibleMany` when:
    *   - The computation has thread-local state that must not leak between calls (e.g. JetBrains Kotlin compiler — see KT-28037)
    *   - The computation makes long blocking JNI / native calls that could starve CE's blocker pool (e.g. Scala.js / Scala Native linkers)
    *
    * Use `IO.interruptibleMany` for plain in-JVM blocking work without those concerns.
    */
  def runInFreshThread[A](
      name: String,
      contextClassLoader: Option[ClassLoader],
      cancellation: bleep.analysis.CancellationToken
  )(work: => A): IO[ThreadOutcome[A]] =
    fromCancellationToken(cancellation).flatMap { killSignal =>
      val workIO: IO[Either[Throwable, A]] = IO.async[Either[Throwable, A]] { cb =>
        IO.delay {
          val runnable: Runnable = () =>
            try cb(Right(Right(work)))
            catch { case e: Throwable => cb(Right(Left(e))) }
          val t = new Thread(runnable, name)
          contextClassLoader.foreach(t.setContextClassLoader)
          t.setDaemon(true)
          t.start()
          Some(IO.delay {
            cancellation.cancel()
            t.interrupt()
          })
        }
      }
      raceKill(killSignal)(workIO).map {
        case Left(Right(value)) =>
          ThreadOutcome.Completed(value)
        // `InterruptedException` is the JVM's cancellation signal — Thread.interrupt at blocking calls, plus toolchain-specific cancellation exceptions that
        // extend it (LinkingCancelledException, CompilationCancelledException). Classify all as `Cancelled` rather than `Crashed`.
        case Left(Left(_: InterruptedException)) =>
          ThreadOutcome.Cancelled(KillReason.UserRequest)
        case Left(Left(throwable)) =>
          ThreadOutcome.Crashed(throwable)
        case Right(reason) =>
          ThreadOutcome.Cancelled(reason)
      }
    }

  /** Bridge a Deferred kill signal to a [[bleep.analysis.CancellationToken]] for toolchains that still use CancellationToken.
    *
    * Returns a `Resource` rather than a plain `IO` so the listener fiber is properly lifecycle-managed: the fiber blocks on `killSignal.get` and, if the
    * killSignal never fires (the common case — the operation completes normally), `.background` cancels it on resource release. With `.start.void` the fiber
    * would block forever, pinning the Deferred and the token in memory once per operation.
    *
    * Usage: `bridgeKillSignal(killSignal).use { cancellation => ...do the work that takes a CancellationToken... }`.
    */
  def bridgeKillSignal(killSignal: Deferred[IO, KillReason]): cats.effect.Resource[IO, bleep.analysis.CancellationToken] = {
    import java.util.concurrent.atomic.AtomicBoolean
    import scala.collection.mutable.ListBuffer

    val mkToken: IO[bleep.analysis.CancellationToken] = IO.delay {
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
    }

    // .background returns a Resource whose acquire spawns the listener and whose release cancels it. If killSignal fires before release, the listener cancels
    // the token and exits naturally. If release fires first (the work completed normally), .background cancels the still-blocked listener.
    for {
      token <- cats.effect.Resource.eval(mkToken)
      _ <- killSignal.get.flatMap(_ => IO.delay(token.cancel())).background
    } yield token
  }

}
