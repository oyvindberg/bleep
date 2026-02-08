package bleep.bsp

import bleep.bsp.Outcome.{KillReason, ProcessOutcome, RunOutcome}
import cats.effect.{Deferred, IO, Resource}

/** Shared utilities for running external processes with explicit outcome tracking.
  *
  * All process operations return explicit ADTs describing what happened. Kill signals are communicated via Deferred[IO, KillReason] which the caller can
  * complete to request termination.
  *
  * Process cleanup happens automatically via Resource release (destroyForcibly). This fires when the Resource scope ends, whether due to normal completion,
  * kill signal, or any other reason.
  */
object ProcessRunner {

  /** Result of test suite discovery. Shared across all runner types. */
  sealed trait DiscoveryResult[+A]
  object DiscoveryResult {
    case class Found[A](suites: A) extends DiscoveryResult[A]
    case class Killed(reason: KillReason) extends DiscoveryResult[Nothing]
    case class Failed(message: String) extends DiscoveryResult[Nothing]

    /** Backward compatibility alias - prefer Killed with explicit reason */
    val Cancelled: DiscoveryResult[Nothing] = Killed(KillReason.UserRequest)
  }

  /** Bridge for code still using CancellationToken.
    *
    * Converts CancellationToken polling to an IO that completes when cancelled.
    */
  def asCancelIO(cancellation: bleep.analysis.CancellationToken): IO[Unit] =
    if (cancellation.isCancelled) IO.unit
    else IO.sleep(scala.concurrent.duration.Duration(100, "millis")) >> asCancelIO(cancellation)

  /** A managed process that cleans up on release via destroyForcibly + waitFor. */
  def start(pb: ProcessBuilder): Resource[IO, Process] =
    Resource.make(IO.blocking(pb.start())) { p =>
      IO.blocking {
        p.destroyForcibly()
        p.waitFor()
        ()
      }
    }

  /** Read lines from a process stream as fs2.Stream.
    *
    * Handles IOException gracefully since destroyForcibly() closes streams.
    */
  def lines(stream: java.io.InputStream): fs2.Stream[IO, String] = {
    val reader = new java.io.BufferedReader(new java.io.InputStreamReader(stream))
    fs2.Stream
      .repeatEval(IO.blocking(Option(reader.readLine())).recover { case _: java.io.IOException => None })
      .unNoneTerminate
  }

  /** Run a process to completion OR until killed.
    *
    * Returns ProcessOutcome - never throws, never uses error channel. The caller provides a killSignal Deferred that they can complete to request termination.
    *
    * @param pb
    *   ProcessBuilder configured for the process to run
    * @param killSignal
    *   Deferred that the caller can complete to kill the process
    * @return
    *   ProcessOutcome indicating whether process exited or was killed
    */
  def runToCompletion(
      pb: ProcessBuilder,
      killSignal: Deferred[IO, KillReason]
  ): IO[ProcessOutcome] =
    start(pb).use { process =>
      val waitForExit = IO.blocking(process.waitFor())
      Outcome.raceKill(killSignal)(waitForExit).map {
        case Left(exitCode) => ProcessOutcome.Exited(exitCode)
        case Right(reason)  => ProcessOutcome.Killed(reason)
      }
    }

  /** Run a process and capture output, returning explicit outcome.
    *
    * @param pb
    *   ProcessBuilder configured for the process to run (should NOT use inheritIO)
    * @param killSignal
    *   Deferred that the caller can complete to kill the process
    * @return
    *   RunOutcome with stdout/stderr captured
    */
  def runWithOutput(
      pb: ProcessBuilder,
      killSignal: Deferred[IO, KillReason]
  ): IO[RunOutcome] =
    start(pb).use { process =>
      for {
        stdoutFiber <- lines(process.getInputStream).compile.toList.start
        stderrFiber <- lines(process.getErrorStream).compile.toList.start
        waitFiber <- IO.blocking(process.waitFor()).start
        result <- Outcome.raceKill(killSignal)(waitFiber.joinWithNever)
        // Always join the output fibers to get whatever was captured
        stdout <- stdoutFiber.joinWithNever
        stderr <- stderrFiber.joinWithNever
        stdoutStr = stdout.mkString("\n")
        stderrStr = stderr.mkString("\n")
      } yield result match {
        case Left(exitCode) =>
          RunOutcome.fromExitCode(exitCode, stdoutStr, stderrStr)
        case Right(reason) =>
          RunOutcome.Killed(reason, stdoutStr, stderrStr)
      }
    }

  /** Create a kill signal that can be completed to request process termination. */
  def createKillSignal: IO[Deferred[IO, KillReason]] =
    Deferred[IO, KillReason]
}
