package bleep.bsp

import bleep.bsp.Outcome.KillReason
import bleep.bsp.TestRunnerTypes.{RunnerEvent, TerminationReason, TestEventHandler, TestResult}
import cats.effect.{Deferred, IO}
import cats.syntax.all._

/** Shared skeleton for non-JVM test runners that spawn an external process.
  *
  * All 4 non-JVM test runners (ScalaJs, KotlinJs, KotlinNative, ScalaNative) follow an identical 7-step lifecycle:
  *   1. Start process via ProcessRunner
  *   2. Start kill watcher fiber
  *   3. Run preRun hook
  *   4. Drain stdout/stderr in parallel via handler callbacks
  *   5. Wait for process exit
  *   6. Interpret exit code (or killed status) into TestResult
  *   7. Cleanup
  *
  * The Config captures what varies per runner; run() implements the shared skeleton.
  */
object ProcessTestRunner {

  /** Snapshot of test execution state at a point in time. */
  case class RunState(
      passed: Int,
      failed: Int,
      skipped: Int,
      ignored: Int,
      currentSuite: Option[String]
  )

  /** Configuration for a process-based test run.
    *
    * @param processBuilder
    *   the configured ProcessBuilder to start
    * @param handleStdoutLine
    *   callback for each stdout line (parses test events)
    * @param handleStderrLine
    *   callback for each stderr line
    * @param getRunState
    *   reads the current test execution state
    * @param eventHandler
    *   handler for runner lifecycle events
    * @param killSignal
    *   signal to request process termination
    * @param killDescendants
    *   if true, kill descendant processes before the main process
    * @param preRun
    *   action to run after process starts but before reading streams
    * @param onNormalExit
    *   action after process exits normally, before reading final state
    * @param cleanup
    *   action to run unconditionally after everything (e.g. delete temp files)
    */
  case class Config(
      processBuilder: ProcessBuilder,
      handleStdoutLine: String => IO[Unit],
      handleStderrLine: String => IO[Unit],
      getRunState: IO[RunState],
      eventHandler: TestEventHandler,
      killSignal: Deferred[IO, KillReason],
      killDescendants: Boolean,
      preRun: IO[Unit],
      onNormalExit: IO[Unit],
      cleanup: IO[Unit]
  )

  /** Run tests using the shared process lifecycle.
    *
    * Starts the process, drains stdout/stderr through the configured handlers, waits for exit, and interprets the result. Handles kill signals at every stage.
    */
  def run(config: Config): IO[TestResult] = {
    val work = ProcessRunner.start(config.processBuilder).use { process =>
      TestRunnerTypes.startKillWatcher(process, config.killSignal, killDescendants = config.killDescendants).flatMap { killFiber =>
        val body = config.preRun >> {
          val stdout = ProcessRunner.lines(process.getInputStream).evalMap(config.handleStdoutLine)
          val stderr = ProcessRunner.lines(process.getErrorStream).evalMap(config.handleStderrLine)

          (stdout.compile.drain, stderr.compile.drain).parTupled.void >>
            IO.blocking(process.waitFor()).flatMap { exitCode =>
              config.killSignal.tryGet.flatMap {
                case Some(reason) =>
                  config.getRunState.map { state =>
                    config.eventHandler.onRunnerEvent(RunnerEvent.Killed(reason))
                    TestResult(state.passed, state.failed, state.skipped, state.ignored, TerminationReason.Killed(reason))
                  }
                case None =>
                  config.onNormalExit >> config.getRunState.map { state =>
                    val result = TestRunnerTypes.interpretExitCode(exitCode, state.currentSuite, state.passed, state.failed, config.eventHandler)
                    TestResult(state.passed, result.adjustedFailed, state.skipped, state.ignored, result.terminationReason)
                  }
              }
            }
        }
        body.guarantee(killFiber.cancel)
      }
    }

    Outcome
      .raceKill(config.killSignal)(work)
      .flatMap {
        case Left(result) => IO.pure(result)
        case Right(reason) =>
          config.getRunState.map { state =>
            config.eventHandler.onRunnerEvent(RunnerEvent.Killed(reason))
            TestResult(state.passed, state.failed, state.skipped, state.ignored, TerminationReason.Killed(reason))
          }
      }
      .guarantee(config.cleanup)
  }
}
