package bleep.bsp

import bleep.bsp.Outcome.KillReason
import bleep.bsp.protocol.BleepBspProtocol
import bleep.model.CrossProjectName
import bleep.testing.{JvmPool, TestJvm, TestProtocol}
import cats.effect._
import cats.effect.std.Queue
import cats.syntax.all._

import java.nio.file.Path
import scala.concurrent.duration._

/** Test runner that executes test suites in forked JVMs.
  *
  * Uses JvmPool for efficient JVM reuse and streams test events back through the DAG event queue. Uses Deferred-based kill signals for explicit cancellation
  * handling.
  */
object TestRunner {

  /** Options for the test runner */
  case class Options(
      jvmOptions: List[String],
      testArgs: List[String],
      idleTimeout: FiniteDuration
  )

  object Options {
    val default: Options = Options(
      jvmOptions = Nil,
      testArgs = Nil,
      idleTimeout = 2.minutes
    )
  }

  /** Run a test suite and emit events to the queue.
    *
    * @param project
    *   the project containing the suite
    * @param suiteName
    *   the fully qualified class name of the test suite
    * @param framework
    *   the test framework name
    * @param classpath
    *   full classpath for the test JVM
    * @param pool
    *   the JVM pool to acquire from
    * @param eventQueue
    *   queue to emit DAG events to
    * @param options
    *   test runner options
    * @param killSignal
    *   Deferred that can be completed to kill the suite
    * @return
    *   Success or Failure result
    */
  def runSuite(
      project: CrossProjectName,
      suiteName: String,
      framework: String,
      classpath: List[Path],
      pool: JvmPool,
      eventQueue: Queue[IO, TaskDag.DagEvent],
      options: Options,
      killSignal: Deferred[IO, KillReason]
  ): IO[TaskDag.TaskResult] = {
    val runnerClass = "bleep.testing.runner.ForkedTestRunner"

    pool.acquire(classpath, options.jvmOptions, runnerClass).use { jvm =>
      executeWithIdleTimeout(
        project = project,
        suiteName = suiteName,
        framework = framework,
        jvm = jvm,
        eventQueue = eventQueue,
        testArgs = options.testArgs,
        idleTimeout = options.idleTimeout,
        killSignal = killSignal
      )
    }
  }

  /** Execute a test suite with idle timeout and kill signal handling.
    *
    * The idle timeout resets each time a test completes. If no test completes within the timeout period, the suite is considered hung and killed.
    */
  private def executeWithIdleTimeout(
      project: CrossProjectName,
      suiteName: String,
      framework: String,
      jvm: TestJvm,
      eventQueue: Queue[IO, TaskDag.DagEvent],
      testArgs: List[String],
      idleTimeout: FiniteDuration,
      killSignal: Deferred[IO, KillReason]
  ): IO[TaskDag.TaskResult] = {
    def now: IO[Long] = IO.realTime.map(_.toMillis)

    def emit(event: TaskDag.DagEvent): IO[Unit] = eventQueue.offer(event)

    val startTime = System.currentTimeMillis()

    for {
      lastActivityAt <- Ref.of[IO, Long](startTime)

      /** Process responses from the forked JVM - streams events in real-time */
      processResponses = for {
        passedCount <- Ref.of[IO, Int](0)
        failedCount <- Ref.of[IO, Int](0)
        skippedCount <- Ref.of[IO, Int](0)
        failures <- Ref.of[IO, List[String]](Nil)

        // Process each response as it arrives (streaming, not batching)
        _ <- jvm
          .runSuite(suiteName, framework, testArgs)
          .evalMap {
            case TestProtocol.TestResponse.TestStarted(_, test) =>
              now.flatMap(ts => emit(TaskDag.DagEvent.TestStarted(project.value, suiteName, test, ts)))

            case TestProtocol.TestResponse.TestFinished(_, test, status, durationMs, message, throwable) =>
              val updateCount = status match {
                case "passed" =>
                  passedCount.update(_ + 1)
                case "failed" | "error" =>
                  failedCount.update(_ + 1) >>
                    failures.update(test :: _)
                case "skipped" | "ignored" | "pending" | "cancelled" =>
                  skippedCount.update(_ + 1)
                case _ =>
                  IO.unit
              }
              updateCount >> now.flatMap { ts =>
                // Reset idle timeout on each test completion
                lastActivityAt.set(ts) >>
                  emit(
                    TaskDag.DagEvent.TestFinished(
                      project = project.value,
                      suite = suiteName,
                      test = test,
                      status = status,
                      durationMs = durationMs,
                      message = message,
                      throwable = throwable,
                      timestamp = ts
                    )
                  )
              }

            case TestProtocol.TestResponse.SuiteDone(_, passed, failed, skipped, _, _) =>
              // Update counts from suite done if we haven't tracked individual tests
              passedCount.update(c => if (c == 0) passed else c) >>
                failedCount.update(c => if (c == 0) failed else c) >>
                skippedCount.update(c => if (c == 0) skipped else c)

            case TestProtocol.TestResponse.Log(level, message, suite) =>
              val isError = level == "error" || level == "stderr"
              val effectiveSuite = suite.getOrElse(suiteName)
              now.flatMap(ts => emit(TaskDag.DagEvent.Output(project.value, effectiveSuite, message, isError, ts)))

            case TestProtocol.TestResponse.Error(message, _) =>
              // Protocol errors - emit as test failure
              failedCount.update(_ + 1) >>
                failures.update(s"[Error] $message" :: _)

            case TestProtocol.TestResponse.Ready =>
              IO.unit

            case TestProtocol.TestResponse.ThreadDump(_) =>
              IO.unit
          }
          .compile
          .drain

        passed <- passedCount.get
        failed <- failedCount.get
        skipped <- skippedCount.get
        failureList <- failures.get
      } yield SuiteResult(passed, failed, skipped, failureList.reverse)

      // Idle timeout: polls lastActivityAt every second and fires when no activity for idleTimeout duration
      idleTimeoutIO = {
        val checkInterval = 1.second
        def loop: IO[Unit] = for {
          nowMs <- IO.realTime.map(_.toMillis)
          lastActivity <- lastActivityAt.get
          elapsed = nowMs - lastActivity
          _ <-
            if (elapsed >= idleTimeout.toMillis) IO.unit
            else IO.sleep(checkInterval) >> loop
        } yield ()
        loop
      }

      // Race between: suite execution, idle timeout, and kill signal
      result <- IO.racePair(processResponses, IO.race(idleTimeoutIO, killSignal.get)).flatMap {
        case Left((outcome, raceFiber)) =>
          // Suite completed before timeout/kill
          raceFiber.cancel >> jvm.drainStderr
            .flatMap { stderrLines =>
              if (stderrLines.nonEmpty) {
                now.flatMap { ts =>
                  stderrLines.traverse_(line => emit(TaskDag.DagEvent.Output(project.value, suiteName, line, true, ts)))
                }
              } else IO.unit
            }
            .handleError(_ => ()) >> outcome.embedError.flatMap { result =>
            val durationMs = System.currentTimeMillis() - startTime
            // Emit SuiteFinished with actual counts
            now.flatMap { ts =>
              emit(
                TaskDag.DagEvent.SuiteFinished(
                  project.value,
                  suiteName,
                  result.passed,
                  result.failed,
                  result.skipped,
                  0,
                  durationMs,
                  ts
                )
              )
            } >> IO.pure {
              if (result.failed > 0) {
                TaskDag.TaskResult.Failure(
                  error = s"${result.failed} test(s) failed",
                  diagnostics = result.failures.map(BleepBspProtocol.Diagnostic.error)
                )
              } else {
                TaskDag.TaskResult.Success
              }
            }
          }

        case Right((suiteFiber, raceOutcome)) =>
          val durationMs = System.currentTimeMillis() - startTime

          // Drain any stderr from JVM before killing it
          def drainStderrToEvents: IO[Unit] =
            jvm.drainStderr
              .flatMap { lines =>
                if (lines.nonEmpty) {
                  now.flatMap { ts =>
                    lines.traverse_(line => emit(TaskDag.DagEvent.Output(project.value, suiteName, line, true, ts)))
                  }
                } else IO.unit
              }
              .handleError(_ => ())

          // Helper for cleanup - uncancelable and recovers from errors
          def cleanup: IO[Unit] = IO.uncancelable { _ =>
            drainStderrToEvents.attempt >> jvm.kill.attempt >> suiteFiber.cancel.attempt.void
          }

          // NOTE: For timeout/kill/error cases, we do NOT emit SuiteFinished here.
          // The executor emits TaskFinished with TimedOut/Killed/Error, which consumeEvents
          // converts to SuiteTimedOut event. Emitting SuiteFinished here would cause
          // double-counting on the client side.

          raceOutcome match {
            case Outcome.Succeeded(fa) =>
              fa.flatMap {
                case Left(_) =>
                  // Idle timeout - kill JVM and report timed out (does NOT propagate to downstream tasks)
                  cleanup >> IO.pure(TaskDag.TaskResult.TimedOut)
                case Right(reason) =>
                  // Kill signal - kill JVM and report killed with reason
                  cleanup >> IO.pure(TaskDag.TaskResult.Killed(reason))
              }
            case Outcome.Errored(e) =>
              // Error during race - this is an infrastructure error, not a test failure
              cleanup >> IO.pure(
                TaskDag.TaskResult.Error(
                  error = s"Error during test: ${e.getMessage}",
                  exitCode = None,
                  signal = None
                )
              )
            case Outcome.Canceled() =>
              // Fiber was cancelled - treat as killed with default reason
              cleanup >> IO.pure(TaskDag.TaskResult.Killed(KillReason.UserRequest))
          }
      }
    } yield result
  }

  /** Result of running a suite */
  private case class SuiteResult(
      passed: Int,
      failed: Int,
      skipped: Int,
      failures: List[String]
  )
}
