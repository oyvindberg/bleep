package bleep.bsp

import bleep.bsp.Outcome.KillReason
import bleep.bsp.protocol.{BleepBspProtocol, OutputChannel, ProcessExit, TestStatus}
import bleep.model.{CrossProjectName, SuiteName, TestName}
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
      idleTimeout: FiniteDuration,
      environment: Map[String, String],
      workingDirectory: Option[Path]
  )

  object Options {
    val default: Options = Options(
      jvmOptions = Nil,
      testArgs = Nil,
      idleTimeout = 2.minutes,
      environment = Map.empty,
      workingDirectory = None
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
      eventQueue: Queue[IO, Option[TaskDag.DagEvent]],
      options: Options,
      killSignal: Deferred[IO, KillReason]
  ): IO[TaskDag.TaskResult] = {
    val runnerClass = "bleep.testing.runner.ForkedTestRunner"

    pool.acquire(classpath, options.jvmOptions, runnerClass, options.environment, options.workingDirectory).use { jvm =>
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
      eventQueue: Queue[IO, Option[TaskDag.DagEvent]],
      testArgs: List[String],
      idleTimeout: FiniteDuration,
      killSignal: Deferred[IO, KillReason]
  ): IO[TaskDag.TaskResult] = {
    def now: IO[Long] = IO.realTime.map(_.toMillis)

    def emit(event: TaskDag.DagEvent): IO[Unit] = eventQueue.offer(Some(event))

    val startTime = System.currentTimeMillis()

    for {
      lastActivityAt <- Ref.of[IO, Long](startTime)

      /** Process responses from the forked JVM - streams events in real-time */
      processResponses =
        for {
          passedCount <- Ref.of[IO, Int](0)
          failedCount <- Ref.of[IO, Int](0)
          skippedCount <- Ref.of[IO, Int](0)
          failures <- Ref.of[IO, List[String]](Nil)

          // Process each response as it arrives (streaming, not batching)
          _ <- jvm
            .runSuite(suiteName, framework, testArgs)
            .evalMap {
              case TestProtocol.TestResponse.TestStarted(_, test) =>
                now.flatMap(ts => lastActivityAt.set(ts) >> emit(TaskDag.DagEvent.TestStarted(project, SuiteName(suiteName), TestName(test), ts)))

              case TestProtocol.TestResponse.TestFinished(_, test, statusStr, durationMs, message, throwable) =>
                val status = TestStatus.fromString(statusStr)
                val updateCount =
                  if (status == TestStatus.Passed) passedCount.update(_ + 1)
                  else if (status.isFailure) failedCount.update(_ + 1) >> failures.update(test :: _)
                  else skippedCount.update(_ + 1)
                updateCount >> now.flatMap { ts =>
                  // Reset idle timeout on each test completion
                  lastActivityAt.set(ts) >>
                    emit(
                      TaskDag.DagEvent.TestFinished(
                        project = project,
                        suite = SuiteName(suiteName),
                        test = TestName(test),
                        status = status,
                        durationMs = durationMs,
                        message = message,
                        throwable = throwable,
                        timestamp = ts
                      )
                    )
                }

              case TestProtocol.TestResponse.SuiteDone(_, passed, failed, skipped, _, _) =>
                // Use the higher of accumulated individual counts vs authoritative SuiteDone.
                // Individual TestFinished events may be partially received (stream ended early),
                // so SuiteDone provides a correction floor.
                passedCount.update(c => math.max(c, passed)) >>
                  failedCount.update(c => math.max(c, failed)) >>
                  skippedCount.update(c => math.max(c, skipped))

              case TestProtocol.TestResponse.Log(level, message, suite) =>
                val isError = level == "error" || level == "stderr"
                val effectiveSuite = suite.getOrElse(suiteName)
                now.flatMap(ts => emit(TaskDag.DagEvent.Output(project, SuiteName(effectiveSuite), message, OutputChannel.fromIsError(isError), ts)))

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
                  stderrLines.traverse_(line => emit(TaskDag.DagEvent.Output(project, SuiteName(suiteName), line, OutputChannel.Stderr, ts)))
                }
              } else IO.unit
            }
            .handleError(e => System.err.println(s"[TestRunner] stderr drain failed: ${e.getClass.getName}: ${e.getMessage}")) >> outcome.embedError.flatMap {
            result =>
              val durationMs = System.currentTimeMillis() - startTime
              // Emit SuiteFinished with actual counts
              now.flatMap { ts =>
                emit(
                  TaskDag.DagEvent.SuiteFinished(
                    project,
                    SuiteName(suiteName),
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
                    lines.traverse_(line => emit(TaskDag.DagEvent.Output(project, SuiteName(suiteName), line, OutputChannel.Stderr, ts)))
                  }
                } else IO.unit
              }
              .handleError(e => System.err.println(s"[TestRunner] stderr drain failed: ${e.getClass.getName}: ${e.getMessage}"))

          // Helper for cleanup - uncancelable and recovers from errors
          def cleanup: IO[Unit] = IO.uncancelable { _ =>
            drainStderrToEvents.attempt >> jvm.kill.attempt >> suiteFiber.cancel.attempt.void
          }

          // On idle timeout the test runner JVM is alive but stuck. Run jstack against it so
          // we capture every thread's stack frames, then ship the dump back through the
          // protocol (TaskResult.TimedOut → SuiteTimedOut.threadDump). Without this the user
          // just sees "Suite idle timeout after 120s" with no idea what the JVM was doing.
          // jstack writes to its own stdout, decoupled from the test JVM's stdio, so the
          // protocol stream doesn't get polluted.
          def captureThreadDump: IO[Option[String]] =
            jvm.dumpThreads.attempt.map {
              case Right(lines) if lines.nonEmpty => Some(lines.mkString("\n"))
              case _                              => None
            }

          // NOTE: For timeout/kill/error cases, we do NOT emit SuiteFinished here.
          // The executor emits TaskFinished with TimedOut/Killed/Error, which consumeEvents
          // converts to SuiteTimedOut event. Emitting SuiteFinished here would cause
          // double-counting on the client side.

          raceOutcome match {
            case Outcome.Succeeded(fa) =>
              fa.flatMap {
                case Left(_) =>
                  // Idle timeout - dump threads, kill JVM, ship the dump out via TimedOut
                  captureThreadDump.flatMap(dump => IO.uncancelable(_ => cleanup) >> IO.pure(TaskDag.TaskResult.TimedOut(dump)))
                case Right(reason) =>
                  // Kill signal - kill JVM and report killed with reason
                  cleanup >> IO.pure(TaskDag.TaskResult.Killed(reason))
              }
            case Outcome.Errored(e) =>
              // Error during race - this is an infrastructure error, not a test failure
              cleanup >> IO.pure(
                TaskDag.TaskResult.Error(
                  error = s"Error during test: ${e.getMessage}",
                  processExit = ProcessExit.Unknown
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
