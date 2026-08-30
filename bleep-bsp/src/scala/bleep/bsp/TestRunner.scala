package bleep.bsp

import bleep.MachineResources
import bleep.bsp.protocol.KillReason
import bleep.bsp.protocol.{BleepBspProtocol, OutputChannel, ProcessExit, SuiteOutcome, TestStatus}
import bleep.model.{CrossProjectName, SuiteName, TestName}
import bleep.testing.{FrameworkSelection, JvmPool, TestJvm, TestProtocol}
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
      /** Heap for a fork whose `jvmOptions` state no `-Xmx` — the `testRunnerHeap` user setting, or bleep's default when it is unset. A project that states its
        * own `-Xmx` runs with that instead; this number is a default, not a ceiling over it.
        */
      defaultHeapMb: Long,
      testArgs: List[String],
      idleTimeout: FiniteDuration,
      environment: Map[String, String],
      workingDirectory: Option[Path]
  )

  object Options {
    val default: Options = Options(
      jvmOptions = Nil,
      defaultHeapMb = MachineResources.DefaultForkHeapMb,
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
    * @param selection
    *   how to run the suite: which runner, and for the sbt path which `Framework` class
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
  /** `resolveSourcePath` turns a failing frame's declaring class into a build-relative source path. Passed as a function rather than as the analysis machinery
    * itself so this stays a process runner: the caller knows where the project's analysis lives, and this only knows it wants a path.
    */
  def runSuite(
      project: CrossProjectName,
      suiteName: String,
      selection: FrameworkSelection,
      classpath: List[Path],
      pool: JvmPool,
      eventQueue: Queue[IO, Option[TaskDag.DagEvent]],
      options: Options,
      resolveSourcePath: String => Option[String],
      killSignal: Deferred[IO, KillReason]
  ): IO[TaskDag.TaskResult] = {
    val runnerClass = "bleep.testing.runner.ForkedTestRunner"

    pool.acquire(suiteName, classpath, options.jvmOptions, options.defaultHeapMb, runnerClass, options.environment, options.workingDirectory).use { jvm =>
      // Recorded here rather than in the pool because this is the only place that knows both which JVM was handed out and what is about to run on it. The pid
      // joins these to the fork_start/fork_end pair, which is what lets a test run be reconstructed: which suites shared a JVM, and which JVM was killed
      // under which suite.
      val startedAt = System.currentTimeMillis()
      IO(BspMetrics.recordSuiteScheduled(jvm.pid, project.value, suiteName, selection.displayName)).attempt >>
        executeWithIdleTimeout(
          project = project,
          suiteName = suiteName,
          selection = selection,
          jvm = jvm,
          eventQueue = eventQueue,
          testArgs = options.testArgs,
          idleTimeout = options.idleTimeout,
          resolveSourcePath = resolveSourcePath,
          killSignal = killSignal
        ).flatTap { result =>
          IO(
            BspMetrics
              .recordSuiteFinished(jvm.pid, project.value, suiteName, System.currentTimeMillis() - startedAt, result.getClass.getSimpleName.stripSuffix("$"))
          ).attempt
        }
    }
  }

  /** Execute a test suite with idle timeout and kill signal handling.
    *
    * The idle timeout resets each time a test completes. If no test completes within the timeout period, the suite is considered hung and killed.
    */
  private def executeWithIdleTimeout(
      project: CrossProjectName,
      suiteName: String,
      selection: FrameworkSelection,
      jvm: TestJvm,
      eventQueue: Queue[IO, Option[TaskDag.DagEvent]],
      testArgs: List[String],
      idleTimeout: FiniteDuration,
      resolveSourcePath: String => Option[String],
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
          // The suite's terminal signal: Right(outcome) from an authoritative SuiteDone, or
          // Left(message) from a protocol Error (JVM died / bad JSON). None means the stream ended
          // without either — treated as an infrastructure error below.
          terminal <- Ref.of[IO, Option[Either[String, SuiteOutcome]]](None)

          // Process each response as it arrives (streaming, not batching)
          _ <- jvm
            .runSuite(suiteName, selection, testArgs)
            .evalMap {
              case TestProtocol.TestResponse.TestStarted(_, test) =>
                now.flatMap(ts => lastActivityAt.set(ts) >> emit(TaskDag.DagEvent.TestStarted(project, SuiteName(suiteName), TestName(test), ts)))

              case TestProtocol.TestResponse.TestFinished(_, test, statusStr, durationMs, message, throwable, location) =>
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
                        timestamp = ts,
                        // The forked JVM knows the class and the bare file name; only this side can say where that source lives.
                        location = location.map(loc => loc.copy(path = resolveSourcePath(loc.declaringClass)))
                      )
                    )
                }

              case TestProtocol.TestResponse.SuiteDone(_, outcome, _) =>
                terminal.set(Some(Right(outcome)))

              case TestProtocol.TestResponse.Log(level, message, suite) =>
                // `debug` is bleep talking to itself — "Loading framework: …", "Matched fingerprint: …", the fork announcing which suite it was handed.
                // It used to be forwarded as test output, so every user's failing test came with four lines of our internals above the framework's own
                // words, and the same noise was written into `<system-out>` of every JUnit report. It goes to the daemon log now, where a person
                // debugging bleep can still find it with BLEEP_BSP_DEBUG=true, and nowhere near the report.
                if (level == "debug")
                  IO.delay(MultiWorkspaceBspServer.debugLogStatic(s"[${suite.getOrElse(suiteName)}] $message"))
                else {
                  val isError = level == "error" || level == "stderr"
                  val effectiveSuite = suite.getOrElse(suiteName)
                  now.flatMap(ts => emit(TaskDag.DagEvent.Output(project, SuiteName(effectiveSuite), message, OutputChannel.fromIsError(isError), ts)))
                }

              case TestProtocol.TestResponse.Error(message, _) =>
                // Infrastructure error (JVM died mid-stream, or malformed response) — no authoritative
                // SuiteDone. Record it as the terminal signal so we emit SuiteError, not a green suite.
                terminal.set(Some(Left(message)))

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
          term <- terminal.get
        } yield SuiteResult(term, passed, failed, skipped, failureList.reverse)

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
              result.terminal match {
                // Authoritative suite outcome from the forked runner. Emit exactly one SuiteFinished
                // carrying it, then derive the TaskResult from the variant — no count arithmetic, no
                // separate SuiteError (the TaskFinished mapping returns None for a Failure because
                // SuiteFinished already conveyed the reason).
                case Some(Right(rawOutcome)) =>
                  // Reconcile Executed counts with what we streamed, in case some TestFinished events
                  // were richer than the runner's tally (belt-and-suspenders floor).
                  val outcome = rawOutcome match {
                    case SuiteOutcome.Executed(p, f, s, i) =>
                      SuiteOutcome.Executed(math.max(p, result.passed), math.max(f, result.failed), math.max(s, result.skipped), i)
                    case other => other
                  }
                  now.flatMap(ts => emit(TaskDag.DagEvent.SuiteFinished(project, SuiteName(suiteName), outcome, durationMs, ts))) >>
                    IO.pure(taskResultFor(suiteName, outcome, result.failures))

                // Infrastructure failure: the JVM died mid-stream or sent garbage, with no SuiteDone.
                // No SuiteFinished — return an Error so the TaskFinished mapping emits SuiteError.
                case Some(Left(message)) =>
                  IO.pure(TaskDag.TaskResult.Error(error = message, processExit = ProcessExit.Unknown))

                case None =>
                  IO.pure(
                    TaskDag.TaskResult.Error(
                      error = s"suite $suiteName produced no terminal event (forked JVM ended silently)",
                      processExit = ProcessExit.Unknown
                    )
                  )
              }
          }

        case Right((suiteFiber, raceOutcome)) =>
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
          //
          // Bound the dump itself to 5s — jstack attaches via the JVM tool interface, which
          // can stall during a long GC pause or kernel signal handling. Without this cap a
          // wedged JVM keeps the TimedOut → SuiteTimedOut protocol event from ever firing,
          // turning a "suite stuck" into "BSP appears stuck".
          def captureThreadDump: IO[Option[String]] =
            IO.race(jvm.dumpThreads.attempt, IO.sleep(5.seconds)).map {
              case Left(Right(lines)) if lines.nonEmpty => Some(lines.mkString("\n"))
              case Left(_)                              => None
              case Right(_)                             => Some("(thread dump timed out after 5s — JVM unresponsive)")
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

  /** Accumulated state of a suite run. `terminal` is the authoritative outcome (Right) or an infrastructure error (Left); the counts and `failures` are what we
    * streamed from individual TestFinished events.
    */
  private case class SuiteResult(
      terminal: Option[Either[String, SuiteOutcome]],
      passed: Int,
      failed: Int,
      skipped: Int,
      failures: List[String]
  )

  /** Map a suite outcome to a DAG task result. One match over the ADT, no count comparisons.
    *
    * `Empty` is a success: a test class with no tests in it is an ordinary thing to have, and failing the build over one was both wrong and inconsistent —
    * munit reports such a class as skipped and the build passed, ScalaTest reports it as empty and the same build failed. Kept in step with
    * [[SuiteOutcome.isFailure]], which decides the same question for the summary; when these two disagreed, the run showed "1 failed" with no failure to point
    * at.
    */
  private def taskResultFor(suiteName: String, outcome: SuiteOutcome, failures: List[String]): TaskDag.TaskResult =
    outcome match {
      case SuiteOutcome.Executed(_, failed, _, _) if failed > 0 =>
        TaskDag.TaskResult.Failure(error = s"$failed test(s) failed", diagnostics = failures.map(BleepBspProtocol.Diagnostic.error))
      case _: SuiteOutcome.Executed =>
        TaskDag.TaskResult.Success
      case SuiteOutcome.Empty =>
        TaskDag.TaskResult.Success
      case SuiteOutcome.NoFrameworkMatched =>
        TaskDag.TaskResult.Failure(error = s"no test framework/engine claimed suite $suiteName", diagnostics = Nil)
      case SuiteOutcome.Errored(message, throwable) =>
        TaskDag.TaskResult.Failure(error = message, diagnostics = throwable.toList.map(BleepBspProtocol.Diagnostic.error))
    }
}
