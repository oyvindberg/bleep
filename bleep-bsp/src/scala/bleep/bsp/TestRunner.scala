package bleep.bsp

import bleep.MachineResources
import bleep.bsp.protocol.KillReason
import bleep.bsp.protocol.{BleepBspProtocol, OutputChannel, ProcessExit, SuiteOutcome, TestStatus}
import bleep.model.{CrossProjectName, SuiteName, TestName}
import bleep.testing.{FrameworkSelection, SessionSharing, TestExecutor, TestProtocol, TestSession, TestSessionRequest}
import cats.effect._
import cats.effect.std.Queue
import cats.syntax.all._

import java.nio.file.Path
import scala.concurrent.duration._

/** Test runner that executes test suites and streams their events back through the DAG event queue.
  *
  * Where a suite runs is [[TestExecutor]]'s business, not this one's: a pooled forked JVM talking over a socket, or a classloader in the server itself. This
  * drives whichever it is handed through the same protocol, with the same idle timeout and the same Deferred-based kill signal — so cancellation, timeouts and
  * reporting behave identically whether or not there is a process on the other end.
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
      workingDirectory: Option[Path],
      /** Shared (the default — this suite's project runs all its suites in one fork) or Exclusive (a fork per suite). Set from the project's `testFork`. */
      sharing: SessionSharing
  )

  object Options {
    val default: Options = Options(
      jvmOptions = Nil,
      defaultHeapMb = MachineResources.DefaultForkHeapMb,
      testArgs = Nil,
      idleTimeout = 2.minutes,
      environment = Map.empty,
      workingDirectory = None,
      sharing = SessionSharing.Exclusive
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
    * @param executor
    *   where to run the suite: a pool of forked JVMs, or this process
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
      executor: TestExecutor,
      eventQueue: Queue[IO, Option[TaskDag.DagEvent]],
      options: Options,
      resolveSourcePath: String => Option[String],
      killSignal: Deferred[IO, KillReason]
  ): IO[TaskDag.TaskResult] = {
    val runnerClass = "bleep.testing.runner.ForkedTestRunner"

    val request = TestSessionRequest(
      label = suiteName,
      classpath = classpath,
      jvmOptions = options.jvmOptions,
      defaultHeapMb = options.defaultHeapMb,
      runnerClass = runnerClass,
      environment = options.environment,
      workingDirectory = options.workingDirectory,
      sharing = options.sharing
    )

    executor.acquire(request).use { jvm =>
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

  /** Run a whole project's JUnit suites as ONE batched execution.
    *
    * The maven one-execute-per-module shape: all the classes go through a single execution in one fork, so an execution-scoped fixture (an application the
    * framework boots for the run) is built once and reused across them instead of rebuilt per class. junit's engine runs `parallelism` classes at once, a
    * number bleep chose. The per-suite events emitted are exactly what a suite-by-suite run emits (each response carries its own suite), so nothing downstream
    * can tell the difference — only the fork does one execute instead of N.
    */
  def runBatch(
      project: CrossProjectName,
      suites: List[(SuiteName, FrameworkSelection)],
      parallelism: Int,
      classpath: List[Path],
      executor: TestExecutor,
      eventQueue: Queue[IO, Option[TaskDag.DagEvent]],
      options: Options,
      resolveSourcePath: String => Option[String],
      killSignal: Deferred[IO, KillReason]
  ): IO[TaskDag.TaskResult] = {
    val runnerClass = "bleep.testing.runner.ForkedTestRunner"
    val classNames = suites.map(_._1.value)
    val selection = suites.head._2 // all JUnit-Platform (the batch is only formed for JUnit)
    val request = TestSessionRequest(
      label = s"${project.value} (batch of ${suites.size})",
      classpath = classpath,
      jvmOptions = options.jvmOptions,
      defaultHeapMb = options.defaultHeapMb,
      runnerClass = runnerClass,
      environment = options.environment,
      workingDirectory = options.workingDirectory,
      // One execute, one fork: exclusive. (A shared session multiplexes independent suites — the opposite model.)
      sharing = SessionSharing.Exclusive
    )
    executor.acquire(request).use { jvm =>
      val startedAt = System.currentTimeMillis()
      IO(BspMetrics.recordSuiteScheduled(jvm.pid, project.value, s"<batch:${suites.size}>", selection.displayName)).attempt >>
        executeBatch(project, classNames, parallelism, selection, jvm, eventQueue, options.idleTimeout, options.testArgs, resolveSourcePath, killSignal)
          .flatTap { result =>
            IO(
              BspMetrics.recordSuiteFinished(
                jvm.pid,
                project.value,
                s"<batch:${suites.size}>",
                System.currentTimeMillis() - startedAt,
                result.getClass.getSimpleName.stripSuffix("$")
              )
            ).attempt
          }
    }
  }

  /** Why a batch fork stopped without reporting the rest of its suites. The fork's own stderr (fd 2) is where a JVM records an OutOfMemoryError, a native
    * crash, or a `System.exit` — none of which travels over the per-suite protocol — so drain it here; say whether the fork is still alive (wedged) or gone;
    * and add a thread dump when it is wedged. Best-effort: every probe is `.attempt`ed, so producing the diagnostic can never itself fail the run.
    */
  private def forkDeathDiagnostic(jvm: TestSession): IO[String] =
    for {
      alive <- jvm.isAlive.attempt.map(_.getOrElse(true))
      stderr <- jvm.drainStderr.attempt.map(_.getOrElse(Nil))
      dump <- if (alive) jvm.dumpThreads.attempt.map(_.getOrElse(Nil)) else IO.pure(Nil)
    } yield {
      val liveNote =
        if (alive) " The fork is still alive — it stopped producing results without exiting, so it is wedged rather than dead."
        else " The fork had exited."
      val errNote =
        if (stderr.nonEmpty) s"\n  fork stderr (tail):\n${stderr.takeRight(50).map("    " + _).mkString("\n")}"
        else " It wrote nothing to its own stderr."
      val dumpNote =
        if (dump.nonEmpty) s"\n  fork thread dump (head):\n${dump.take(80).map("    " + _).mkString("\n")}" else ""
      liveNote + errNote + dumpNote
    }

  private def executeBatch(
      project: CrossProjectName,
      classNames: List[String],
      parallelism: Int,
      selection: FrameworkSelection,
      jvm: TestSession,
      eventQueue: Queue[IO, Option[TaskDag.DagEvent]],
      idleTimeout: FiniteDuration,
      args: List[String],
      resolveSourcePath: String => Option[String],
      killSignal: Deferred[IO, KillReason]
  ): IO[TaskDag.TaskResult] = {
    def now: IO[Long] = IO.realTime.map(_.toMillis)
    def emit(event: TaskDag.DagEvent): IO[Unit] = eventQueue.offer(Some(event))
    val startTime = System.currentTimeMillis()

    for {
      lastActivityAt <- Ref.of[IO, Long](startTime)
      outcomes <- Ref.of[IO, Map[String, SuiteOutcome]](Map.empty)
      failuresPerSuite <- Ref.of[IO, Map[String, List[String]]](Map.empty)
      forkError <- Ref.of[IO, Option[String]](None)

      processResponses =
        jvm
          .runSuites(classNames, parallelism, selection, args)
          .evalMap {
            case TestProtocol.TestResponse.TestStarted(suite, test) =>
              now.flatMap(ts => lastActivityAt.set(ts) >> emit(TaskDag.DagEvent.TestStarted(project, SuiteName(suite), TestName(test), ts)))

            case TestProtocol.TestResponse.TestFinished(suite, test, statusStr, durationMs, message, throwable, location) =>
              val status = TestStatus.fromString(statusStr)
              val track = if (status.isFailure) failuresPerSuite.update(m => m.updated(suite, test :: m.getOrElse(suite, Nil))) else IO.unit
              track >> now.flatMap { ts =>
                lastActivityAt.set(ts) >>
                  emit(
                    TaskDag.DagEvent.TestFinished(
                      project,
                      SuiteName(suite),
                      TestName(test),
                      status,
                      durationMs,
                      message,
                      throwable,
                      ts,
                      location.map(loc => loc.copy(path = resolveSourcePath(loc.declaringClass)))
                    )
                  )
              }

            case TestProtocol.TestResponse.SuiteDone(suite, outcome, durationMs) =>
              outcomes.update(_ + (suite -> outcome)) >>
                now.flatMap(ts => lastActivityAt.set(ts) >> emit(TaskDag.DagEvent.SuiteFinished(project, SuiteName(suite), outcome, durationMs, ts)))

            case TestProtocol.TestResponse.Log(level, message, suite) =>
              if (level == "debug") IO.delay(MultiWorkspaceBspServer.debugLogStatic(s"[${suite.getOrElse(project.value)}] $message"))
              else
                suite match {
                  case Some(s) =>
                    now.flatMap(ts =>
                      emit(TaskDag.DagEvent.Output(project, SuiteName(s), message, OutputChannel.fromIsError(level == "error" || level == "stderr"), ts))
                    )
                  case None =>
                    IO.unit // batch output not attributable to a single suite (a framework thread) — dropped, as its structured events already carried the result
                }

            case TestProtocol.TestResponse.Error(message, details) =>
              // A fork-level error (the JVM died) has no suite — it fails the whole batch. Carry the
              // runner's detail (the "likely System.exit()" hint and the fork's stderr tail) so the
              // batch's failure says why, not just that it died.
              forkError.set(Some(withDetail(message, details)))

            case TestProtocol.TestResponse.BatchComplete => IO.unit
            case TestProtocol.TestResponse.Ready         => IO.unit
            case TestProtocol.TestResponse.ThreadDump(_) => IO.unit
          }
          .compile
          .drain

      idleTimeoutIO = {
        val checkInterval = 1.second
        def loop: IO[Unit] = for {
          nowMs <- IO.realTime.map(_.toMillis)
          lastActivity <- lastActivityAt.get
          elapsed = nowMs - lastActivity
          _ <- if (elapsed >= idleTimeout.toMillis) IO.unit else IO.sleep(checkInterval) >> loop
        } yield ()
        loop
      }

      result <- IO.racePair(processResponses, IO.race(idleTimeoutIO, killSignal.get)).flatMap {
        case Left((_, raceFiber)) =>
          // The batch completed. Aggregate: a fork-level death is an Error; a class that never reported is an Error; otherwise the first failing suite decides,
          // else Success. Per-suite results already went out as SuiteFinished events, so this is only the batch task's own status.
          raceFiber.cancel >> (for {
            fe <- forkError.get
            outs <- outcomes.get
            fps <- failuresPerSuite.get
            result <- fe match {
              case Some(msg) =>
                // The fork reported a death (or bleep synthesised one when its stream ended). Append the fork diagnostic — its own stderr, where a shutdown
                // hook prints the thread dump that names a System.exit caller — so a clean-looking "exited 0" carries the reason with it.
                forkDeathDiagnostic(jvm).flatMap { diag =>
                  val full = s"$msg$diag"
                  IO(System.err.println(s"[bleep] batch fork error for ${project.value}:\n$full")) >>
                    IO.pure(TaskDag.TaskResult.Error(error = full, processExit = ProcessExit.Unknown))
                }
              case None =>
                val missing = classNames.filterNot(outs.contains)
                if (missing.nonEmpty)
                  // The fork stopped after reporting some suites but not these. It almost always died mid-run, and a JVM that dies of an OutOfMemoryError, a
                  // native crash, or `System.exit` says so on its OWN stderr (fd 2) — which the protocol, carrying only per-suite events, never delivered. So
                  // this used to be a dead end: "produced no result", cause unknown. Drain that stderr (and note whether the fork is even still alive) so the
                  // reason travels with the failure.
                  forkDeathDiagnostic(jvm).flatMap { diag =>
                    val msg = s"${missing.size} of ${classNames.size} batched suites never reported a result " +
                      s"(${missing.take(8).mkString(", ")}${if (missing.size > 8) ", …" else ""}).$diag"
                    // The DAG keeps only the errored task's id and drops this message, so emit it as output too — attributed to the first suite that never
                    // reported (the one the fork was on when it went) — so the reason reaches history and the client, not just this returned value.
                    IO(System.err.println(s"[bleep] batch fork diagnostic for ${project.value}:\n$msg")) >>
                      now.flatMap { ts =>
                        msg.linesIterator.toList
                          .traverse_(line =>
                            emit(TaskDag.DagEvent.Output(project, SuiteName(missing.head), line, OutputChannel.fromIsError(isError = true), ts))
                          )
                      } >> IO.pure(TaskDag.TaskResult.Error(error = msg, processExit = ProcessExit.Unknown))
                  }
                else {
                  val perSuite = classNames.map(c => taskResultFor(c, outs(c), fps.getOrElse(c, Nil)))
                  IO.pure(
                    perSuite
                      .collectFirst { case f: TaskDag.TaskResult.Failure => f }
                      .orElse(perSuite.collectFirst { case e: TaskDag.TaskResult.Error => e })
                      .getOrElse(TaskDag.TaskResult.Success)
                  )
                }
            }
          } yield result)

        case Right((suiteFiber, raceOutcome)) =>
          // Idle timeout or kill: the whole fork goes (there is one execute; there is no per-suite thread to interrupt without ending the run).
          val cleanup: IO[Unit] = IO.uncancelable(_ => jvm.kill.attempt >> suiteFiber.cancel.attempt.void)
          raceOutcome match {
            case Outcome.Succeeded(fa) =>
              fa.flatMap {
                case Left(_) =>
                  IO.race(jvm.dumpThreads.attempt, IO.sleep(5.seconds))
                    .map {
                      case Left(Right(lines)) if lines.nonEmpty => Some(lines.mkString("\n"))
                      case _                                    => None
                    }
                    .flatMap(dump => cleanup >> IO.pure(TaskDag.TaskResult.TimedOut(dump)))
                case Right(reason) => cleanup >> IO.pure(TaskDag.TaskResult.Killed(reason))
              }
            case Outcome.Errored(e) =>
              cleanup >> IO.pure(TaskDag.TaskResult.Error(error = s"Error during batch: ${e.getMessage}", processExit = ProcessExit.Unknown))
            case Outcome.Canceled() => cleanup >> IO.pure(TaskDag.TaskResult.Killed(KillReason.UserRequest))
          }
      }
    } yield result
  }

  /** Execute a test suite with idle timeout and kill signal handling.
    *
    * The idle timeout resets each time a test completes. If no test completes within the timeout period, the suite is considered hung and killed.
    */
  private def executeWithIdleTimeout(
      project: CrossProjectName,
      suiteName: String,
      selection: FrameworkSelection,
      jvm: TestSession,
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

              case TestProtocol.TestResponse.Error(message, details) =>
                // Infrastructure error (JVM died mid-stream, or malformed response) — no authoritative
                // SuiteDone. Record it as the terminal signal so we emit SuiteError, not a green suite.
                // Keep the runner's diagnostic detail: JvmPool assembles the "likely System.exit()" hint
                // and the fork's stderr tail here, and dropping them leaves the user with a bare "died
                // unexpectedly" that cannot be acted on.
                terminal.set(Some(Left(withDetail(message, details))))

              case TestProtocol.TestResponse.Ready =>
                IO.unit

              case TestProtocol.TestResponse.ThreadDump(_) =>
                IO.unit

              case TestProtocol.TestResponse.BatchComplete =>
                IO.unit // a single-suite run never batches; only runSuites produces this
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

          // Helper for cleanup - uncancelable and recovers from errors.
          // killSuite, not kill: on an exclusive fork the two are the same (the fork is this suite), but on a per-project shared fork this stops ONLY this
          // suite (a CancelSuite interrupting its thread) and leaves its siblings running. Killing the whole fork here would take a project's other in-flight
          // suites down with a single one's timeout or cancellation.
          def cleanup: IO[Unit] = IO.uncancelable { _ =>
            drainStderrToEvents.attempt >> jvm.killSuite(suiteName).attempt >> suiteFiber.cancel.attempt.void
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
  /** Append the runner's diagnostic detail to a fork-death message, on its own lines. The detail is what makes such a death actionable — the exit-status
    * reading ("likely called System.exit()") and the fork's stderr tail, both assembled in [[bleep.testing.JvmPool]] — and the display renders a multi-line
    * message verbatim, so the user finally sees why the JVM went instead of only that it did.
    */
  private def withDetail(message: String, details: Option[String]): String =
    details.filter(_.trim.nonEmpty).fold(message)(d => s"$message\n$d")

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
