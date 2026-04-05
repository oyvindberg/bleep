package bleep.testing

import bleep.bsp.protocol.{OutputChannel, TestStatus}
import bleep.model.{SuiteName, TestName}
import cats.effect._
import cats.effect.std.Queue
import cats.syntax.all._

import java.io._
import scala.concurrent.duration._
import scala.util.control.NonFatal

/** Interpreter that executes scheduler actions (GADT-based)
  *
  * The interpreter recursively interprets the action tree:
  *   - SpawnJvm: Start JVM, wait for Ready, return JvmId
  *   - RunSuite: Get JvmId from sub-action, run suite
  *   - FlatMap: Interpret first, apply function, interpret result
  *   - Pure: Return value directly
  */
class SchedulerInterpreter(
    display: BuildDisplay,
    testRunState: Ref[IO, TestRunState],
    testArgs: List[String],
    jvmOptions: List[String],
    timeoutConfig: TimeoutConfig
) {

  /** Handle to a running JVM */
  case class JvmHandle(
      jvmId: JvmId,
      process: Process,
      stdin: PrintWriter,
      stdout: BufferedReader,
      stderr: BufferedReader
  ) {
    def pid: Long = process.pid()
    def isAlive: Boolean = process.isAlive
    def kill(): Unit = {
      try stdin.close()
      catch { case NonFatal(_) => }
      process.destroyForcibly()
    }
  }

  /** Interpret and execute an action, returning its result */
  def interpret[A](
      action: SchedulerAction[A],
      eventQueue: Queue[IO, SchedulerEvent],
      jvmsRef: Ref[IO, Map[JvmId, JvmHandle]]
  ): IO[A] = action match {

    case SchedulerAction.Pure(value) =>
      IO.pure(value)

    case SchedulerAction.SpawnJvm(job) =>
      spawnJvm(job, eventQueue, jvmsRef)

    case SchedulerAction.RunSuite(jvmId, suite) =>
      runSuiteOnJvm(jvmId, suite, eventQueue, jvmsRef)

    case SchedulerAction.GetThreadDump(jvmId, _) =>
      getThreadDump(jvmId, eventQueue, jvmsRef)

    case SchedulerAction.KillJvm(jvmId, _) =>
      killJvm(jvmId, eventQueue, jvmsRef)

    case SchedulerAction.NotifyTimeout(jvmId, job, reason, threadDump) =>
      notifyTimeout(jvmId, job, reason, threadDump)

    case SchedulerAction.FlatMap(first, f) =>
      for {
        a <- interpret(first, eventQueue, jvmsRef)
        b <- interpret(f(a), eventQueue, jvmsRef)
      } yield b
  }

  /** Spawn a new JVM and return its ID (PID) once ready */
  private def spawnJvm(
      job: SuiteJob,
      eventQueue: Queue[IO, SchedulerEvent],
      jvmsRef: Ref[IO, Map[JvmId, JvmHandle]]
  ): IO[JvmId] =
    IO.blocking {
      val cpString = job.classpath.map(_.toString).mkString(java.io.File.pathSeparator)
      // Include scala3CompatOptions to suppress sun.misc.Unsafe warnings
      val allJvmOptions = bleep.internal.jvmRunCommand.scala3CompatOptions ++ jvmOptions

      // On Windows, command-line length is limited to 32,767 characters.
      // When the classpath is too long, pass it via CLASSPATH environment variable instead.
      val useEnvClasspath = scala.util.Properties.isWin && cpString.length > 30000

      val cmd =
        if (useEnvClasspath)
          List(job.jvmCommand.toString) ++ allJvmOptions ++ List("bleep.testing.runner.ForkedTestRunner")
        else
          List(job.jvmCommand.toString) ++ allJvmOptions ++ List("-cp", cpString, "bleep.testing.runner.ForkedTestRunner")

      val pb = new ProcessBuilder(cmd: _*)
      pb.redirectErrorStream(false)
      if (useEnvClasspath) {
        pb.environment().put("CLASSPATH", cpString)
      }
      val process = pb.start()
      val stdin = new PrintWriter(new BufferedOutputStream(process.getOutputStream), true)
      val stdout = new BufferedReader(new InputStreamReader(process.getInputStream))
      val stderr = new BufferedReader(new InputStreamReader(process.getErrorStream))

      // JvmId is the process PID
      val jvmId = JvmId(process.pid())
      JvmHandle(jvmId, process, stdin, stdout, stderr)
    }.flatMap { handle =>
      // Wait for Ready
      val waitForReady: IO[Unit] = IO.blocking {
        val line = handle.stdout.readLine()
        if (line == null) {
          throw new IOException("JVM terminated before sending Ready")
        }
        TestProtocol.decodeResponse(line) match {
          case Right(TestProtocol.TestResponse.Ready) => ()
          case Right(other)                           => throw new IOException(s"Expected Ready, got: $other")
          case Left(err)                              => throw new IOException(s"Failed to decode: $err")
        }
      }

      waitForReady
        .flatTap(_ => jvmsRef.update(_ + (handle.jvmId -> handle)))
        .as(handle.jvmId)
        .handleErrorWith { error =>
          IO(handle.kill()) >>
            eventQueue.offer(SchedulerEvent.JvmDied(handle.jvmId, Some(error.getMessage))) >>
            IO.raiseError(error)
        }
    }

  /** Run a suite on an existing JVM */
  private def runSuiteOnJvm(
      jvmId: JvmId,
      job: SuiteJob,
      eventQueue: Queue[IO, SchedulerEvent],
      jvmsRef: Ref[IO, Map[JvmId, JvmHandle]]
  ): IO[SuiteResult] =
    for {
      jvms <- jvmsRef.get
      handle <- IO.fromOption(jvms.get(jvmId))(new IOException(s"JVM $jvmId not found"))
      result <- runSuiteOnHandle(jvmId, handle, job, eventQueue, jvmsRef)
    } yield result

  private def runSuiteOnHandle(
      jvmId: JvmId,
      handle: JvmHandle,
      job: SuiteJob,
      eventQueue: Queue[IO, SchedulerEvent],
      jvmsRef: Ref[IO, Map[JvmId, JvmHandle]]
  ): IO[SuiteResult] = {
    val startTime = System.currentTimeMillis()

    // Notify scheduler and display that suite is starting
    val notifyStart: IO[Unit] = for {
      timestamp <- IO.realTime.map(_.toMillis)
      _ <- eventQueue.offer(SchedulerEvent.JvmStartedSuite(jvmId, job, timestamp))
      _ <- updateTestRunStateForSuiteStart(job, jvmId.pid, timestamp)
    } yield ()

    // Send command, then read responses one at a time so we can emit
    // TestActivity events to the scheduler between reads (resets idle timeout).
    val runSuite: IO[SuiteResult] = {
      case class RunState(
          passed: Int,
          failed: Int,
          skipped: Int,
          ignored: Int,
          failures: List[TestFailureInfo],
          done: Boolean
      )

      val sendCommand: IO[Unit] = IO.blocking {
        val cmd = TestProtocol.TestCommand.RunSuite(job.suite.className, job.suite.framework, testArgs)
        handle.stdin.println(TestProtocol.encodeCommand(cmd))
        handle.stdin.flush()
      }

      val readLine: IO[Option[String]] = IO.blocking {
        if (!handle.isAlive) None
        else Option(handle.stdout.readLine())
      }

      def processLoop(state: RunState): IO[RunState] =
        if (state.done) IO.pure(state)
        else
          readLine.flatMap {
            case None       => IO.pure(state.copy(done = true))
            case Some(line) =>
              TestProtocol.decodeResponse(line) match {
                case Right(TestProtocol.TestResponse.TestStarted(_, _)) =>
                  processLoop(state)

                case Right(TestProtocol.TestResponse.TestFinished(_, test, status, durationMs, message, throwable)) =>
                  val testStatus = TestStatus.fromString(status)
                  val newState = testStatus match {
                    case TestStatus.Passed => state.copy(passed = state.passed + 1)
                    case s if s.isFailure  =>
                      state.copy(
                        failed = state.failed + 1,
                        failures = state.failures :+ TestFailureInfo(TestName(test), message, throwable)
                      )
                    case TestStatus.Skipped | TestStatus.AssumptionFailed => state.copy(skipped = state.skipped + 1)
                    case TestStatus.Ignored | TestStatus.Pending          => state.copy(ignored = state.ignored + 1)
                    case _                                                => state
                  }
                  // Emit TestActivity to scheduler (resets idle timeout) and display event
                  for {
                    ts <- IO.realTime.map(_.toMillis)
                    _ <- eventQueue.offer(SchedulerEvent.TestActivity(jvmId, ts))
                    _ <- display.handle(
                      BuildEvent.TestFinished(
                        job.project,
                        SuiteName(job.suite.className),
                        TestName(test),
                        testStatus,
                        durationMs,
                        message,
                        throwable,
                        ts
                      )
                    )
                    result <- processLoop(newState)
                  } yield result

                case Right(TestProtocol.TestResponse.SuiteDone(_, p, f, s, i, _)) =>
                  IO.pure(state.copy(passed = p, failed = f, skipped = s, ignored = i, done = true))

                case Right(TestProtocol.TestResponse.Error(msg, details)) =>
                  IO.pure(
                    state.copy(
                      failed = state.failed + 1,
                      failures = state.failures :+ TestFailureInfo(TestName("(error)"), Some(msg), details),
                      done = true
                    )
                  )

                case Right(TestProtocol.TestResponse.Log(level, message, suite)) =>
                  val channel = if (level == "error" || level == "stderr") OutputChannel.Stderr else OutputChannel.Stdout
                  val effectiveSuite = suite.getOrElse(job.suite.className)
                  for {
                    ts <- IO.realTime.map(_.toMillis)
                    _ <- display.handle(BuildEvent.Output(job.project, SuiteName(effectiveSuite), message, channel, ts))
                    result <- processLoop(state)
                  } yield result

                case _ => processLoop(state)
              }
          }

      sendCommand >> processLoop(RunState(0, 0, 0, 0, Nil, false)).map { state =>
        val endTime = System.currentTimeMillis()
        SuiteResult(
          suite = SuiteName(job.suite.className),
          passed = state.passed,
          failed = state.failed,
          skipped = state.skipped,
          ignored = state.ignored,
          durationMs = endTime - startTime,
          failures = state.failures
        )
      }
    }

    // Complete suite and update state
    def completeSuite(result: SuiteResult): IO[SuiteResult] = for {
      _ <- eventQueue.offer(SchedulerEvent.SuiteFinished(jvmId, result))
      _ <- updateTestRunStateForSuiteFinish(job, result)
    } yield result

    // Drain stderr from JVM and emit as Output events
    def drainStderr: IO[Unit] = IO
      .blocking {
        val sb = new StringBuilder
        try
          while (handle.stderr.ready()) {
            val line = handle.stderr.readLine()
            if (line != null) sb.append(line).append("\n")
          }
        catch { case NonFatal(_) => }
        sb.toString()
      }
      .flatMap { stderrOutput =>
        if (stderrOutput.nonEmpty) {
          IO.realTime.map(_.toMillis).flatMap { ts =>
            stderrOutput.split('\n').toList.traverse_ { line =>
              display.handle(BuildEvent.Output(job.project, SuiteName(job.suite.className), line, OutputChannel.Stderr, ts))
            }
          }
        } else IO.unit
      }

    // Handle errors
    def handleError(error: Throwable): IO[SuiteResult] = {
      val failureResult = SuiteResult(
        suite = SuiteName(job.suite.className),
        passed = 0,
        failed = 1,
        skipped = 0,
        ignored = 0,
        durationMs = System.currentTimeMillis() - startTime,
        failures = List(TestFailureInfo(TestName("(error)"), Some(error.getMessage), None))
      )
      drainStderr >>
        eventQueue.offer(SchedulerEvent.JvmDied(jvmId, Some(error.getMessage))) >>
        jvmsRef.update(_ - jvmId) >>
        IO(handle.kill()) >>
        IO.pure(failureResult)
    }

    notifyStart >> runSuite.flatTap(_ => drainStderr).flatMap(completeSuite).handleErrorWith(handleError)
  }

  /** Get thread dump from a JVM and send event to scheduler */
  private def getThreadDump(
      jvmId: JvmId,
      eventQueue: Queue[IO, SchedulerEvent],
      jvmsRef: Ref[IO, Map[JvmId, JvmHandle]]
  ): IO[Option[ThreadDumpInfo]] = for {
    jvms <- jvmsRef.get
    dump <- jvms.get(jvmId) match {
      case Some(handle) =>
        IO.blocking {
          // Send GetThreadDump command
          handle.stdin.println(TestProtocol.encodeCommand(TestProtocol.TestCommand.GetThreadDump))
          handle.stdin.flush()
          // Read response with timeout
          val line = handle.stdout.readLine()
          if (line == null) {
            None
          } else {
            TestProtocol.decodeResponse(line) match {
              case Right(TestProtocol.TestResponse.ThreadDump(threads)) =>
                // Convert to ThreadDumpInfo
                val activeThreads = threads.filterNot(t => t.state == "WAITING" || t.state == "TIMED_WAITING" || t.state == "BLOCKED")
                val activeCount = activeThreads.size
                val singleStack = if (activeCount == 1) {
                  Some(activeThreads.head.stackTrace.take(20).mkString("\n"))
                } else {
                  None
                }
                Some(ThreadDumpInfo(activeCount, singleStack, None))
              case _ => None
            }
          }
        }.timeout(5.seconds)
          .handleError(_ => None)
      case None => IO.pure(None)
    }
    _ <- eventQueue.offer(SchedulerEvent.ThreadDumpReceived(jvmId, dump))
  } yield dump

  private def killJvm(
      jvmId: JvmId,
      eventQueue: Queue[IO, SchedulerEvent],
      jvmsRef: Ref[IO, Map[JvmId, JvmHandle]]
  ): IO[Unit] = for {
    jvms <- jvmsRef.get
    _ <- jvms.get(jvmId) match {
      case Some(handle) =>
        IO(handle.kill()) >>
          jvmsRef.update(_ - jvmId) >>
          eventQueue.offer(SchedulerEvent.JvmKilled(jvmId))
      case None => IO.unit
    }
  } yield ()

  private def notifyTimeout(jvmId: JvmId, job: SuiteJob, reason: String, threadDump: Option[ThreadDumpInfo]): IO[Unit] = {
    val timestamp = System.currentTimeMillis()
    display.handle(
      BuildEvent.SuiteTimedOut(job.project, SuiteName(job.suite.className), timeoutConfig.idleTimeoutMs, threadDump, timestamp)
    ) >> updateTestRunStateForTimeout(job)
  }

  private def updateTestRunStateForSuiteStart(job: SuiteJob, jvmPid: Long, timestamp: Long): IO[Unit] = {
    val action = ProjectAction.StartSuite(
      job.project,
      SuiteName(job.suite.className),
      jvmPid,
      timestamp
    )
    testRunState.update(_.reduceSimple(action)) >>
      display.handle(BuildEvent.SuiteStarted(job.project, SuiteName(job.suite.className), timestamp))
  }

  private def updateTestRunStateForSuiteFinish(job: SuiteJob, result: SuiteResult): IO[Unit] = {
    val action = ProjectAction.FinishSuite(
      job.project,
      SuiteName(job.suite.className),
      result
    )
    testRunState.update(_.reduceSimple(action)) >>
      display.handle(
        BuildEvent.SuiteFinished(
          job.project,
          SuiteName(job.suite.className),
          result.passed,
          result.failed,
          result.skipped,
          result.ignored,
          result.durationMs,
          System.currentTimeMillis()
        )
      )
  }

  private def updateTestRunStateForTimeout(job: SuiteJob): IO[Unit] = {
    val failureResult = SuiteResult(
      suite = SuiteName(job.suite.className),
      passed = 0,
      failed = 1,
      skipped = 0,
      ignored = 0,
      durationMs = 0,
      failures = List(TestFailureInfo(TestName("(timeout)"), Some("Suite idle timeout"), None))
    )
    val action = ProjectAction.FinishSuite(
      job.project,
      SuiteName(job.suite.className),
      failureResult
    )
    testRunState.update(_.reduceSimple(action))
  }

  /** Run the scheduler loop */
  def run(
      eventQueue: Queue[IO, SchedulerEvent],
      stateRef: Ref[IO, SuiteSchedulerState],
      cancelSignal: Deferred[IO, Unit],
      tickIntervalMs: Int
  ): IO[SuiteSchedulerState] =
    for {
      jvmsRef <- Ref.of[IO, Map[JvmId, JvmHandle]](Map.empty)
      fibersRef <- Ref.of[IO, Set[Fiber[IO, Throwable, Unit]]](Set.empty)
      // Use guaranteeCase to ensure cleanup happens even on exceptions or cancellation
      result <- tickLoop(eventQueue, stateRef, jvmsRef, fibersRef, cancelSignal, tickIntervalMs)
        .guaranteeCase { _ =>
          // Cleanup: cancel all running fibers and kill JVMs
          for {
            fibers <- fibersRef.get
            _ <- fibers.toList.traverse_(_.cancel)
            jvms <- jvmsRef.get
            _ <- IO(jvms.values.foreach(jvm => if (jvm.isAlive) jvm.kill()))
          } yield ()
        }
    } yield result

  private def tickLoop(
      eventQueue: Queue[IO, SchedulerEvent],
      stateRef: Ref[IO, SuiteSchedulerState],
      jvmsRef: Ref[IO, Map[JvmId, JvmHandle]],
      fibersRef: Ref[IO, Set[Fiber[IO, Throwable, Unit]]],
      cancelSignal: Deferred[IO, Unit],
      tickIntervalMs: Int
  ): IO[SuiteSchedulerState] = {
    val tickInterval = tickIntervalMs.millis

    def drainQueue: IO[List[SchedulerEvent]] = {
      def drain(acc: List[SchedulerEvent]): IO[List[SchedulerEvent]] =
        eventQueue.tryTake.flatMap {
          case Some(event) => drain(event :: acc)
          case None        => IO.pure(acc.reverse)
        }
      drain(Nil)
    }

    def loop: IO[SuiteSchedulerState] = for {
      events <- drainQueue
      nowMs <- IO.realTime.map(_.toMillis)

      // Run tick
      actions <- stateRef.modify { state =>
        val result = SuiteScheduler.tick(state, events, nowMs)
        (result.state, result.actions)
      }

      // Fork all actions and track fibers for cleanup
      newFibers <- actions.traverse { action =>
        interpret(action, eventQueue, jvmsRef).void.handleErrorWith { error =>
          // Report error through display (safe for TUI)
          display.handle(BuildEvent.Error(s"Action ${action.name} failed", Some(error.getMessage), System.currentTimeMillis()))
        }.start
      }
      _ <- fibersRef.update(_ ++ newFibers.toSet)

      // Clean up completed fibers periodically to avoid memory leak
      _ <- fibersRef.update { fibers =>
        // This is a simplification - ideally we'd poll fiber status
        // For now, keep all fibers (they'll be cleaned up at the end)
        fibers
      }

      // Check completion
      state <- stateRef.get
      result <-
        if (state.isComplete) {
          IO.pure(state)
        } else {
          IO.race(cancelSignal.get, IO.sleep(tickInterval)).flatMap {
            case Left(())  => stateRef.get
            case Right(()) => loop
          }
        }
    } yield result

    loop
  }
}

object SchedulerInterpreter {
  def create(
      display: BuildDisplay,
      testRunState: Ref[IO, TestRunState],
      testArgs: List[String],
      jvmOptions: List[String],
      timeoutConfig: TimeoutConfig
  ): SchedulerInterpreter =
    new SchedulerInterpreter(display, testRunState, testArgs, jvmOptions, timeoutConfig)
}
