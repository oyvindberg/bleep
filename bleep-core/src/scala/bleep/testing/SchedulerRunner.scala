package bleep.testing

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
    display: TestDisplay,
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
      val cmd = List(job.jvmCommand.toString) ++ allJvmOptions ++ List(
        "-cp",
        cpString,
        "bleep.testing.runner.ForkedTestRunner"
      )

      val pb = new ProcessBuilder(cmd: _*)
      pb.redirectErrorStream(false)
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

    // Collected test events to emit after blocking section
    case class TestEventInfo(test: String, status: TestStatus, durationMs: Long, message: Option[String], throwable: Option[String])

    // Send command and read responses
    val runSuite: IO[SuiteResult] = IO
      .blocking {
        var passed = 0
        var failed = 0
        var skipped = 0
        var ignored = 0
        var failures = List.empty[TestFailureInfo]
        var testEvents = List.empty[TestEventInfo]

        // Send run command
        val cmd = TestProtocol.TestCommand.RunSuite(job.suite.className, job.suite.framework, testArgs)
        handle.stdin.println(TestProtocol.encodeCommand(cmd))
        handle.stdin.flush()

        // Read responses
        var done = false
        while (!done && handle.isAlive) {
          val line = handle.stdout.readLine()
          if (line == null) {
            done = true
          } else {
            TestProtocol.decodeResponse(line) match {
              case Right(TestProtocol.TestResponse.TestStarted(_, _)) => ()
              case Right(TestProtocol.TestResponse.TestFinished(_, test, status, durationMs, message, throwable)) =>
                val testStatus = TestStatus.fromString(status)
                testEvents = testEvents :+ TestEventInfo(test, testStatus, durationMs, message, throwable)
                status match {
                  case "passed" => passed += 1
                  case "failed" =>
                    failed += 1
                    failures = failures :+ TestFailureInfo(TestTypes.TestName(test), message, throwable)
                  case "skipped" => skipped += 1
                  case "ignored" => ignored += 1
                  case _         => ()
                }
              case Right(TestProtocol.TestResponse.SuiteDone(_, p, f, s, i, _)) =>
                passed = p; failed = f; skipped = s; ignored = i
                done = true
              case Right(TestProtocol.TestResponse.Error(msg, details)) =>
                failed += 1
                failures = failures :+ TestFailureInfo(TestTypes.TestName("(error)"), Some(msg), details)
                done = true
              case _ => ()
            }
          }
        }

        val endTime = System.currentTimeMillis()
        (
          SuiteResult(
            suite = TestTypes.SuiteClassName(job.suite.className),
            passed = passed,
            failed = failed,
            skipped = skipped,
            ignored = ignored,
            durationMs = endTime - startTime,
            failures = failures
          ),
          testEvents
        )
      }
      .flatMap { case (result, testEvents) =>
        // Emit test events to display for speedup calculation
        val emitEvents = testEvents.traverse_ { info =>
          display.handle(
            TestEvent.TestFinished(
              job.project.value,
              job.suite.className,
              info.test,
              info.status,
              info.durationMs,
              info.message,
              info.throwable,
              System.currentTimeMillis()
            )
          )
        }
        emitEvents.as(result)
      }

    // Complete suite and update state
    def completeSuite(result: SuiteResult): IO[SuiteResult] = for {
      _ <- eventQueue.offer(SchedulerEvent.SuiteFinished(jvmId, result))
      _ <- updateTestRunStateForSuiteFinish(job, result)
    } yield result

    // Handle errors
    def handleError(error: Throwable): IO[SuiteResult] = {
      val failureResult = SuiteResult(
        suite = TestTypes.SuiteClassName(job.suite.className),
        passed = 0,
        failed = 1,
        skipped = 0,
        ignored = 0,
        durationMs = System.currentTimeMillis() - startTime,
        failures = List(TestFailureInfo(TestTypes.TestName("(error)"), Some(error.getMessage), None))
      )
      eventQueue.offer(SchedulerEvent.JvmDied(jvmId, Some(error.getMessage))) >>
        jvmsRef.update(_ - jvmId) >>
        IO(handle.kill()) >>
        IO.pure(failureResult)
    }

    notifyStart >> runSuite.flatMap(completeSuite).handleErrorWith(handleError)
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
      TestEvent.SuiteTimedOut(job.project.value, job.suite.className, timeoutConfig.suiteExecutionMs, threadDump, timestamp)
    ) >> updateTestRunStateForTimeout(job)
  }

  private def updateTestRunStateForSuiteStart(job: SuiteJob, jvmPid: Long, timestamp: Long): IO[Unit] = {
    val action = ProjectAction.StartSuite(
      job.project,
      TestTypes.SuiteClassName(job.suite.className),
      jvmPid,
      timestamp
    )
    testRunState.update(_.reduceSimple(action)) >>
      display.handle(TestEvent.SuiteStarted(job.project.value, job.suite.className, timestamp))
  }

  private def updateTestRunStateForSuiteFinish(job: SuiteJob, result: SuiteResult): IO[Unit] = {
    val action = ProjectAction.FinishSuite(
      job.project,
      TestTypes.SuiteClassName(job.suite.className),
      result
    )
    testRunState.update(_.reduceSimple(action)) >>
      display.handle(
        TestEvent.SuiteFinished(
          job.project.value,
          job.suite.className,
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
      suite = TestTypes.SuiteClassName(job.suite.className),
      passed = 0,
      failed = 1,
      skipped = 0,
      ignored = 0,
      durationMs = 0,
      failures = List(TestFailureInfo(TestTypes.TestName("(timeout)"), Some("Suite timed out"), None))
    )
    val action = ProjectAction.FinishSuite(
      job.project,
      TestTypes.SuiteClassName(job.suite.className),
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
      result <- tickLoop(eventQueue, stateRef, jvmsRef, fibersRef, cancelSignal, tickIntervalMs)
      // Cleanup: cancel all running fibers and kill JVMs
      fibers <- fibersRef.get
      _ <- fibers.toList.traverse_(_.cancel)
      jvms <- jvmsRef.get
      _ <- IO(jvms.values.foreach(jvm => if (jvm.isAlive) jvm.kill()))
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
          IO(System.err.println(s"Action ${action.name} failed: ${error.getMessage}"))
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
      display: TestDisplay,
      testRunState: Ref[IO, TestRunState],
      testArgs: List[String],
      jvmOptions: List[String],
      timeoutConfig: TimeoutConfig
  ): SchedulerInterpreter =
    new SchedulerInterpreter(display, testRunState, testArgs, jvmOptions, timeoutConfig)
}
