package bleep.testing

import bleep.{model, Started}
import bleep.model.CrossProjectName
import bloop.rifle.BuildServer
import cats.effect._
import cats.effect.std.Console
import cats.syntax.all._
import ch.epfl.scala.bsp4j
import coursier._

import java.nio.file.{Files, Path, Paths}
import scala.concurrent.duration._
import scala.jdk.CollectionConverters._

/** Reactive test runner that starts tests as soon as their dependencies compile.
  *
  * Key features:
  *   - Runs tests in parallel as soon as they're ready
  *   - Reuses forked JVMs for performance
  *   - Streams real-time progress to display
  *   - Supports cancellation via interrupt
  */
object ReactiveTestRunner {

  /** Options for the test runner */
  case class Options(
      jvmOptions: List[String],
      testArgs: List[String],
      maxParallelJvms: Int,
      quietMode: Boolean,
      suiteTimeout: FiniteDuration
  )

  object Options {
    val default: Options = Options(
      jvmOptions = Nil,
      testArgs = Nil,
      maxParallelJvms = Runtime.getRuntime.availableProcessors(),
      quietMode = false,
      suiteTimeout = 2.minutes
    )
  }

  /** Result of running tests */
  case class Result(
      passed: Int,
      failed: Int,
      skipped: Int,
      ignored: Int,
      failedSuites: List[String],
      durationMs: Long
  ) {
    def success: Boolean = failed == 0
  }

  /** Run tests for the given projects.
    *
    * @param started
    *   The bleep Started context
    * @param bloop
    *   The BSP build server
    * @param testProjects
    *   Projects to test (must be test projects)
    * @param options
    *   Test runner options
    * @return
    *   Test results
    */
  def run(
      started: Started,
      bloop: BuildServer,
      testProjects: Set[CrossProjectName],
      options: Options
  ): IO[Result] = {
    val console = Console.make[IO]

    JvmPool.create(options.maxParallelJvms, started.jvmCommand).use { pool =>
      for {
        display <- TestDisplay.create(options.quietMode, console)

        // Build target lookup
        buildTargetFor = (p: CrossProjectName) =>
          new bsp4j.BuildTargetIdentifier(
            started.projectPaths(p).targetDir.toUri.toASCIIString
          )

        // Discover all test suites
        suites <- IO(
          TestDiscovery.discoverAll(
            bloop,
            testProjects,
            buildTargetFor
          )
        )

        _ <- console.println(s"Discovered ${suites.size} test suites in ${testProjects.size} projects")

        // Group suites by project for classpath sharing
        suitesByProject = suites.groupBy(_.project)

        // Run all suites in parallel, bounded by JVM pool
        result <- {
          val runSuites = suitesByProject.toList.parTraverse { case (project, projectSuites) =>
            runProjectSuites(
              started = started,
              project = project,
              suites = projectSuites,
              pool = pool,
              display = display,
              options = options
            )
          }

          runSuites.flatMap { results =>
            for {
              _ <- display.printSummary
              summary <- display.summary
            } yield Result(
              passed = summary.testsPassed,
              failed = summary.testsFailed,
              skipped = summary.testsSkipped,
              ignored = summary.testsIgnored,
              failedSuites = summary.failures.map(_.suite).distinct,
              durationMs = summary.durationMs
            )
          }
        }
      } yield result
    }
  }

  private def runProjectSuites(
      started: Started,
      project: CrossProjectName,
      suites: List[DiscoveredSuite],
      pool: JvmPool,
      display: TestDisplay,
      options: Options
  ): IO[Unit] = {
    // Get classpath for this project
    val classpath = getClasspath(started, project)

    // The ForkedTestRunner class must be on the classpath
    // It's in the bleep-test-runner project which has minimal dependencies
    val runnerClass = "bleep.testing.runner.ForkedTestRunner"

    pool.acquire(classpath, options.jvmOptions, runnerClass).use { jvm =>
      suites.traverse_ { suite =>
        runSuite(
          projectName = project.value,
          suite = suite,
          jvm = jvm,
          display = display,
          testArgs = options.testArgs,
          timeout = options.suiteTimeout
        )
      }
    }
  }

  /** Result of processing test responses */
  private case class SuiteResult(
      suiteFinishedSent: Boolean,
      passed: Int,
      failed: Int,
      skipped: Int,
      ignored: Int
  )

  /** Run a single test suite on the given JVM */
  def runSuite(
      projectName: String,
      suite: DiscoveredSuite,
      jvm: TestJvm,
      display: TestDisplay,
      testArgs: List[String],
      timeout: FiniteDuration
  ): IO[Unit] = {

    def debugLog(msg: String): Unit = {
      import java.nio.file.StandardOpenOption
      val line = s"[RUNNER] $msg\n"
      try Files.write(Paths.get("/tmp/bleep-test-reactive.log"), line.getBytes, StandardOpenOption.APPEND)
      catch { case _: Exception => () }
    }

    // Track test counts and whether SuiteFinished was sent
    def processResponses: IO[SuiteResult] = for {
      responses <- jvm.runSuite(suite.className, suite.framework, testArgs).compile.toList
      _ <- IO(debugLog(s"Suite: ${suite.className}, framework: ${suite.framework}, responses: ${responses.size}"))
      _ <- IO(responses.foreach(r => debugLog(s"  Response: $r")))
      suiteFinishedSent <- Ref.of[IO, Boolean](false)
      passedCount <- Ref.of[IO, Int](0)
      failedCount <- Ref.of[IO, Int](0)
      skippedCount <- Ref.of[IO, Int](0)
      ignoredCount <- Ref.of[IO, Int](0)

      // Convert responses to events
      _ <- responses.traverse_ {
        case TestProtocol.TestResponse.TestStarted(_, test) =>
          IO.realTime.map(_.toMillis).flatMap(now => display.handle(TestEvent.TestStarted(projectName, suite.className, test, now)))

        case TestProtocol.TestResponse.TestFinished(_, test, status, durationMs, message, throwable) =>
          val testStatus = TestStatus.fromString(status)
          val updateCount = testStatus match {
            case TestStatus.Passed                                         => passedCount.update(_ + 1)
            case TestStatus.Failed | TestStatus.Error | TestStatus.Timeout => failedCount.update(_ + 1)
            case TestStatus.Skipped | TestStatus.Cancelled                 => skippedCount.update(_ + 1)
            case TestStatus.Ignored | TestStatus.Pending                   => ignoredCount.update(_ + 1)
          }
          updateCount >> IO.realTime
            .map(_.toMillis)
            .flatMap(now =>
              display.handle(
                TestEvent.TestFinished(projectName, suite.className, test, testStatus, durationMs, message, throwable, now)
              )
            )

        case TestProtocol.TestResponse.SuiteDone(_, passed, failed, skipped, ignored, durationMs) =>
          IO.realTime
            .map(_.toMillis)
            .flatMap(now =>
              display.handle(
                TestEvent.SuiteFinished(projectName, suite.className, passed, failed, skipped, ignored, durationMs, now)
              )
            ) >> suiteFinishedSent.set(true)

        case TestProtocol.TestResponse.Log(level, message) =>
          val isError = level == "error" || level == "warn"
          IO.realTime.map(_.toMillis).flatMap(now => display.handle(TestEvent.Output(projectName, suite.className, s"[$level] $message", isError, now)))

        case TestProtocol.TestResponse.Error(message, throwable) =>
          IO.realTime
            .map(_.toMillis)
            .flatMap(now =>
              display.handle(TestEvent.Output(projectName, suite.className, s"[ERROR] $message", true, now)) >>
                throwable.traverse_(t => display.handle(TestEvent.Output(projectName, suite.className, t, true, now)))
            )

        case TestProtocol.TestResponse.Ready =>
          IO.unit // Ignore Ready messages during suite execution

        case TestProtocol.TestResponse.ThreadDump(_) =>
          IO.unit // Ignore ThreadDump during normal suite execution (used for timeout handling)
      }
      sent <- suiteFinishedSent.get
      passed <- passedCount.get
      failed <- failedCount.get
      skipped <- skippedCount.get
      ignored <- ignoredCount.get
    } yield SuiteResult(sent, passed, failed, skipped, ignored)

    val suiteKey = s"$projectName/${suite.className}"

    def handleTimeout: IO[Unit] = for {
      _ <- IO(debugLog(s"TIMEOUT TRIGGERED for suite: $suiteKey"))
      endTime <- IO.realTime.map(_.toMillis)
      _ <- IO(debugLog(s"Getting thread dump for $suiteKey (3s timeout)..."))
      // Try to get thread dump with 3 second timeout using racePair
      // Can't use .timeoutTo because blocking IO can't be interrupted
      threadDumpResult <- IO.racePair(jvm.getThreadDump, IO.sleep(3.seconds))
      threadDump <- threadDumpResult match {
        case Left((outcome, timerFiber)) =>
          // Thread dump completed - cancel timer and get result
          timerFiber.cancel >> outcome.embedError.map { dump =>
            IO(debugLog(s"Thread dump received for $suiteKey: ${dump.map(d => s"${d.threads.size} threads").getOrElse("None")}"))
            dump
          }
        case Right((dumpFiber, _)) =>
          // Timer won - abandon thread dump attempt (don't wait for it)
          IO(debugLog(s"Thread dump timed out for $suiteKey, proceeding to kill JVM")) >>
            IO.pure(None)
        // Note: we intentionally don't cancel dumpFiber here because it's blocked on readLine
        // and can't be cancelled anyway. It will terminate when we kill the JVM.
      }
      threadDumpInfo <- IO(processThreadDump(projectName, suite.className, threadDump))
      _ <- IO(debugLog(s"Killing JVM for $suiteKey..."))
      _ <- jvm.kill
      _ <- IO(debugLog(s"JVM killed for $suiteKey, sending SuiteTimedOut event"))
      _ <- display.handle(
        TestEvent.SuiteTimedOut(projectName, suite.className, timeout.toMillis, threadDumpInfo, endTime)
      )
      _ <- IO(debugLog(s"TIMEOUT COMPLETE for $suiteKey"))
    } yield ()

    for {
      startTime <- IO.realTime.map(_.toMillis)
      _ <- display.handle(TestEvent.SuiteStarted(projectName, suite.className, startTime))
      _ <- IO(debugLog(s"Starting race: processResponses vs sleep(${timeout.toSeconds}s) for $suiteKey"))
      // Use racePair instead of race - race waits for cancellation of the loser,
      // but blocking IO (readLine) can't be cancelled, so race would hang.
      // racePair returns immediately when one side wins, giving us the fiber to cancel later.
      raceResult <- IO.racePair(
        processResponses,
        IO.sleep(timeout).flatTap(_ => IO(debugLog(s"Sleep timer finished for $suiteKey")))
      )
      _ <- IO(debugLog(s"RacePair completed for $suiteKey"))
      endTime <- IO.realTime.map(_.toMillis)
      _ <- raceResult match {
        case Left((outcome, timerFiber)) =>
          // processResponses won - cancel the timer (instant) and process result
          timerFiber.cancel >> outcome.embedError.flatMap { result =>
            IO(debugLog(s"Normal completion for $suiteKey: sent=${result.suiteFinishedSent}")) >>
              (if (!result.suiteFinishedSent) {
                 display.handle(
                   TestEvent
                     .SuiteFinished(projectName, suite.className, result.passed, result.failed, result.skipped, result.ignored, endTime - startTime, endTime)
                 )
               } else IO.unit)
          }
        case Right((responseFiber, _)) =>
          // Timeout won - kill JVM first (which will unblock readLine), then cancel fiber
          IO(debugLog(s"Timeout won for $suiteKey, handling...")) >>
            handleTimeout >>
            responseFiber.cancel // This should now complete quickly since JVM is dead
      }
    } yield ()
  }

  /** Process thread dump and create ThreadDumpInfo */
  private def processThreadDump(
      projectName: String,
      suiteName: String,
      threadDump: Option[TestProtocol.TestResponse.ThreadDump]
  ): Option[ThreadDumpInfo] =
    threadDump.map { dump =>
      // Filter for "active" threads (not waiting/timed_waiting on common things)
      val activeThreads = dump.threads.filter { t =>
        val state = t.state
        // Consider RUNNABLE threads and BLOCKED threads as active
        // Exclude WAITING and TIMED_WAITING on common patterns
        state == "RUNNABLE" || state == "BLOCKED" || {
          // Include WAITING/TIMED_WAITING only if not on common wait patterns
          val firstFrame = t.stackTrace.headOption.getOrElse("")
          !firstFrame.contains("Object.wait") &&
          !firstFrame.contains("LockSupport.park") &&
          !firstFrame.contains("Thread.sleep") &&
          !firstFrame.contains("SocketInputStream.read") &&
          !firstFrame.contains("BufferedInputStream.read")
        }
      }

      if (activeThreads.size == 1) {
        // Single active thread - show inline
        val thread = activeThreads.head
        val stackPreview = thread.stackTrace.take(15).mkString("\n  at ")
        val preview = s"Thread: ${thread.name} (${thread.state})\n  at $stackPreview"
        ThreadDumpInfo(
          activeThreadCount = 1,
          singleThreadStack = Some(preview),
          dumpFile = None
        )
      } else {
        // Multiple threads - write to temp file
        val timestamp = java.time.Instant.now().toString.replace(":", "-")
        val safeSuiteName = suiteName.replaceAll("[^a-zA-Z0-9.-]", "_")
        val fileName = s"thread-dump-$safeSuiteName-$timestamp.txt"
        val dumpPath = Paths.get(System.getProperty("java.io.tmpdir"), "bleep-test-dumps", fileName)

        try {
          Files.createDirectories(dumpPath.getParent)
          val content = new StringBuilder()
          content.append(s"Thread dump for suite: $suiteName\n")
          content.append(s"Project: $projectName\n")
          content.append(s"Time: ${java.time.Instant.now()}\n")
          content.append(s"Total threads: ${dump.threads.size}\n")
          content.append(s"Active threads: ${activeThreads.size}\n")
          content.append("\n" + "=" * 80 + "\n\n")

          dump.threads.foreach { thread =>
            content.append(s"Thread: ${thread.name}\n")
            content.append(s"State: ${thread.state}\n")
            thread.stackTrace.foreach(line => content.append(s"  at $line\n"))
            content.append("\n")
          }

          Files.write(dumpPath, content.toString.getBytes)

          ThreadDumpInfo(
            activeThreadCount = activeThreads.size,
            singleThreadStack = None,
            dumpFile = Some(dumpPath)
          )
        } catch {
          case e: Exception =>
            // Failed to write file, include preview in single thread stack
            val preview = activeThreads
              .take(3)
              .map { t =>
                s"Thread: ${t.name} (${t.state})\n  at ${t.stackTrace.take(5).mkString("\n  at ")}"
              }
              .mkString("\n\n")
            ThreadDumpInfo(
              activeThreadCount = activeThreads.size,
              singleThreadStack = Some(s"(Failed to write dump file: ${e.getMessage})\n$preview"),
              dumpFile = None
            )
        }
      }
    }

  /** Get the full classpath for a project including dependencies */
  def getClasspath(started: Started, project: CrossProjectName): List[Path] = {
    // Get the full classpath for the project including dependencies
    val projectPaths = started.projectPaths(project)
    val classesDir = projectPaths.classes

    // Get dependency jars from bloop config
    val bloopProject = started.bloopProject(project)
    val dependencyClasspath = bloopProject.classpath.map(p => Path.of(p.toString))

    // Find bleep-test-runner classes for ForkedTestRunner.
    // First try from the current build (when running bleep's own tests),
    // otherwise always fetch via coursier for consistency
    val testRunnerFromBuild = started.build.explodedProjects.keys
      .find(p => p.name.value == "bleep-test-runner")
      .map(p => started.projectPaths(p).classes)

    debugLog(s"testRunnerFromBuild: $testRunnerFromBuild")

    val testRunnerClasses = testRunnerFromBuild match {
      case Some(path) =>
        debugLog(s"Using build path: $path")
        List(path)
      case None =>
        debugLog("Fetching via coursier...")
        fetchTestRunnerViaCoursier()
    }

    // Verify we actually got the test runner
    if (testRunnerClasses.isEmpty) {
      throw new RuntimeException(
        "Failed to find bleep-test-runner classes. " +
          "This is required for running tests with the reactive test runner."
      )
    }

    (classesDir :: dependencyClasspath) ++ testRunnerClasses
  }

  /** Fetch bleep-test-runner via coursier */
  private def fetchTestRunnerViaCoursier(): List[Path] = {
    val version = model.BleepVersion.current.value
    debugLog(s"Fetching bleep-test-runner:$version via coursier")
    val dep = Dependency(
      Module(Organization("build.bleep"), ModuleName("bleep-test-runner")),
      version
    )

    val fetch = Fetch()
      .addDependencies(dep)
      .addRepositories(
        Repositories.central,
        Repositories.sonatype("snapshots"),
        coursier.LocalRepositories.ivy2Local
      )

    try {
      val files = fetch.run()
      // Only use bleep-test-runner.jar and test-interface.jar
      // The test project's classpath already has the test framework dependencies (junit, scalatest, etc)
      // but may not have test-interface which bleep-test-runner needs
      val neededJars = files.filter { f =>
        val name = f.getName
        name.contains("bleep-test-runner") || name.contains("test-interface")
      }
      if (neededJars.isEmpty) {
        throw new RuntimeException(
          s"bleep-test-runner jar not found in fetched files: ${files.map(_.getName).mkString(", ")}"
        )
      }
      val paths = neededJars.map(f => Path.of(f.getAbsolutePath)).toList
      debugLog(s"Using jars: ${paths.mkString(", ")}")
      paths
    } catch {
      case e: Exception =>
        throw new RuntimeException(
          s"Failed to fetch bleep-test-runner:$version via coursier. " +
            "This is needed when running bleep as a native image. " +
            s"Error: ${e.getMessage}",
          e
        )
    }
  }

  private def debugLog(msg: String): Unit = {
    import java.nio.file.{Files, Paths, StandardOpenOption}
    val line = s"[CLASSPATH] $msg\n"
    try Files.write(Paths.get("/tmp/bleep-test-reactive.log"), line.getBytes, StandardOpenOption.CREATE, StandardOpenOption.APPEND)
    catch { case _: Exception => () }
  }
}
