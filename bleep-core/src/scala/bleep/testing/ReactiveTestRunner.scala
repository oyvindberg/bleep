package bleep.testing

import bleep.bsp.protocol.{OutputChannel, TestStatus}
import bleep.bsp.BuildServer
import bleep.{model, Started}
import bleep.model.{CrossProjectName, SuiteName, TestName}
import cats.effect._
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
      idleTimeout: FiniteDuration
  )

  object Options {
    val default: Options = Options(
      jvmOptions = Nil,
      testArgs = Nil,
      maxParallelJvms = Runtime.getRuntime.availableProcessors(),
      quietMode = false,
      idleTimeout = model.BspServerConfig.DefaultTestIdleTimeoutMinutes.minutes
    )
  }

  /** Result of running tests */
  case class Result(
      passed: Int,
      failed: Int,
      skipped: Int,
      ignored: Int,
      failedSuites: List[SuiteName],
      durationMs: Long
  ) {
    def success: Boolean = failed == 0
  }

  /** Run tests for the given projects.
    *
    * @param started
    *   The bleep Started context
    * @param server
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
      server: BuildServer,
      testProjects: Set[CrossProjectName],
      options: Options
  ): IO[Result] =

    JvmPool.create(options.maxParallelJvms, started.jvmCommand, started.buildPaths.buildDir).use { pool =>
      for {
        display <- BuildDisplay.create(options.quietMode, started.logger)

        // Build target lookup
        buildTargetFor = (p: CrossProjectName) =>
          new bsp4j.BuildTargetIdentifier(
            started.projectPaths(p).targetDir.toUri.toASCIIString
          )

        // Discover all test suites
        suites <- IO(
          TestDiscovery.discoverAll(
            server,
            testProjects,
            buildTargetFor
          )
        )

        _ <- IO.delay(started.logger.info(s"Discovered ${suites.size} test suites in ${testProjects.size} projects"))

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

  private def runProjectSuites(
      started: Started,
      project: CrossProjectName,
      suites: List[DiscoveredSuite],
      pool: JvmPool,
      display: BuildDisplay,
      options: Options
  ): IO[Unit] = {
    // Get classpath for this project
    val classpath = getClasspath(started, project)

    // The ForkedTestRunner class must be on the classpath
    // It's in the bleep-test-runner project which has minimal dependencies
    val runnerClass = "bleep.testing.runner.ForkedTestRunner"

    val environment = started.build.explodedProjects.get(project).flatMap(_.platform).map(_.jvmEnvironment.toMap).getOrElse(Map.empty)
    val projectDir = started.build.explodedProjects.get(project).flatMap(_.folder).map(rp => started.buildPaths.buildDir.resolve(rp.toString))
    pool.acquire(classpath, options.jvmOptions, runnerClass, environment, projectDir).use { jvm =>
      suites.traverse_ { suite =>
        runSuite(
          projectName = project,
          suite = suite,
          jvm = jvm,
          display = display,
          testArgs = options.testArgs,
          idleTimeout = options.idleTimeout,
          logger = started.logger
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
      projectName: CrossProjectName,
      suite: DiscoveredSuite,
      jvm: TestJvm,
      display: BuildDisplay,
      testArgs: List[String],
      idleTimeout: FiniteDuration,
      logger: ryddig.Logger
  ): IO[Unit] = {

    def debugLog(msg: String): Unit = logger.debug(s"[RUNNER] $msg")

    val suiteKey = s"$projectName/${suite.className}"

    // Track test counts and whether SuiteFinished was sent
    // lastActivityAt is reset on each TestFinished — the idle timeout fires only when
    // no test has completed for `idleTimeout` duration.
    def processResponses(lastActivityAt: Ref[IO, Long]): IO[SuiteResult] = for {
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
          IO.realTime.map(_.toMillis).flatMap(now => display.handle(BuildEvent.TestStarted(projectName, SuiteName(suite.className), TestName(test), now)))

        case TestProtocol.TestResponse.TestFinished(_, test, status, durationMs, message, throwable) =>
          val testStatus = TestStatus.fromString(status)
          val updateCount = testStatus match {
            case TestStatus.Passed                                         => passedCount.update(_ + 1)
            case TestStatus.Failed | TestStatus.Error | TestStatus.Timeout => failedCount.update(_ + 1)
            case TestStatus.Skipped | TestStatus.AssumptionFailed          => skippedCount.update(_ + 1)
            case TestStatus.Cancelled                                      => failedCount.update(_ + 1)
            case TestStatus.Ignored | TestStatus.Pending                   => ignoredCount.update(_ + 1)
          }
          updateCount >> IO.realTime
            .map(_.toMillis)
            .flatMap { now =>
              // Reset idle timeout on each test completion
              lastActivityAt.set(now) >>
                display.handle(
                  BuildEvent.TestFinished(projectName, SuiteName(suite.className), TestName(test), testStatus, durationMs, message, throwable, now)
                )
            }

        case TestProtocol.TestResponse.SuiteDone(_, passed, failed, skipped, ignored, durationMs) =>
          IO.realTime
            .map(_.toMillis)
            .flatMap(now =>
              display.handle(
                BuildEvent.SuiteFinished(projectName, SuiteName(suite.className), passed, failed, skipped, ignored, durationMs, now)
              )
            ) >> suiteFinishedSent.set(true)

        case TestProtocol.TestResponse.Log(level, message, logSuite) =>
          val channel = if (level == "error" || level == "warn") OutputChannel.Stderr else OutputChannel.Stdout
          val effectiveSuite = logSuite.getOrElse(suite.className)
          IO.realTime
            .map(_.toMillis)
            .flatMap(now => display.handle(BuildEvent.Output(projectName, SuiteName(effectiveSuite), s"[$level] $message", channel, now)))

        case TestProtocol.TestResponse.Error(message, throwable) =>
          IO.realTime
            .map(_.toMillis)
            .flatMap(now =>
              display.handle(BuildEvent.Output(projectName, SuiteName(suite.className), s"[ERROR] $message", OutputChannel.Stderr, now)) >>
                throwable.traverse_(t => display.handle(BuildEvent.Output(projectName, SuiteName(suite.className), t, OutputChannel.Stderr, now)))
            )

        case TestProtocol.TestResponse.Ready =>
          IO.unit // Ignore Ready messages during suite execution

        case TestProtocol.TestResponse.ThreadDump(_) =>
          IO.unit // Ignore ThreadDump during normal suite execution (used for idle timeout handling)
      }
      sent <- suiteFinishedSent.get
      passed <- passedCount.get
      failed <- failedCount.get
      skipped <- skippedCount.get
      ignored <- ignoredCount.get
    } yield SuiteResult(sent, passed, failed, skipped, ignored)

    def handleTimeout: IO[Unit] = for {
      _ <- IO(debugLog(s"IDLE TIMEOUT TRIGGERED for suite: $suiteKey"))
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
      threadDumpInfo <- IO(processThreadDump(projectName, SuiteName(suite.className), threadDump))
      _ <- IO(debugLog(s"Killing JVM for $suiteKey..."))
      _ <- jvm.kill
      _ <- IO(debugLog(s"JVM killed for $suiteKey, sending SuiteTimedOut event"))
      _ <- display.handle(
        BuildEvent.SuiteTimedOut(projectName, SuiteName(suite.className), idleTimeout.toMillis, threadDumpInfo, endTime)
      )
      _ <- IO(debugLog(s"IDLE TIMEOUT COMPLETE for $suiteKey"))
    } yield ()

    // Idle timeout: polls lastActivityAt every second and fires when no activity for idleTimeout duration
    def idleTimeoutFiber(lastActivityAt: Ref[IO, Long]): IO[Unit] = {
      val checkInterval = 1.second
      def loop: IO[Unit] = for {
        now <- IO.realTime.map(_.toMillis)
        lastActivity <- lastActivityAt.get
        elapsed = now - lastActivity
        _ <-
          if (elapsed >= idleTimeout.toMillis) {
            IO(debugLog(s"Idle timeout fired for $suiteKey (no activity for ${elapsed}ms)"))
          } else {
            IO.sleep(checkInterval) >> loop
          }
      } yield ()
      loop
    }

    for {
      startTime <- IO.realTime.map(_.toMillis)
      lastActivityAt <- Ref.of[IO, Long](startTime)
      _ <- display.handle(BuildEvent.SuiteStarted(projectName, SuiteName(suite.className), startTime))
      _ <- IO(debugLog(s"Starting race: processResponses vs idleTimeout(${idleTimeout.toSeconds}s) for $suiteKey"))
      // Use racePair instead of race - race waits for cancellation of the loser,
      // but blocking IO (readLine) can't be cancelled, so race would hang.
      // racePair returns immediately when one side wins, giving us the fiber to cancel later.
      raceResult <- IO.racePair(
        processResponses(lastActivityAt),
        idleTimeoutFiber(lastActivityAt)
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
                   BuildEvent
                     .SuiteFinished(
                       projectName,
                       SuiteName(suite.className),
                       result.passed,
                       result.failed,
                       result.skipped,
                       result.ignored,
                       endTime - startTime,
                       endTime
                     )
                 )
               } else IO.unit)
          }
        case Right((responseFiber, _)) =>
          // Timeout won - kill JVM first (which will unblock readLine), then cancel fiber
          IO(debugLog(s"Idle timeout won for $suiteKey, handling...")) >>
            handleTimeout >>
            responseFiber.cancel // This should now complete quickly since JVM is dead
      }
    } yield ()
  }

  /** Process thread dump and create ThreadDumpInfo */
  private def processThreadDump(
      projectName: CrossProjectName,
      suiteName: SuiteName,
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
        val safeSuiteName = suiteName.value.replaceAll("[^a-zA-Z0-9.-]", "_")
        val fileName = s"thread-dump-$safeSuiteName-$timestamp.txt"
        val dumpPath = Paths.get(System.getProperty("java.io.tmpdir"), "bleep-test-dumps", fileName)

        try {
          Files.createDirectories(dumpPath.getParent)
          val content = new StringBuilder()
          content.append(s"Thread dump for suite: ${suiteName.value}\n")
          content.append(s"Project: ${projectName.value}\n")
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
    val logger = started.logger
    def debugLog(msg: String): Unit = logger.debug(s"[CLASSPATH] $msg")

    // Get the full classpath for the project including dependencies
    val projectPaths = started.projectPaths(project)
    val classesDir = projectPaths.classes

    val resolvedProject = started.resolvedProject(project)
    val dependencyClasspath = resolvedProject.classpath.map(p => Path.of(p.toString))

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
        fetchTestRunnerViaCoursier(logger)
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
  private def fetchTestRunnerViaCoursier(logger: ryddig.Logger): List[Path] = {
    def debugLog(msg: String): Unit = logger.debug(s"[CLASSPATH] $msg")
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
      if (files.isEmpty) {
        throw new RuntimeException(
          s"bleep-test-runner resolution returned no jars for version $version"
        )
      }
      val paths = files.map(f => Path.of(f.getAbsolutePath)).toList
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

}
