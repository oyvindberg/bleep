package bleep
package commands

import bleep.internal.{bleepLoggers, BspClientDisplayProgress, DoSourceGen, TransitiveProjects}
import bleep.testing.{
  ProjectAction,
  ProjectState,
  ReactiveTestClient,
  ReactiveTestRunner,
  SchedulerEvent,
  SchedulerInterpreter,
  SuiteSchedulerState,
  TestDiscovery,
  TestDisplay,
  TestEvent,
  TestRunState,
  TestSummary,
  TimeoutConfig
}
import bloop.rifle.BuildServer
import cats.effect._
import cats.effect.std.{Console, Queue}
import cats.effect.unsafe.implicits.global
import cats.syntax.all._
import ch.epfl.scala.bsp4j

import java.nio.file.{Files, Paths, StandardOpenOption}
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import java.util.concurrent.atomic.AtomicReference
import scala.concurrent.duration._
import scala.jdk.CollectionConverters._

/** Display mode for test output */
sealed trait DisplayMode
object DisplayMode {

  /** Full TUI with live updates, spinners, progress bars */
  case object Tui extends DisplayMode

  /** Simple output, just failures and summary (for CI/agents) */
  case object NoTui extends DisplayMode

  /** Smart constructor - checks if TUI is supported */
  def resolve(requested: DisplayMode): DisplayMode = requested match {
    case Tui if !bleep.testing.FancyTestDisplay.isSupported => NoTui
    case other                                              => other
  }

  /** Parse from CLI flags */
  def fromFlags(noTui: Boolean): DisplayMode =
    if (noTui) NoTui else Tui
}

/** How to compute max parallel JVMs */
sealed trait Parallelism {
  def resolve: Int
}
object Parallelism {
  private val numCores: Int = Runtime.getRuntime.availableProcessors()

  /** Fixed number of JVMs */
  case class Fixed(n: Int) extends Parallelism {
    def resolve: Int = n
  }

  /** Ratio of available cores (e.g., 1.0 = numCores, 0.5 = half) */
  case class Ratio(r: Double) extends Parallelism {
    def resolve: Int = Math.max(1, (numCores * r).toInt)
  }

  /** Default: use all cores */
  val default: Parallelism = Ratio(1.0)

  /** Parse from CLI options - fixed takes precedence over ratio */
  def fromOptions(fixed: Option[Int], ratio: Option[Double]): Parallelism =
    fixed.map(Fixed.apply).orElse(ratio.map(Ratio.apply)).getOrElse(default)
}

/** Configuration for reactive test runner */
case class TestConfig(
    /** Max parallel JVMs across all projects */
    parallelism: Parallelism,
    /** Max JVMs per individual project */
    jvmsPerProject: Int,
    /** Timeout for JVM startup in seconds */
    jvmStartupTimeoutSeconds: Int,
    /** Timeout for suite execution in minutes */
    suiteTimeoutMinutes: Int
) {
  def maxParallelJvms: Int = parallelism.resolve
}

object TestConfig {
  val default: TestConfig = TestConfig(
    parallelism = Parallelism.default,
    jvmsPerProject = 2,
    jvmStartupTimeoutSeconds = 30,
    suiteTimeoutMinutes = 2
  )
}

/** Reactive test command that runs tests as soon as they compile.
  *
  * Unlike the standard Test command, this:
  *   - Issues ONE compile command for all test projects
  *   - Starts running tests for each project as soon as it finishes compiling
  *   - Runs test suites in parallel across multiple JVMs
  *   - Provides real-time progress feedback
  */
case class TestReactive(
    watch: Boolean,
    projects: Array[model.CrossProjectName],
    config: TestConfig,
    displayMode: DisplayMode,
    jvmOptions: List[String],
    testArgs: List[String]
) extends BleepCommandRemote(watch)
    with BleepCommandRemote.OnlyChanged {

  // Queue for receiving compile events (for display)
  private val compileEventQueueRef = new AtomicReference[Option[Queue[IO, TestEvent]]](None)

  // Logging for debugging
  private val logFile = Paths.get("/tmp/bleep-test-reactive.log")
  private def log(msg: String): Unit = {
    val timestamp = LocalDateTime.now().format(DateTimeFormatter.ofPattern("HH:mm:ss.SSS"))
    val line = s"[$timestamp] $msg\n"
    try
      Files.write(logFile, line.getBytes, StandardOpenOption.CREATE, StandardOpenOption.APPEND)
    catch {
      case e: Exception => System.err.println(s"LOG WRITE FAILED: $e")
    }
    ()
  }
  private def logIO(msg: String): IO[Unit] = IO(log(msg))

  override def watchableProjects(started: Started): TransitiveProjects =
    TransitiveProjects(started.build, projects)

  override def onlyChangedProjects(started: Started, isChanged: model.CrossProjectName => Boolean): BleepCommandRemote =
    copy(projects = watchableProjects(started).transitiveFilter(isChanged).direct)

  override def wrapBuildClient(started: Started, client: BspClientDisplayProgress): bsp4j.BuildClient = {
    val testProjects = projects.toSet
    val testTargets = testProjects.map(p => BleepCommandRemote.buildTarget(started.buildPaths, p))

    def projectFromTarget(target: bsp4j.BuildTargetIdentifier): Option[model.CrossProjectName] =
      BleepCommandRemote.projectFromBuildTarget(started)(target)

    // Create the queue for compile events
    val compileEventQueue = Queue.unbounded[IO, TestEvent].unsafeRunSync()
    compileEventQueueRef.set(Some(compileEventQueue))

    // When using fancy TUI, use a silent logger for BSP
    // to prevent log messages from interfering with the TUI display.
    val bspClient: bsp4j.BuildClient = displayMode match {
      case DisplayMode.NoTui => client
      case DisplayMode.Tui   => BspClientDisplayProgress(bleepLoggers.silent)
    }

    new ReactiveTestClient(
      bspClient,
      testTargets,
      projectFromTarget,
      onTestReady = (project, _) =>
        // No longer needed - we poll state instead
        log(s"BSP: project ready: ${project.value}"),
      onCompileStart = (projectName, _) => {
        log(s"BSP: compile start: $projectName")
        val event = TestEvent.CompileStarted(projectName, System.currentTimeMillis())
        compileEventQueue.offer(event).unsafeRunSync()
      },
      onCompileProgress = (projectName, percent) => {
        val event = TestEvent.CompileProgress(projectName, percent, System.currentTimeMillis())
        compileEventQueue.offer(event).unsafeRunSync()
      },
      onCompileFinish = (projectName, _, success, errors) => {
        log(s"BSP: compile finish: $projectName (success=$success, errors=${errors.size})")
        val event = TestEvent.CompileFinished(projectName, success, 0L, System.currentTimeMillis(), errors)
        compileEventQueue.offer(event).unsafeRunSync()
      }
    )
  }

  override def runWithServer(started: Started, bloop: BuildServer): Either[BleepException, Unit] = {
    val testProjects = projects.toSet

    if (testProjects.isEmpty) {
      started.logger.info("No test projects to run")
      Right(())
    } else {
      compileEventQueueRef.get() match {
        case None =>
          Left(new BleepException.Text("Compile event queue not initialized"))
        case Some(compileEventQueue) =>
          // Run source generation first
          DoSourceGen(started, bloop, watchableProjects(started)).flatMap { _ =>
            runTestsReactively(started, bloop, testProjects, compileEventQueue)
          }
      }
    }
  }

  private def runTestsReactively(
      started: Started,
      bloop: BuildServer,
      testProjects: Set[model.CrossProjectName],
      compileEventQueue: Queue[IO, TestEvent]
  ): Either[BleepException, Unit] = {
    val console = Console.make[IO]
    val parallelism = config.maxParallelJvms

    val testTargets = testProjects.map(p => BleepCommandRemote.buildTarget(started.buildPaths, p))

    // Build the project graph for dependency tracking
    val transitive = TransitiveProjects(started.build, testProjects.toArray)
    val allProjects = transitive.all.toSet
    val projectDependencies: Map[model.CrossProjectName, Set[model.CrossProjectName]] =
      allProjects.map { p =>
        p -> started.build.resolvedDependsOn(p).toSet
      }.toMap
    val projectGraph = bleep.testing.ProjectGraph(projectDependencies, testProjects)

    // Clear log file at start
    Files.write(logFile, Array.emptyByteArray)
    log(s"=== Test run starting: ${testProjects.size} test projects, ${allProjects.size} total projects, parallelism=$parallelism ===")

    val program: IO[(ReactiveTestRunner.Result, TestSummary, TestRunState, DisplayMode)] = for {
      // Create queue for scheduler events
      schedulerEventQueue <- Queue.unbounded[IO, SchedulerEvent]

      startTime <- IO.realTime.map(_.toMillis)

      // Central state - everything is derived from this
      state <- Ref.of[IO, TestRunState](TestRunState.initial(testProjects, projectGraph, startTime))

      // Scheduler state
      timeoutCfg = TimeoutConfig.fromSeconds(config.jvmStartupTimeoutSeconds, config.suiteTimeoutMinutes)
      schedulerState <- Ref.of[IO, SuiteSchedulerState](SuiteSchedulerState.empty(parallelism, timeoutCfg))

      // Resolve display mode (checks TUI support)
      effectiveMode <- IO.pure(DisplayMode.resolve(displayMode))
      _ <- effectiveMode match {
        case DisplayMode.NoTui if displayMode == DisplayMode.Tui =>
          logIO("TUI not supported (raw mode unavailable), falling back to no-tui mode")
        case _ => IO.unit
      }

      // Create display based on mode
      displayAndCompletion <- effectiveMode match {
        case DisplayMode.NoTui =>
          TestDisplay.create(true, console).map(d => (d, d.summary, d.summary))
        case DisplayMode.Tui =>
          TestDisplay.createFancyWithState(Some(state))
      }
      (display, signalCompletionAndWait, waitForUserQuit) = displayAndCompletion

      // Create scheduler interpreter
      interpreter = SchedulerInterpreter.create(display, state, testArgs, jvmOptions, timeoutCfg)

      // Start a fiber to forward compile events to the display and update state
      compileEventForwarder <- forwardCompileEvents(compileEventQueue, display, state, testProjects).start

      _ <- logIO("Starting compilation (non-blocking)...")

      // Start compilation of all test projects (and their dependencies) - NON-BLOCKING
      compileFuture <- IO.blocking {
        val targets = testTargets.toList.asJava
        val params = new bsp4j.CompileParams(targets)
        log(s"Calling buildTargetCompile with ${targets.size} targets")
        bloop.buildTargetCompile(params)
      }

      // Create cancel signal for scheduler
      cancelSignal <- Deferred[IO, Unit]

      _ <- logIO("Starting scheduler fiber...")

      // Start the scheduler loop (replaces project stream processing)
      schedulerFiber <- interpreter.run(schedulerEventQueue, schedulerState, cancelSignal, 50).start

      _ <- logIO("Starting reactive discovery fiber...")

      // Pre-evaluate all bloop configs sequentially to avoid Lazy circular detection during parallel access.
      // The Lazy wrapper is not thread-safe, so concurrent evaluation of interdependent projects fails.
      _ <- IO.blocking {
        testProjects.foreach { p =>
          started.bloopProject(p) // Force evaluation
        }
      }

      // Start fiber to discover suites as projects finish compiling (polls state)
      // Sends SchedulerEvent.SuitesReady when suites are discovered
      discoveryFiber <- discoverSuitesReactively(
        started,
        bloop,
        schedulerEventQueue,
        state,
        display
      ).start

      _ <- logIO("Waiting for compilation to complete...")

      // Wait for compilation to complete
      _ <- IO.blocking(compileFuture.get())

      _ <- logIO("Compilation complete, stopping event forwarder...")

      // Stop the forwarder before draining to avoid race conditions
      _ <- compileEventForwarder.cancel

      _ <- logIO("Draining compile event queue...")

      // Drain remaining compile events from the queue (process them synchronously)
      _ <- drainCompileEventQueue(compileEventQueue, display, state, testProjects)

      _ <- logIO("Promoting noop projects to CompileSucceeded...")

      // Bloop doesn't send compile events for noop projects (already compiled).
      // Promote any test projects still in Initial state to CompileSucceeded.
      _ <- promoteNoopProjects(state, testProjects)

      _ <- logIO("All compile events processed, waiting for discovery to complete...")

      // Wait for discovery and processing, but allow cancellation if display exits early (user pressed 'q')
      // The display runs in background for fancy mode - if user quits, we should cancel fibers
      summary <- effectiveMode match {
        case DisplayMode.NoTui =>
          // In no-tui mode, wait for fibers then print summary
          for {
            _ <- discoveryFiber.joinWithNever
            _ <- logIO("Discovery complete, waiting for scheduler...")
            _ <- schedulerFiber.joinWithNever
            _ <- logIO("All suites completed")
            _ <- display.printSummary
            s <- display.summary
          } yield s
        case DisplayMode.Tui =>
          // In fancy mode, race completion against user quitting
          // When user presses 'q', the fancy display fiber returns early
          val waitForCompletion: IO[Either[Throwable, Unit]] = (for {
            _ <- logIO("Waiting for discovery fiber to complete...")
            _ <- discoveryFiber.joinWithNever
            _ <- logIO("Discovery complete, waiting for scheduler fiber...")
            _ <- schedulerFiber.joinWithNever
            _ <- logIO("All suites completed")
          } yield ()).attempt

          IO.race(waitForCompletion, waitForUserQuit).flatMap {
            case Left(Left(error)) =>
              // Error during discovery/scheduling - signal display to stop, then re-raise
              signalCompletionAndWait.attempt >> IO.raiseError(error)
            case Left(Right(())) =>
              // Normal completion - signal display to finish and get summary
              logIO("Tests completed normally, signaling display...") >> signalCompletionAndWait
            case Right(summary) =>
              // User quit early - get final state and print summary before cleanup
              // Important: print before cleanup since cleanup might hang on stuck JVMs
              for {
                _ <- logIO("User quit - printing summary before cleanup...")
                _ <- logIO(s"Currently running suites: ${summary.currentlyRunning}")
                finalState <- state.get
                // Mark running suites as cancelled failures
                // currentlyRunning contains keys in format "project:suite"
                cancelledSummary <- IO {
                  val cancelledSuites = summary.currentlyRunning.map { key =>
                    // Parse "project:suite" format
                    val (project, suite) = key.indexOf(':') match {
                      case -1  => ("", key) // No colon, treat whole thing as suite
                      case idx => (key.substring(0, idx), key.substring(idx + 1))
                    }
                    bleep.testing.TestFailure(
                      project = project,
                      suite = suite,
                      test = "(cancelled)",
                      message = Some("Test was cancelled by user"),
                      throwable = None,
                      output = Nil
                    )
                  }
                  summary.copy(
                    wasCancelled = true,
                    suitesFailed = summary.suitesFailed + summary.currentlyRunning.size,
                    testsFailed = summary.testsFailed + summary.currentlyRunning.size,
                    failures = summary.failures ++ cancelledSuites
                  )
                }
                _ <- IO(printFailureSummary(started, cancelledSummary, finalState.projectsCompileFailed))
                _ <- logIO("Cancelling fibers and scheduler...")
                _ <- cancelSignal.complete(()) // Signal scheduler to stop
                _ <- discoveryFiber.cancel
                _ <- schedulerFiber.cancel
                _ <- compileEventForwarder.cancel
                _ <- logIO("Fibers cancelled, cleaning up JVMs...")
              } yield cancelledSummary
          }
      }

      endTime <- IO.realTime.map(_.toMillis)
      finalState <- state.get
    } yield (
      ReactiveTestRunner.Result(
        passed = summary.testsPassed,
        failed = summary.testsFailed,
        skipped = summary.testsSkipped,
        ignored = summary.testsIgnored,
        failedSuites = summary.failures.map(_.suite).distinct,
        durationMs = endTime - startTime
      ),
      summary,
      finalState,
      effectiveMode
    )

    try {
      val (result, summary, finalState, usedMode) = program.unsafeRunSync()
      val failedProjects = finalState.projectsCompileFailed

      // Print summary after TUI exits (unless cancelled - already printed before cleanup)
      // In TUI mode, we print here. In NoTui mode, display already printed.
      usedMode match {
        case DisplayMode.Tui if !summary.wasCancelled =>
          printFailureSummary(started, summary, failedProjects)
        case _ => ()
      }

      if (summary.wasCancelled) {
        val incomplete = summary.suitesTotal - summary.suitesCompleted
        val msg = if (summary.testsFailed > 0) {
          s"Test run cancelled (${result.passed} passed, ${result.failed} failed, $incomplete suites incomplete)"
        } else {
          s"Test run cancelled (${result.passed} passed, $incomplete suites incomplete)"
        }
        Left(new BleepException.Text(msg))
      } else if (failedProjects.nonEmpty) {
        Left(
          new BleepException.Text(
            s"Compilation failed for ${failedProjects.size} project(s)"
          )
        )
      } else if (result.success) {
        Right(())
      } else {
        Left(
          new BleepException.Text(
            s"Tests failed: ${result.passed} passed, ${result.failed} failed, ${result.skipped} skipped"
          )
        )
      }
    } catch {
      case ex: Exception =>
        Left(new BleepException.Cause(ex, s"Test execution failed"))
    }
  }

  private def printFailureSummary(started: Started, summary: TestSummary, failedProjects: Map[model.CrossProjectName, List[String]]): Unit = {
    import scala.{Console => C}
    val logger = started.logger
    val failures = summary.failures

    // Compile failures section - shown first if any
    if (failedProjects.nonEmpty) {
      // Convert to string keys for easier comparison with project names
      val failedProjectsByName: Map[String, List[String]] = failedProjects.map { case (k, v) => k.value -> v }

      // Calculate which test projects were skipped due to each failed project
      val testProjectNames = projects.map(_.value).toSet
      val transitive = TransitiveProjects(started.build, projects)

      // Map: failed project -> list of test projects that depend on it
      val skippedTestsByFailure: Map[String, List[String]] = failedProjectsByName.keys.map { failedProject =>
        // Find test projects whose transitive deps include the failed project
        val affectedTests = transitive.deps
          .filter { dep =>
            dep.deps.exists(_.value == failedProject) || dep.projectName.value == failedProject
          }
          .map(_.projectName.value)
          .filter(testProjectNames.contains)
          .toList
          .sorted
        failedProject -> affectedTests
      }.toMap

      // Projects with actual errors (root causes) vs projects that failed due to dependencies
      val projectsWithErrors = failedProjectsByName.filter(_._2.nonEmpty)
      val derivedFailures = failedProjectsByName.filter(_._2.isEmpty)

      logger.info("")
      logger.info(s"${C.RED}${C.BOLD}Compilation Failures${C.RESET}")
      logger.info("")

      // Show projects with errors (root causes)
      projectsWithErrors.toList.sortBy(_._1).foreach { case (project, errors) =>
        val skippedTests = skippedTestsByFailure.getOrElse(project, Nil)
        logger.info(s"${C.RED}✗ $project${C.RESET}")

        // Show errors (limited to first 5)
        errors.take(5).foreach { error =>
          logger.info(s"  ${C.YELLOW}│${C.RESET} $error")
        }
        if (errors.size > 5) {
          logger.info(s"  ${C.YELLOW}│${C.RESET} ... and ${errors.size - 5} more errors")
        }

        // Show skipped test projects
        if (skippedTests.nonEmpty) {
          logger.info(s"  ${C.YELLOW}└─ Skipped test projects:${C.RESET}")
          skippedTests.foreach { testProject =>
            logger.info(s"     ${C.YELLOW}• $testProject${C.RESET}")
          }
        }
        logger.info("")
      }

      // Show derived failures (projects that failed because of dependencies)
      if (derivedFailures.nonEmpty) {
        logger.info(s"${C.YELLOW}Projects with failed dependencies:${C.RESET}")
        derivedFailures.keys.toList.sorted.foreach { project =>
          logger.info(s"  ${C.YELLOW}○ $project${C.RESET}")
        }
        logger.info("")
      }
    }

    // Print skipped/ignored tests first
    val skipped = summary.skipped
    if (skipped.nonEmpty) {
      logger.info("")
      logger.info(s"${C.YELLOW}${C.BOLD}Skipped/Ignored (${skipped.size})${C.RESET}")
      logger.info("")

      // Group by project, sort alphabetically
      val sortedByProject = skipped
        .groupBy(_.project)
        .toList
        .sortBy(_._1)

      sortedByProject.foreach { case (project, projectSkipped) =>
        logger.info(s"${C.MAGENTA}${C.BOLD}$project${C.RESET}")

        // Sort by suite then test
        val sorted = projectSkipped.sortBy(s => (s.suite, s.test))

        sorted.foreach { s =>
          val statusLabel = s.status match {
            case bleep.testing.TestStatus.Skipped   => s"${C.YELLOW}[SKIP]${C.RESET}"
            case bleep.testing.TestStatus.Ignored   => s"${C.YELLOW}[IGN]${C.RESET}"
            case bleep.testing.TestStatus.Pending   => s"${C.YELLOW}[PEND]${C.RESET}"
            case bleep.testing.TestStatus.Cancelled => s"${C.YELLOW}[CANC]${C.RESET}"
            case _                                  => s"${C.YELLOW}[SKIP]${C.RESET}"
          }
          logger.info(s"  $statusLabel ${C.CYAN}${s.suite}${C.RESET} ${C.WHITE}>${C.RESET} ${s.test}")
        }
        logger.info("")
      }
    }

    if (failures.nonEmpty) {
      logger.info("")
      logger.info(s"${C.RED}${C.BOLD}Failures (${failures.size})${C.RESET}")
      logger.info("")

      // Group by project -> suite -> tests
      val byProject = failures.groupBy(_.project).toList.sortBy(_._1)

      byProject.foreach { case (project, projectFailures) =>
        val bySuite = projectFailures.groupBy(_.suite).toList.sortBy(_._1)
        val suiteCount = bySuite.size
        val testCount = projectFailures.size

        // Project header
        logger.info(s"${C.RED}${C.BOLD}$project${C.RESET} ${C.WHITE}($suiteCount suite${if (suiteCount != 1) "s"
          else ""}, $testCount test${if (testCount != 1) "s" else ""} failed)${C.RESET}")

        bySuite.foreach { case (suite, suiteFailures) =>
          // Suite line
          logger.info(s"  ${C.RED}✗${C.RESET} ${C.CYAN}$suite${C.RESET}")

          // Failed tests under this suite
          suiteFailures.sortBy(_.test).foreach { failure =>
            logger.info(s"      ${C.RED}✗${C.RESET} ${failure.test}")

            // Error message
            failure.message.foreach { msg =>
              msg.split("\n").foreach { line =>
                logger.info(s"        ${C.YELLOW}$line${C.RESET}")
              }
            }

            // Stack trace (compact)
            failure.throwable.foreach { throwable =>
              val allLines = throwable.split("\n")
              // Show first line (exception type + message) and a few stack frames
              val relevantLines = allLines.take(8)
              relevantLines.foreach { line =>
                logger.info(s"        ${C.WHITE}$line${C.RESET}")
              }
              if (allLines.length > 8) {
                logger.info(s"        ${C.WHITE}... (${allLines.length - 8} more lines)${C.RESET}")
              }
            }
          }
        }
        logger.info("")
      }
    }

    // Summary stats at the bottom
    logger.info("")
    logger.info(s"${C.CYAN}${"=" * 60}${C.RESET}")
    if (summary.wasCancelled) {
      logger.info(s"${C.YELLOW}${C.BOLD}Test Summary (CANCELLED)${C.RESET}")
    } else {
      logger.info(s"${C.CYAN}${C.BOLD}Test Summary${C.RESET}")
    }
    logger.info(s"${C.CYAN}${"=" * 60}${C.RESET}")

    // Suites line
    val suitesPassed = summary.suitesCompleted - summary.suitesFailed
    val suitesPassedStr = s"${C.GREEN}$suitesPassed passed${C.RESET}"
    val suitesFailedStr = if (summary.suitesFailed > 0) s"${C.RED}${summary.suitesFailed} failed${C.RESET}" else s"${summary.suitesFailed} failed"
    logger.info(s"Suites:  $suitesPassedStr, $suitesFailedStr")

    // Tests line with colored counts
    val passedStr = s"${C.GREEN}${summary.testsPassed} passed${C.RESET}"
    val failedStr = if (summary.testsFailed > 0) s"${C.RED}${summary.testsFailed} failed${C.RESET}" else s"${summary.testsFailed} failed"
    val skippedStr2 = if (summary.testsSkipped > 0) s"${C.YELLOW}${summary.testsSkipped} skipped${C.RESET}" else s"${summary.testsSkipped} skipped"
    val ignoredStr = s"${summary.testsIgnored} ignored"
    logger.info(s"Tests:   $passedStr, $failedStr, $skippedStr2, $ignoredStr")

    // Timing stats
    val wallTimeSeconds = summary.durationMs / 1000.0
    val testTimeSeconds = summary.totalTestTimeMs / 1000.0
    val savedSeconds = testTimeSeconds - wallTimeSeconds
    val speedup = if (wallTimeSeconds > 0) testTimeSeconds / wallTimeSeconds else 1.0

    logger.info(f"Time:    Wall clock: ${wallTimeSeconds}%.1fs, Total test time: ${testTimeSeconds}%.1fs")
    if (savedSeconds > 0) {
      logger.info(f"         ${C.GREEN}Saved ${savedSeconds}%.1fs thanks to parallelism (${speedup}%.1fx speedup)${C.RESET}")
    }
    logger.info(s"${C.CYAN}${"=" * 60}${C.RESET}")
  }

  /** Get the platform for a project */
  private def getPlatform(started: Started, project: model.CrossProjectName): model.PlatformId =
    started.build.explodedProjects(project).platform.flatMap(_.name).getOrElse(model.PlatformId.Jvm)

  /** Discover test suites reactively as projects finish compiling. Polls state to find CompileSucceeded projects that need discovery.
    *
    * Platform routing:
    *   - JVM projects: Full granularity via reactive runner (suite/test level tracking)
    *   - JS/Native projects: BSP fallback (project-level only, no granular tracking)
    */
  private def discoverSuitesReactively(
      started: Started,
      bloop: BuildServer,
      schedulerEventQueue: Queue[IO, SchedulerEvent],
      state: Ref[IO, TestRunState],
      display: TestDisplay
  ): IO[Unit] = {
    import bleep.testing.TestTypes.SuiteClassName
    import scala.jdk.CollectionConverters._

    /** Process a JVM project - discover suites for reactive runner */
    def processJvmProject(project: model.CrossProjectName): IO[Unit] = {
      val target = BleepCommandRemote.buildTarget(started.buildPaths, project)
      for {
        _ <- state.update(_.reduceSimple(ProjectAction.StartDiscovery(project)))
        _ <- logIO(s"[JVM] Discovering suites for: ${project.value}")
        suites <- IO.blocking(TestDiscovery.discoverForProject(bloop, project, target))
        _ <- logIO(s"[JVM] Found ${suites.size} suites in ${project.value}")
        suiteClassNames = suites.map(s => SuiteClassName(s.className))
        _ <- state.update(_.reduceSimple(ProjectAction.FinishDiscovery(project, suiteClassNames)))
        updatedState <- state.get
        _ <- display.handle(TestEvent.SuitesDiscovered(updatedState.totalSuitesDiscovered, System.currentTimeMillis()))
        classpath <- IO.blocking(ReactiveTestRunner.getClasspath(started, project))
        // Send suites to scheduler - it will create SuiteJobs
        _ <- logIO(s"[JVM] Sending ${suites.size} suites to scheduler for: ${project.value}")
        _ <- schedulerEventQueue.offer(SchedulerEvent.SuitesReady(project, suites, classpath, started.jvmCommand, jvmOptions))
      } yield ()
    }

    /** Process a JS/Native project - run tests via BSP (project-level only) */
    def processBspProject(project: model.CrossProjectName, platform: model.PlatformId): IO[Unit] = {
      val target = BleepCommandRemote.buildTarget(started.buildPaths, project)
      val platformName = platform.value.toUpperCase
      for {
        _ <- logIO(s"[$platformName] Running tests via BSP for: ${project.value}")
        startTime <- IO.realTime.map(_.toMillis)
        // Update state to TestsInProgress with BSP execution
        _ <- state.update(_.reduceSimple(ProjectAction.StartBspTests(project, platform, startTime)))
        // Run BSP test
        result <- IO.blocking {
          val testParams = new bsp4j.TestParams(List(target).asJava)
          bloop.buildTargetTest(testParams).get()
        }.attempt
        endTime <- IO.realTime.map(_.toMillis)
        durationMs = endTime - startTime
        // Handle result
        _ <- result match {
          case Right(testResult) =>
            val success = testResult.getStatusCode == bsp4j.StatusCode.OK
            logIO(s"[$platformName] Tests ${if (success) "passed" else "failed"} for: ${project.value} (${durationMs}ms)") >>
              state.update(_.reduceSimple(ProjectAction.FinishBspTests(project, success, durationMs, Nil)))
          case Left(error) =>
            logIO(s"[$platformName] Tests error for: ${project.value}: ${error.getMessage}") >>
              state.update(_.reduceSimple(ProjectAction.FinishBspTests(project, success = false, durationMs, List(error.getMessage))))
        }
      } yield ()
    }

    def processReadyProjects: IO[List[model.CrossProjectName]] = for {
      currentState <- state.get
      // Find test projects in CompileSucceeded state (ready for discovery/testing)
      readyProjects = currentState.projectsCompileSucceeded.filter(currentState.testProjects.contains)
      // Process projects in parallel, routing by platform
      _ <- readyProjects.parTraverse_ { project =>
        val platform = getPlatform(started, project)
        platform match {
          case model.PlatformId.Jvm =>
            // Full granularity via reactive runner
            processJvmProject(project)
          case nonJvm =>
            // BSP fallback for JS/Native
            processBspProject(project, nonJvm)
        }
      }
    } yield readyProjects

    def isDiscoveryDone: IO[Boolean] = state.get.map { currentState =>
      // Discovery is done when no test projects need discovery
      // (none in Initial, Compiling, or CompileSucceeded state)
      !currentState.testProjects.exists { p =>
        currentState.projects.get(p) match {
          case Some(_: bleep.testing.ProjectState.Initial)          => true
          case Some(_: bleep.testing.ProjectState.Compiling)        => true
          case Some(_: bleep.testing.ProjectState.CompileSucceeded) => true
          case _                                                    => false
        }
      }
    }

    def loop: IO[Unit] = for {
      _ <- processReadyProjects
      done <- isDiscoveryDone
      _ <-
        if (done) {
          logIO("Discovery complete for all test projects, signaling scheduler") >>
            schedulerEventQueue.offer(SchedulerEvent.DiscoveryComplete)
        } else {
          // Wait a bit before polling again
          IO.sleep(50.millis) >> loop
        }
    } yield ()

    loop
  }

  /** Drain all remaining compile events from the queue. This processes events synchronously until the queue is empty. When a compile failure occurs, downstream
    * projects are automatically marked as Skipped.
    */
  private def drainCompileEventQueue(
      queue: Queue[IO, TestEvent],
      display: TestDisplay,
      state: Ref[IO, TestRunState],
      testProjects: Set[model.CrossProjectName]
  ): IO[Unit] = {
    def projectFromName(name: String): model.CrossProjectName =
      testProjects.find(_.value == name).getOrElse(model.CrossProjectName(model.ProjectName(name), None))

    def processEvent(event: TestEvent): IO[Unit] = event match {
      case TestEvent.CompileStarted(project, timestamp) =>
        val action = ProjectAction.StartCompile(projectFromName(project), timestamp)
        state.update(_.reduceSimple(action)) >> display.handle(event)

      case TestEvent.CompileFinished(project, success, durationMs, _, errors) =>
        val action = ProjectAction.FinishCompile(projectFromName(project), success, errors, durationMs)
        for {
          _ <- logIO(s"Drain: compile finished for $project (success=$success, errors=${errors.size})")
          skipped <- state.modify { s =>
            val (newState, skippedProjects) = s.reduce(action)
            (newState, skippedProjects)
          }
          _ <- display.handle(event)
          // Send skip events for all downstream projects that were marked as Skipped
          _ <- skipped.toList.traverse_ { skippedProject =>
            val skipEvent = TestEvent.ProjectSkipped(
              skippedProject.value,
              s"Dependency $project failed to compile",
              System.currentTimeMillis()
            )
            logIO(s"Cascading skip: ${skippedProject.value} (downstream of $project)") >>
              display.handle(skipEvent)
          }
        } yield ()

      case _ =>
        display.handle(event)
    }

    def loop: IO[Unit] = queue.tryTake.flatMap {
      case Some(event) => processEvent(event) >> loop
      case None        => IO.unit
    }

    loop
  }

  /** Promote test projects still in Initial state to CompileSucceeded. This handles "noop" projects that Bloop doesn't send compile events for.
    */
  private def promoteNoopProjects(
      state: Ref[IO, TestRunState],
      testProjects: Set[model.CrossProjectName]
  ): IO[Unit] = for {
    currentState <- state.get
    initialProjects = testProjects.filter { p =>
      currentState.projects.get(p).exists(_.isInstanceOf[bleep.testing.ProjectState.Initial])
    }
    _ <- initialProjects.toList.traverse_ { project =>
      logIO(s"Promoting noop project to CompileSucceeded: ${project.value}") >>
        state.update(_.reduceSimple(ProjectAction.FinishCompile(project, success = true, errors = Nil, durationMs = 0L)))
    }
  } yield ()

  /** Forward compile events from the queue to the display and update central state */
  private def forwardCompileEvents(
      queue: Queue[IO, TestEvent],
      display: TestDisplay,
      state: Ref[IO, TestRunState],
      testProjects: Set[model.CrossProjectName]
  ): IO[Unit] = {
    def projectFromName(name: String): model.CrossProjectName =
      testProjects.find(_.value == name).getOrElse(model.CrossProjectName(model.ProjectName(name), None))

    def processEvent(event: TestEvent): IO[Unit] = event match {
      case TestEvent.CompileStarted(project, timestamp) =>
        val action = ProjectAction.StartCompile(projectFromName(project), timestamp)
        state.update(_.reduceSimple(action)) >> display.handle(event)

      case TestEvent.CompileProgress(project, percent, _) =>
        val action = ProjectAction.UpdateCompileProgress(projectFromName(project), percent)
        state.update(_.reduceSimple(action))

      case TestEvent.CompileFinished(project, success, durationMs, _, errors) =>
        val action = ProjectAction.FinishCompile(projectFromName(project), success, errors, durationMs)
        for {
          _ <- logIO(s"State update: compile finished for $project (success=$success, errors=${errors.size})")
          skipped <- state.modify { s =>
            val (newState, skippedProjects) = s.reduce(action)
            (newState, skippedProjects)
          }
          _ <- display.handle(event)
          // Send skip events for all downstream projects that were marked as Skipped
          _ <- skipped.toList.traverse_ { skippedProject =>
            val skipEvent = TestEvent.ProjectSkipped(
              skippedProject.value,
              s"Dependency $project failed to compile",
              System.currentTimeMillis()
            )
            logIO(s"Cascading skip: ${skippedProject.value} (downstream of $project)") >>
              display.handle(skipEvent)
          }
        } yield ()

      case _ =>
        display.handle(event)
    }

    queue.tryTake.flatMap {
      case Some(event) =>
        processEvent(event) >> forwardCompileEvents(queue, display, state, testProjects)
      case None =>
        // No event available, wait a bit and try again
        IO.sleep(scala.concurrent.duration.Duration(10, scala.concurrent.duration.MILLISECONDS)) >>
          forwardCompileEvents(queue, display, state, testProjects)
    }
  }
}

object TestReactive {
  val default: TestReactive = TestReactive(
    watch = false,
    projects = Array.empty,
    config = TestConfig.default,
    displayMode = DisplayMode.Tui,
    jvmOptions = Nil,
    testArgs = Nil
  )
}
