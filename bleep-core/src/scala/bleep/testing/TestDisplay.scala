package bleep.testing

import cats.effect._
import cats.effect.std.{Console => CatsConsole}
import cats.syntax.all._

import scala.{Console => SConsole}

/** Displays test progress in real-time.
  *
  * Features:
  *   - Shows currently running tests
  *   - Live-updates passed/failed/skipped counts
  *   - Collects failures for summary at end
  *   - Optional quiet mode (only show failures)
  */
trait TestDisplay {

  /** Handle a test event */
  def handle(event: TestEvent): IO[Unit]

  /** Get the current summary */
  def summary: IO[TestSummary]

  /** Print final summary */
  def printSummary: IO[Unit]
}

case class TestSummary(
    suitesTotal: Int,
    suitesCompleted: Int,
    suitesFailed: Int,
    testsTotal: Int,
    testsPassed: Int,
    testsFailed: Int,
    testsSkipped: Int,
    testsIgnored: Int,
    currentlyRunning: List[String],
    failures: List[TestFailure],
    skipped: List[TestSkipped],
    durationMs: Long,
    totalTestTimeMs: Long, // Sum of all individual test durations (for parallelism stats)
    wasCancelled: Boolean = false
)

object TestSummary {
  val empty: TestSummary = TestSummary(
    suitesTotal = 0,
    suitesCompleted = 0,
    suitesFailed = 0,
    testsTotal = 0,
    testsPassed = 0,
    testsFailed = 0,
    testsSkipped = 0,
    testsIgnored = 0,
    currentlyRunning = Nil,
    failures = Nil,
    skipped = Nil,
    durationMs = 0L,
    totalTestTimeMs = 0L,
    wasCancelled = false
  )
}

case class TestFailure(
    project: String,
    suite: String,
    test: String,
    message: Option[String],
    throwable: Option[String],
    output: List[String]
)

case class TestSkipped(
    project: String,
    suite: String,
    test: String,
    status: TestStatus // Skipped, Ignored, Cancelled, or Pending
)

object TestDisplay {

  /** Create a new test display */
  def create(
      quietMode: Boolean,
      console: CatsConsole[IO]
  ): IO[TestDisplay] =
    for {
      state <- Ref.of[IO, DisplayState](DisplayState.empty)
      startTime <- IO.realTime.map(_.toMillis)
    } yield new TestDisplayImpl(state, startTime, quietMode, console)

  private case class DisplayState(
      suitesTotal: Int,
      suitesCompleted: Int,
      suitesFailed: Int,
      testsTotal: Int,
      testsPassed: Int,
      testsFailed: Int,
      testsSkipped: Int,
      testsIgnored: Int,
      currentlyRunning: Set[String],
      currentlyCompiling: Set[String],
      failures: List[TestFailure],
      skipped: List[TestSkipped],
      pendingOutput: Map[String, List[String]], // suite -> output lines (for quiet mode)
      totalTestTimeMs: Long // Sum of individual test durations
  )

  private object DisplayState {
    def empty: DisplayState = DisplayState(
      suitesTotal = 0,
      suitesCompleted = 0,
      suitesFailed = 0,
      testsTotal = 0,
      testsPassed = 0,
      testsFailed = 0,
      testsSkipped = 0,
      testsIgnored = 0,
      currentlyRunning = Set.empty,
      currentlyCompiling = Set.empty,
      failures = Nil,
      skipped = Nil,
      pendingOutput = Map.empty,
      totalTestTimeMs = 0
    )
  }

  private class TestDisplayImpl(
      state: Ref[IO, DisplayState],
      startTime: Long,
      quietMode: Boolean,
      console: CatsConsole[IO]
  ) extends TestDisplay {

    override def handle(event: TestEvent): IO[Unit] = event match {
      case TestEvent.CompileStarted(project, _) =>
        state.update(s => s.copy(currentlyCompiling = s.currentlyCompiling + project)) >> (if (!quietMode) console.println(s"Compiling: $project") else IO.unit)

      case TestEvent.CompileFinished(project, success, durationMs, _, _) =>
        val status = if (success) "done" else "FAILED"
        state.update(s =>
          s.copy(currentlyCompiling = s.currentlyCompiling - project)
        ) >> (if (!quietMode) console.println(s"Compile $status: $project (${durationMs}ms)") else IO.unit)

      case TestEvent.SuiteStarted(project, suite, _) =>
        val key = s"$project:$suite"
        state.update(s =>
          s.copy(
            suitesTotal = s.suitesTotal + 1,
            currentlyRunning = s.currentlyRunning + key
          )
        ) >> (if (!quietMode) printStatus else IO.unit)

      case TestEvent.TestStarted(project, suite, test, _) =>
        val key = s"$project:$suite:$test"
        state.update(s =>
          s.copy(
            testsTotal = s.testsTotal + 1,
            currentlyRunning = s.currentlyRunning + key
          )
        )

      case TestEvent.TestFinished(project, suite, test, status, durationMs, message, throwable, _) =>
        val key = s"$project:$suite:$test"
        for {
          _ <- state.update { s =>
            val updatedFailures = status match {
              case TestStatus.Failed | TestStatus.Error | TestStatus.Timeout =>
                val output = s.pendingOutput.getOrElse(s"$project:$suite", Nil)
                TestFailure(project, suite, test, message, None, output) :: s.failures
              case _ => s.failures
            }

            val updatedSkipped = status match {
              case TestStatus.Skipped | TestStatus.Cancelled | TestStatus.Ignored | TestStatus.Pending =>
                TestSkipped(project, suite, test, status) :: s.skipped
              case _ => s.skipped
            }

            s.copy(
              currentlyRunning = s.currentlyRunning - key,
              testsPassed = s.testsPassed + (if (status == TestStatus.Passed) 1 else 0),
              testsFailed = s.testsFailed + (if (status == TestStatus.Failed || status == TestStatus.Error || status == TestStatus.Timeout) 1 else 0),
              testsSkipped = s.testsSkipped + (if (status == TestStatus.Skipped || status == TestStatus.Cancelled) 1 else 0),
              testsIgnored = s.testsIgnored + (if (status == TestStatus.Ignored || status == TestStatus.Pending) 1 else 0),
              failures = updatedFailures,
              skipped = updatedSkipped,
              totalTestTimeMs = s.totalTestTimeMs + durationMs
            )
          }
          _ <- if (!quietMode) printTestResult(suite, test, status, durationMs) else IO.unit
        } yield ()

      case TestEvent.SuiteFinished(project, suite, passed, failed, skipped, ignored, durationMs, _) =>
        val key = s"$project:$suite"
        for {
          _ <- state.update { s =>
            s.copy(
              suitesCompleted = s.suitesCompleted + 1,
              suitesFailed = s.suitesFailed + (if (failed > 0) 1 else 0),
              currentlyRunning = s.currentlyRunning - key,
              pendingOutput = s.pendingOutput - key
            )
          }
          _ <- if (!quietMode) printSuiteResult(suite, passed, failed, skipped, ignored, durationMs) else IO.unit
        } yield ()

      case TestEvent.Output(project, suite, line, isError, _) =>
        val key = s"$project:$suite"
        for {
          _ <- state.update(s =>
            s.copy(
              pendingOutput = s.pendingOutput.updated(
                key,
                s.pendingOutput.getOrElse(key, Nil) :+ line
              )
            )
          )
          _ <- if (!quietMode) console.println(s"  $line") else IO.unit
        } yield ()

      case TestEvent.SuitesDiscovered(count, _) =>
        // For basic display, just log the count if not in quiet mode
        if (!quietMode) console.println(s"Discovered $count test suites")
        else IO.unit

      case TestEvent.ProjectSkipped(project, reason, _) =>
        // Log skipped projects
        if (!quietMode) console.println(s"${SConsole.YELLOW}[SKIPPED]${SConsole.RESET} $project: $reason")
        else IO.unit

      case TestEvent.CompileProgress(_, _, _) =>
        // Basic display doesn't show compile progress
        IO.unit

      case TestEvent.SuiteTimedOut(project, suite, timeoutMs, threadDumpInfo, _) =>
        val key = s"$project:$suite"
        val timeoutSec = timeoutMs / 1000
        for {
          _ <- state.update { s =>
            val timeoutFailure = TestFailure(
              project = project,
              suite = suite,
              test = "(timeout)",
              message = Some(s"Suite timed out after ${timeoutSec}s"),
              throwable = threadDumpInfo.flatMap(_.singleThreadStack),
              output = threadDumpInfo.flatMap(_.dumpFile).map(p => s"Thread dump: $p").toList
            )
            s.copy(
              suitesCompleted = s.suitesCompleted + 1,
              suitesFailed = s.suitesFailed + 1,
              testsFailed = s.testsFailed + 1,
              currentlyRunning = s.currentlyRunning - key,
              failures = timeoutFailure :: s.failures
            )
          }
          _ <- console.println(s"${SConsole.RED}[TIMEOUT]${SConsole.RESET} $suite after ${timeoutSec}s")
          _ <- threadDumpInfo.flatMap(_.singleThreadStack) match {
            case Some(stack) => console.println(s"  Stack trace:\n$stack")
            case None        => IO.unit
          }
          _ <- threadDumpInfo.flatMap(_.dumpFile) match {
            case Some(path) => console.println(s"  Full thread dump: $path")
            case None       => IO.unit
          }
        } yield ()
    }

    private def printStatus: IO[Unit] =
      for {
        s <- state.get
        running = s.currentlyRunning.take(3).mkString(", ")
        more = if (s.currentlyRunning.size > 3) s" (+${s.currentlyRunning.size - 3} more)" else ""
        _ <- console.println(s"Running: $running$more")
      } yield ()

    private def printTestResult(
        suite: String,
        test: String,
        status: TestStatus,
        durationMs: Long
    ): IO[Unit] = {
      val icon = status match {
        case TestStatus.Passed    => SConsole.GREEN + "+" + SConsole.RESET
        case TestStatus.Failed    => SConsole.RED + "x" + SConsole.RESET
        case TestStatus.Error     => SConsole.RED + "!" + SConsole.RESET
        case TestStatus.Timeout   => SConsole.RED + "T" + SConsole.RESET
        case TestStatus.Skipped   => SConsole.YELLOW + "-" + SConsole.RESET
        case TestStatus.Ignored   => SConsole.YELLOW + "o" + SConsole.RESET
        case TestStatus.Cancelled => SConsole.YELLOW + "c" + SConsole.RESET
        case TestStatus.Pending   => SConsole.YELLOW + "?" + SConsole.RESET
      }
      console.println(s"  $icon $test (${durationMs}ms)")
    }

    private def printSuiteResult(
        suite: String,
        passed: Int,
        failed: Int,
        skipped: Int,
        ignored: Int,
        durationMs: Long
    ): IO[Unit] = {
      val status =
        if (failed > 0) SConsole.RED + "FAILED" + SConsole.RESET
        else SConsole.GREEN + "PASSED" + SConsole.RESET
      console.println(s"$status $suite: $passed passed, $failed failed, $skipped skipped ($durationMs ms)")
    }

    override def summary: IO[TestSummary] =
      for {
        s <- state.get
        now <- IO.realTime.map(_.toMillis)
      } yield TestSummary(
        suitesTotal = s.suitesTotal,
        suitesCompleted = s.suitesCompleted,
        suitesFailed = s.suitesFailed,
        testsTotal = s.testsTotal,
        testsPassed = s.testsPassed,
        testsFailed = s.testsFailed,
        testsSkipped = s.testsSkipped,
        testsIgnored = s.testsIgnored,
        currentlyRunning = s.currentlyRunning.toList,
        failures = s.failures.reverse,
        skipped = s.skipped.reverse,
        durationMs = now - startTime,
        totalTestTimeMs = s.totalTestTimeMs
      )

    override def printSummary: IO[Unit] =
      for {
        s <- summary
        _ <- console.println("")
        _ <- console.println("=" * 60)
        _ <- console.println("Test Summary")
        _ <- console.println("=" * 60)
        _ <- console.println(s"Suites: ${s.suitesCompleted}/${s.suitesTotal} completed, ${s.suitesFailed} failed")
        _ <- console.println(s"Tests:  ${s.testsPassed} passed, ${s.testsFailed} failed, ${s.testsSkipped} skipped, ${s.testsIgnored} ignored")
        wallTimeSeconds = s.durationMs / 1000.0
        testTimeSeconds = s.totalTestTimeMs / 1000.0
        savedSeconds = testTimeSeconds - wallTimeSeconds
        speedup = if (wallTimeSeconds > 0) testTimeSeconds / wallTimeSeconds else 1.0
        _ <- console.println(f"Time:   Wall clock: ${wallTimeSeconds}%.1fs, Total test time: ${testTimeSeconds}%.1fs")
        _ <-
          if (savedSeconds > 0)
            console.println(f"        Saved ${savedSeconds}%.1fs thanks to parallelism (${speedup}%.1fx speedup)")
          else IO.unit
        _ <- if (s.failures.nonEmpty) printFailures(s.failures) else IO.unit
        _ <- console.println("=" * 60)
      } yield ()

    private def printFailures(failures: List[TestFailure]): IO[Unit] =
      for {
        _ <- console.println("")
        _ <- console.println(SConsole.RED + "Failures:" + SConsole.RESET)
        _ <- failures.traverse_ { f =>
          for {
            _ <- console.println(s"  ${f.suite} > ${f.test}")
            _ <- f.message.traverse_(m => console.println(s"    $m"))
            _ <- f.output.take(10).traverse_(o => console.println(s"    | $o"))
            _ <- if (f.output.size > 10) console.println(s"    ... (${f.output.size - 10} more lines)") else IO.unit
          } yield ()
        }
      } yield ()
  }

  /** Create a fancy TUI display. Returns:
    *   - display: The TestDisplay to send events to
    *   - signalCompletionAndWait: IO that signals completion and waits for summary (use when tests finish)
    *   - waitForUserQuit: IO that waits for user to quit (without signaling) - returns summary when user presses 'q'
    */
  def createFancy: IO[(TestDisplay, IO[TestSummary], IO[TestSummary])] =
    createFancyWithState(None)

  /** Create a fancy TUI display with access to TestRunState for richer display. When testRunState is provided, the running section shows projects with compile
    * progress, test progress, JVM assignments, and failure counts.
    */
  def createFancyWithState(testRunState: Option[Ref[IO, TestRunState]]): IO[(TestDisplay, IO[TestSummary], IO[TestSummary])] =
    for {
      eventQueue <- cats.effect.std.Queue.unbounded[IO, Option[TestEvent]]
      state <- Ref.of[IO, DisplayState](DisplayState.empty)
      startTime <- IO.realTime.map(_.toMillis)
      // Start the fancy display in a background fiber
      fancyFiber <- FancyTestDisplay.run(eventQueue, testRunState).start
    } yield {
      val display = new FancyBridgeDisplay(eventQueue, state, startTime)
      // Signal completion to the fancy display, then wait for summary
      val signalCompletionAndWait = for {
        _ <- eventQueue.offer(None)
        summary <- fancyFiber.joinWithNever
      } yield summary
      // Just wait for the fiber (for when user presses 'q')
      val waitForUserQuit = fancyFiber.joinWithNever
      (display, signalCompletionAndWait, waitForUserQuit)
    }

  private class FancyBridgeDisplay(
      eventQueue: cats.effect.std.Queue[IO, Option[TestEvent]],
      state: Ref[IO, DisplayState],
      startTime: Long
  ) extends TestDisplay {

    override def handle(event: TestEvent): IO[Unit] =
      for {
        // Update local state for summary
        _ <- updateState(event)
        // Forward to fancy display
        _ <- eventQueue.offer(Some(event))
      } yield ()

    private def updateState(event: TestEvent): IO[Unit] = event match {
      case TestEvent.CompileStarted(project, _) =>
        state.update(s => s.copy(currentlyCompiling = s.currentlyCompiling + project))

      case TestEvent.CompileFinished(project, _, _, _, _) =>
        state.update(s => s.copy(currentlyCompiling = s.currentlyCompiling - project))

      case TestEvent.SuiteStarted(project, suite, _) =>
        val key = s"$project:$suite"
        state.update(s =>
          s.copy(
            suitesTotal = s.suitesTotal + 1,
            currentlyRunning = s.currentlyRunning + key
          )
        )

      case TestEvent.TestStarted(project, suite, test, _) =>
        val key = s"$project:$suite:$test"
        state.update(s =>
          s.copy(
            testsTotal = s.testsTotal + 1,
            currentlyRunning = s.currentlyRunning + key
          )
        )

      case TestEvent.TestFinished(project, suite, test, status, durationMs, message, _, _) =>
        val key = s"$project:$suite:$test"
        state.update { s =>
          val updatedFailures = status match {
            case TestStatus.Failed | TestStatus.Error | TestStatus.Timeout =>
              val output = s.pendingOutput.getOrElse(s"$project:$suite", Nil)
              TestFailure(project, suite, test, message, None, output) :: s.failures
            case _ => s.failures
          }

          val updatedSkipped = status match {
            case TestStatus.Skipped | TestStatus.Cancelled | TestStatus.Ignored | TestStatus.Pending =>
              TestSkipped(project, suite, test, status) :: s.skipped
            case _ => s.skipped
          }

          s.copy(
            currentlyRunning = s.currentlyRunning - key,
            testsPassed = s.testsPassed + (if (status == TestStatus.Passed) 1 else 0),
            testsFailed = s.testsFailed + (if (status == TestStatus.Failed || status == TestStatus.Error || status == TestStatus.Timeout) 1 else 0),
            testsSkipped = s.testsSkipped + (if (status == TestStatus.Skipped || status == TestStatus.Cancelled) 1 else 0),
            testsIgnored = s.testsIgnored + (if (status == TestStatus.Ignored || status == TestStatus.Pending) 1 else 0),
            failures = updatedFailures,
            skipped = updatedSkipped,
            totalTestTimeMs = s.totalTestTimeMs + durationMs
          )
        }

      case TestEvent.SuiteFinished(project, suite, _, failed, _, _, _, _) =>
        val key = s"$project:$suite"
        state.update { s =>
          s.copy(
            suitesCompleted = s.suitesCompleted + 1,
            suitesFailed = s.suitesFailed + (if (failed > 0) 1 else 0),
            currentlyRunning = s.currentlyRunning - key,
            pendingOutput = s.pendingOutput - key
          )
        }

      case TestEvent.Output(project, suite, line, _, _) =>
        val key = s"$project:$suite"
        state.update(s =>
          s.copy(
            pendingOutput = s.pendingOutput.updated(
              key,
              s.pendingOutput.getOrElse(key, Nil) :+ line
            )
          )
        )

      case TestEvent.SuitesDiscovered(_, _) =>
        IO.unit

      case TestEvent.ProjectSkipped(_, _, _) =>
        IO.unit // Forwarded to fancy display, no local state update needed

      case TestEvent.CompileProgress(_, _, _) =>
        IO.unit // Forwarded to fancy display, no local state update needed

      case TestEvent.SuiteTimedOut(project, suite, timeoutMs, threadDumpInfo, _) =>
        val key = s"$project:$suite"
        state.update { s =>
          val timeoutFailure = TestFailure(
            project = project,
            suite = suite,
            test = "(timeout)",
            message = Some(s"Suite timed out after ${timeoutMs / 1000}s"),
            throwable = threadDumpInfo.flatMap(_.singleThreadStack),
            output = threadDumpInfo.flatMap(_.dumpFile).map(p => s"Thread dump: $p").toList
          )
          s.copy(
            suitesCompleted = s.suitesCompleted + 1,
            suitesFailed = s.suitesFailed + 1,
            testsFailed = s.testsFailed + 1,
            currentlyRunning = s.currentlyRunning - key,
            failures = timeoutFailure :: s.failures
          )
        }
    }

    override def summary: IO[TestSummary] =
      for {
        s <- state.get
        now <- IO.realTime.map(_.toMillis)
      } yield TestSummary(
        suitesTotal = s.suitesTotal,
        suitesCompleted = s.suitesCompleted,
        suitesFailed = s.suitesFailed,
        testsTotal = s.testsTotal,
        testsPassed = s.testsPassed,
        testsFailed = s.testsFailed,
        testsSkipped = s.testsSkipped,
        testsIgnored = s.testsIgnored,
        currentlyRunning = s.currentlyRunning.toList,
        failures = s.failures.reverse,
        skipped = s.skipped.reverse,
        durationMs = now - startTime,
        totalTestTimeMs = s.totalTestTimeMs
      )

    override def printSummary: IO[Unit] = IO.unit // Fancy display handles this
  }
}
