package bleep.testing

import cats.effect._
import cats.effect.std.Queue
import cats.syntax.all._
import tui._
import tui.crossterm.CrosstermJni
import tui.widgets._

import java.time.{Duration, Instant}
import scala.collection.mutable

/** Terminal UI for test execution - functional and readable */
object FancyTestDisplay {

  // ASCII spinners that work reliably
  private val spinnerFrames = Array("|", "/", "-", "\\")

  // ASCII indicators
  private val passedIcon = "+"
  private val failedIcon = "x"
  private val skippedIcon = "o"
  private val runningIcon = "*"
  private val compilingIcon = ">"

  // Color palette using basic 16 colors
  object Palette {
    val success = Color.Green
    val error = Color.LightRed
    val warning = Color.Yellow
    val info = Color.Cyan
    val text = Color.White
    val textMuted = Color.Gray
    val textDim = Color.DarkGray
    val border = Color.DarkGray
    val accent = Color.Magenta
  }

  case class State(
      testsTotal: Int = 0,
      testsPassed: Int = 0,
      testsFailed: Int = 0,
      testsSkipped: Int = 0,
      testsIgnored: Int = 0,
      suitesTotal: Int = 0,
      suitesCompleted: Int = 0,
      suitesFailed: Int = 0,
      suitesDiscovered: Int = 0,
      compilingProjects: mutable.LinkedHashMap[String, CompilingProject] = mutable.LinkedHashMap.empty,
      compiledProjects: Int = 0,
      compiledFailed: Int = 0,
      compileFailures: mutable.ArrayDeque[CompileFailureEntry] = mutable.ArrayDeque.empty,
      skippedProjects: mutable.ArrayDeque[SkippedProjectEntry] = mutable.ArrayDeque.empty,
      runningTests: mutable.LinkedHashMap[String, RunningTest] = mutable.LinkedHashMap.empty,
      runningSuites: mutable.LinkedHashMap[String, RunningSuite] = mutable.LinkedHashMap.empty,
      failures: mutable.ArrayDeque[FailureEntry] = mutable.ArrayDeque.empty,
      skippedTests: mutable.ArrayDeque[SkippedEntry] = mutable.ArrayDeque.empty,
      recentPasses: mutable.ArrayDeque[String] = mutable.ArrayDeque.empty,
      startTime: Instant = Instant.now(),
      frame: Int = 0,
      totalTestTimeMs: Long = 0
  ) {
    def suiteProgress: Double = {
      val total = math.max(suitesDiscovered, suitesTotal)
      if (total == 0) 0.0
      else suitesCompleted.toDouble / total.toDouble
    }

    def effectiveSuiteCount: Int = math.max(suitesDiscovered, suitesTotal)

    def elapsedMs: Long =
      java.time.Duration.between(startTime, Instant.now()).toMillis

    def advanceFrame(): State =
      copy(frame = frame + 1)

    def spinner: String = spinnerFrames(frame % spinnerFrames.length)
  }

  case class RunningTest(
      project: String,
      suite: String,
      test: String,
      startTime: Instant
  ) {
    def elapsedMs: Long = java.time.Duration.between(startTime, Instant.now()).toMillis
  }

  case class RunningSuite(
      project: String,
      suite: String,
      startTime: Instant,
      jvmPid: Long = 0L,
      testsRun: Int = 0,
      testsPassed: Int = 0,
      testsFailed: Int = 0
  ) {
    def elapsedMs: Long = java.time.Duration.between(startTime, Instant.now()).toMillis
  }

  case class FailureEntry(
      project: String,
      suite: String,
      test: String,
      message: Option[String],
      throwable: Option[String],
      timestamp: Instant
  )

  case class SkippedEntry(
      project: String,
      suite: String,
      test: String,
      status: TestStatus
  )

  case class CompilingProject(
      name: String,
      startTime: Instant,
      progress: Option[Int] = None
  ) {
    def elapsedMs: Long = java.time.Duration.between(startTime, Instant.now()).toMillis
  }

  case class CompileFailureEntry(
      project: String,
      errors: List[String],
      timestamp: Instant
  )

  case class SkippedProjectEntry(
      project: String,
      reason: String,
      timestamp: Instant
  )

  private def logError(msg: String, e: Throwable): Unit = {
    val errorFile = java.nio.file.Paths.get("/tmp/tui-error.log")
    val sw = new java.io.StringWriter()
    val pw = new java.io.PrintWriter(sw)
    pw.println(s"$msg at ${java.time.Instant.now()}: ${e.getMessage}")
    e.printStackTrace(pw)
    java.nio.file.Files.write(
      errorFile,
      sw.toString.getBytes,
      java.nio.file.StandardOpenOption.CREATE,
      java.nio.file.StandardOpenOption.APPEND
    )
    ()
  }

  /** Check if TUI mode is supported (terminal with raw mode support) */
  def isSupported: Boolean = {
    // CrosstermJni doesn't work on Windows - it tries to register SIGWINCH handler
    val isWindows = System.getProperty("os.name", "").toLowerCase.contains("win")
    if (isWindows) {
      false
    } else {
      try {
        val jni = new CrosstermJni
        jni.enableRawMode()
        jni.disableRawMode()
        true
      } catch {
        case _: Throwable => false
      }
    }
  }

  /** Result of TUI run - either success with summary or failure with error and partial summary */
  case class TuiResult(
      summary: TestSummary,
      error: Option[Throwable]
  )

  def run(
      eventQueue: Queue[IO, Option[TestEvent]],
      testRunState: Option[Ref[IO, TestRunState]]
  ): IO[TestSummary] =
    runSafe(eventQueue, testRunState).flatMap { result =>
      result.error match {
        case Some(e) =>
          // Log error but return summary - terminal is already restored
          IO(logError("TUI encountered error", e)) >> IO.pure(result.summary)
        case None =>
          IO.pure(result.summary)
      }
    }

  /** Run TUI safely - always restores terminal, returns errors as data */
  private def runSafe(
      eventQueue: Queue[IO, Option[TestEvent]],
      testRunState: Option[Ref[IO, TestRunState]]
  ): IO[TuiResult] =
    IO.blocking {
      var jni: CrosstermJni = null
      var backend: CrosstermBackend = null
      var error: Option[Throwable] = None
      var summary: TestSummary = TestSummary.empty

      try {
        jni = new CrosstermJni
        jni.enableRawMode()
        jni.execute(new tui.crossterm.Command.EnterAlternateScreen(), new tui.crossterm.Command.EnableMouseCapture())

        backend = new CrosstermBackend(jni)
        val terminal = Terminal.init(backend)

        summary = runLoop(jni, terminal, eventQueue, testRunState)
      } catch {
        case e: Throwable =>
          error = Some(e)
          logError("TUI error", e)
      } finally {
        // ALWAYS restore terminal - this is critical
        try
          if (jni != null) {
            jni.disableRawMode()
            jni.execute(new tui.crossterm.Command.LeaveAlternateScreen(), new tui.crossterm.Command.DisableMouseCapture())
          }
        catch {
          case e: Throwable => logError("Failed to restore terminal", e)
        }
        try
          if (backend != null) backend.showCursor()
        catch {
          case _: Throwable => ()
        }
      }

      TuiResult(summary, error)
    }

  private def runLoop(
      jni: CrosstermJni,
      terminal: Terminal,
      eventQueue: Queue[IO, Option[TestEvent]],
      testRunStateOpt: Option[Ref[IO, TestRunState]]
  ): TestSummary = {
    import cats.effect.unsafe.implicits.global

    var state = State()
    var done = false
    var userCancelled = false
    val tickRate = Duration.ofMillis(100)
    var lastTick = Instant.now()

    while (!done) {
      // Read TestRunState if available for rich project display
      val displayItems: List[ProjectDisplayItem] = testRunStateOpt match {
        case Some(ref) =>
          val testState = ref.get.unsafeRunSync()
          ProjectDisplayItem.fromState(testState, System.currentTimeMillis())
        case None =>
          Nil
      }

      try
        terminal.draw(f => renderUI(f, state, displayItems))
      catch {
        case e: Throwable =>
          logError("Render error", e)
      }

      val elapsed = java.time.Duration.between(lastTick, Instant.now())
      val timeout = {
        val remaining = tickRate.minus(elapsed)
        if (remaining.isNegative) Duration.ZERO else remaining
      }

      if (jni.poll(new tui.crossterm.Duration(0, timeout.toNanos.toInt.max(1)))) {
        jni.read() match {
          case key: tui.crossterm.Event.Key =>
            key.keyEvent.code match {
              case char: tui.crossterm.KeyCode.Char if char.c() == 'q' || char.c() == 'Q' =>
                done = true
                userCancelled = true
              case _: tui.crossterm.KeyCode.Esc =>
                done = true
                userCancelled = true
              case _ =>
                key.keyEvent.code match {
                  case char: tui.crossterm.KeyCode.Char if char.c().toInt == 3 =>
                    done = true
                    userCancelled = true
                  case _ => ()
                }
            }
          case _ => ()
        }
      }

      var moreEvents = true
      while (moreEvents && !done)
        eventQueue.tryTake.unsafeRunSync() match {
          case Some(Some(event)) =>
            state = processEvent(state, event)
          case Some(None) =>
            done = true
          case None =>
            moreEvents = false
        }

      if (elapsed.compareTo(tickRate) >= 0) {
        state = state.advanceFrame()
        lastTick = Instant.now()
      }
    }

    TestSummary(
      suitesTotal = state.suitesTotal,
      suitesCompleted = state.suitesCompleted,
      suitesFailed = state.suitesFailed,
      testsTotal = state.testsTotal,
      testsPassed = state.testsPassed,
      testsFailed = state.testsFailed,
      testsSkipped = state.testsSkipped,
      testsIgnored = state.testsIgnored,
      // Use runningSuites for cancellation tracking (format: "project:suite")
      // runningTests might be empty between tests even when suite is running
      currentlyRunning = state.runningSuites.keys.toList,
      failures = state.failures
        .map(f =>
          TestFailure(
            project = f.project,
            suite = f.suite,
            test = f.test,
            message = f.message,
            throwable = f.throwable,
            output = Nil
          )
        )
        .toList,
      skipped = state.skippedTests
        .map(s =>
          TestSkipped(
            project = s.project,
            suite = s.suite,
            test = s.test,
            status = s.status
          )
        )
        .toList,
      durationMs = state.elapsedMs,
      totalTestTimeMs = state.totalTestTimeMs,
      wasCancelled = userCancelled
    )
  }

  private def processEvent(state: State, event: TestEvent): State =
    event match {
      case TestEvent.CompileStarted(project, timestamp) =>
        state.compilingProjects.put(project, CompilingProject(project, Instant.ofEpochMilli(timestamp)))
        state

      case TestEvent.CompileFinished(project, success, _, _, errors) =>
        state.compilingProjects.remove(project)
        if (!success) {
          state.compileFailures.prepend(CompileFailureEntry(project, errors, Instant.now()))
        }
        state.copy(
          compiledProjects = state.compiledProjects + 1,
          compiledFailed = state.compiledFailed + (if (!success) 1 else 0)
        )

      case TestEvent.SuiteStarted(project, suite, timestamp) =>
        val key = s"$project:$suite"
        state.runningSuites.put(key, RunningSuite(project, suite, Instant.ofEpochMilli(timestamp)))
        state.copy(suitesTotal = state.suitesTotal + 1)

      case TestEvent.TestStarted(project, suite, test, timestamp) =>
        val key = s"$project:$suite:$test"
        state.runningTests.put(key, RunningTest(project, suite, test, Instant.ofEpochMilli(timestamp)))
        state.copy(testsTotal = state.testsTotal + 1)

      case TestEvent.TestFinished(project, suite, test, status, durationMs, message, throwable, _) =>
        val key = s"$project:$suite:$test"
        state.runningTests.remove(key)

        val suiteKey = s"$project:$suite"
        state.runningSuites.get(suiteKey).foreach { rs =>
          state.runningSuites.put(
            suiteKey,
            rs.copy(
              testsRun = rs.testsRun + 1,
              testsPassed = rs.testsPassed + (if (status == TestStatus.Passed) 1 else 0),
              testsFailed = rs.testsFailed + (if (status == TestStatus.Failed || status == TestStatus.Error) 1 else 0)
            )
          )
        }

        val baseState = state.copy(totalTestTimeMs = state.totalTestTimeMs + durationMs)

        status match {
          case TestStatus.Passed =>
            state.recentPasses.prepend(s"$suite.$test")
            if (state.recentPasses.size > 5) state.recentPasses.removeLast()
            baseState.copy(testsPassed = baseState.testsPassed + 1)

          case TestStatus.Failed | TestStatus.Error | TestStatus.Timeout =>
            state.failures.prepend(FailureEntry(project, suite, test, message, throwable, Instant.now()))
            baseState.copy(testsFailed = baseState.testsFailed + 1)

          case TestStatus.Skipped | TestStatus.Cancelled =>
            state.skippedTests.prepend(SkippedEntry(project, suite, test, status))
            baseState.copy(testsSkipped = baseState.testsSkipped + 1)

          case TestStatus.Ignored | TestStatus.Pending =>
            state.skippedTests.prepend(SkippedEntry(project, suite, test, status))
            baseState.copy(testsIgnored = baseState.testsIgnored + 1)
        }

      case TestEvent.SuiteFinished(project, suite, _, failed, _, _, _, _) =>
        val key = s"$project:$suite"
        state.runningSuites.remove(key)
        state.copy(
          suitesCompleted = state.suitesCompleted + 1,
          suitesFailed = state.suitesFailed + (if (failed > 0) 1 else 0)
        )

      case TestEvent.Output(_, _, _, _, _) =>
        state

      case TestEvent.SuitesDiscovered(count, _) =>
        state.copy(suitesDiscovered = count)

      case TestEvent.ProjectSkipped(project, reason, timestamp) =>
        state.skippedProjects.prepend(SkippedProjectEntry(project, reason, Instant.ofEpochMilli(timestamp)))
        state

      case TestEvent.CompileProgress(project, percent, _) =>
        // Update the compiling project with its progress
        state.compilingProjects.get(project) match {
          case Some(cp) =>
            state.compilingProjects.put(project, cp.copy(progress = Some(percent)))
          case None =>
            () // Project not found, ignore
        }
        state

      case TestEvent.SuiteTimedOut(project, suite, timeoutMs, threadDumpInfo, timestamp) =>
        val key = s"$project:$suite"
        state.runningSuites.remove(key)
        // Add timeout as a failure
        val errorMsg = s"Suite timed out after ${timeoutMs / 1000}s"
        val details = threadDumpInfo
          .map { info =>
            info.singleThreadStack.getOrElse(
              info.dumpFile.map(p => s"Thread dump: $p").getOrElse("No thread dump available")
            )
          }
          .getOrElse("")
        state.failures.prepend(FailureEntry(project, suite, "(timeout)", Some(errorMsg), Some(details), Instant.ofEpochMilli(timestamp)))
        state.copy(
          suitesCompleted = state.suitesCompleted + 1,
          suitesFailed = state.suitesFailed + 1,
          testsFailed = state.testsFailed + 1
        )
    }

  private def renderUI(f: Frame, state: State, displayItems: List[ProjectDisplayItem]): Unit = {
    if (f.size.height < 10 || f.size.width < 40) {
      val msg = ParagraphWidget(
        text = Text.nostyle("Terminal too small"),
        alignment = Alignment.Center
      )
      f.renderWidget(msg, f.size)
      return
    }

    val issueCount = state.compileFailures.size + state.skippedProjects.size + state.failures.size

    // Calculate how many lines Activity needs
    val activityLinesNeeded = displayItems.map {
      case ProjectDisplayItem.Compiling(_, _)                            => 1
      case ProjectDisplayItem.Testing.Reactive(_, _, _, _, runningTests) => 1 + runningTests.size
      case ProjectDisplayItem.Testing.Bsp(_, _, _)                       => 1
      case ProjectDisplayItem.Waiting(_)                                 => 1
    }.sum + 2 // +2 for border

    // Layout
    val headerHeight = 3
    val statsHeight = 3
    val footerHeight = 1

    val middleHeight = math.max(4, f.size.height - headerHeight - statsHeight - footerHeight)
    val halfHeight = middleHeight / 2
    val minIssuesHeight = if (issueCount > 0) 4 else 2 // Issues always gets at least 4 lines if there are any

    // If neither needs more than half, split 50/50. Otherwise Activity takes what it needs.
    val (topMiddle, bottomMiddle) =
      if (activityLinesNeeded <= halfHeight) {
        // Activity doesn't need more than half, so split evenly
        (halfHeight, middleHeight - halfHeight)
      } else {
        // Activity needs more than half, give it what it needs (but leave room for Issues minimum)
        val maxActivityHeight = middleHeight - minIssuesHeight
        val activityHeight = math.max(3, math.min(activityLinesNeeded, maxActivityHeight))
        (activityHeight, middleHeight - activityHeight)
      }

    val chunks = Layout(
      direction = Direction.Vertical,
      constraints = Array(
        Constraint.Length(headerHeight),
        Constraint.Length(statsHeight),
        Constraint.Length(topMiddle),
        Constraint.Length(bottomMiddle),
        Constraint.Length(footerHeight)
      )
    ).split(f.size)

    renderHeader(f, chunks(0), state)
    renderStats(f, chunks(1), state)
    renderActivity(f, chunks(2), state, displayItems)
    renderIssues(f, chunks(3), state)
    renderFooter(f, chunks(4), state)
  }

  private def renderHeader(f: Frame, area: Rect, state: State): Unit = {
    val elapsed = formatDuration(state.elapsedMs)
    val progress = (state.suiteProgress * 100).toInt
    val suiteTotal = state.effectiveSuiteCount

    val title = s"BLEEP TEST RUNNER - ${state.suitesCompleted}/$suiteTotal suites ($progress%) - $elapsed"

    val paragraph = ParagraphWidget(
      text = Text.fromSpans(
        Spans.from(Span.styled(title, Style(fg = Some(Palette.info), addModifier = Modifier.BOLD)))
      ),
      block = Some(
        BlockWidget(
          borders = Borders.ALL,
          borderStyle = Style(fg = Some(Palette.border)),
          borderType = BlockWidget.BorderType.Rounded
        )
      ),
      alignment = Alignment.Center
    )
    f.renderWidget(paragraph, area)
  }

  private def renderStats(f: Frame, area: Rect, state: State): Unit = {
    val columns = Layout(
      direction = Direction.Horizontal,
      constraints = Array(
        Constraint.Percentage(25),
        Constraint.Percentage(25),
        Constraint.Percentage(25),
        Constraint.Percentage(25)
      )
    ).split(area)

    def statWidget(icon: String, label: String, value: Int, color: Color): ParagraphWidget =
      ParagraphWidget(
        text = Text.fromSpans(
          Spans.from(
            Span.styled(s"$icon $label: ", Style(fg = Some(Palette.textMuted))),
            Span.styled(value.toString, Style(fg = Some(color), addModifier = Modifier.BOLD))
          )
        ),
        alignment = Alignment.Center
      )

    f.renderWidget(statWidget(passedIcon, "PASS", state.testsPassed, Palette.success), columns(0))
    f.renderWidget(
      statWidget(failedIcon, "FAIL", state.testsFailed, if (state.testsFailed > 0) Palette.error else Palette.textDim),
      columns(1)
    )
    f.renderWidget(
      statWidget(skippedIcon, "SKIP", state.testsSkipped, if (state.testsSkipped > 0) Palette.warning else Palette.textDim),
      columns(2)
    )
    f.renderWidget(statWidget(runningIcon, "RUN", state.runningSuites.size, Palette.info), columns(3))
  }

  private def renderActivity(f: Frame, area: Rect, state: State, displayItems: List[ProjectDisplayItem]): Unit = {
    val maxItems = math.max(1, area.height - 2)

    // Build list items from ProjectDisplayItem ADT
    val items = scala.collection.mutable.ArrayBuffer.empty[ListWidget.Item]
    var itemCount = 0

    displayItems.foreach {
      case ProjectDisplayItem.Compiling(project, percent) if itemCount < maxItems =>
        val progressStr = percent.map(p => s" $p%").getOrElse("")
        items += ListWidget.Item(
          Text.fromSpans(
            Spans.from(
              Span.styled(s" $compilingIcon ", Style(fg = Some(Palette.accent))),
              Span.styled("BUILD ", Style(fg = Some(Palette.accent))),
              Span.styled(project.value, Style(fg = Some(Palette.text))),
              Span.styled(progressStr, Style(fg = Some(Palette.textDim)))
            )
          )
        )
        itemCount += 1

      case ProjectDisplayItem.Testing.Reactive(project, suitesCompleted, suitesTotal, failures, runningTests) if itemCount < maxItems =>
        // Project header with progress
        val progressStr = s"$suitesCompleted/$suitesTotal"
        val progressPercent = if (suitesTotal > 0) (suitesCompleted * 100) / suitesTotal else 0
        val failureStr = if (failures > 0) s" ${failedIcon}$failures" else ""
        val spinnerIdx = state.frame % spinnerFrames.length
        val spinner = spinnerFrames(spinnerIdx)

        items += ListWidget.Item(
          Text.fromSpans(
            Spans.from(
              Span.styled(s" $spinner ", Style(fg = Some(Palette.info))),
              Span.styled(project.value, Style(fg = Some(Palette.accent))),
              Span.styled(s" $progressStr ($progressPercent%)", Style(fg = Some(Palette.text))),
              Span.styled(failureStr, Style(fg = Some(if (failures > 0) Palette.error else Palette.textDim)))
            )
          )
        )
        itemCount += 1

        // Running tests under the project
        runningTests.take(maxItems - itemCount).foreach { rt =>
          val elapsed = formatDuration(rt.elapsedMs)
          val testDisplay = if (rt.testName.value.nonEmpty) {
            s"${rt.suiteName.shortName}.${rt.testName.value}"
          } else {
            rt.suiteName.shortName
          }
          items += ListWidget.Item(
            Text.fromSpans(
              Spans.from(
                Span.styled("    ", Style.DEFAULT),
                Span.styled(s"[${rt.jvmPid}] ", Style(fg = Some(Palette.textDim))),
                Span.styled(testDisplay, Style(fg = Some(Palette.text))),
                Span.styled(s" ($elapsed)", Style(fg = Some(Palette.textDim)))
              )
            )
          )
          itemCount += 1
        }

      case ProjectDisplayItem.Testing.Bsp(project, platform, elapsedMs) if itemCount < maxItems =>
        // BSP testing (JS/Native) - project-level only
        val elapsed = formatDuration(elapsedMs)
        val spinnerIdx = state.frame % spinnerFrames.length
        val spinner = spinnerFrames(spinnerIdx)
        val platformStr = platform.value.toUpperCase

        items += ListWidget.Item(
          Text.fromSpans(
            Spans.from(
              Span.styled(s" $spinner ", Style(fg = Some(Palette.info))),
              Span.styled(project.value, Style(fg = Some(Palette.accent))),
              Span.styled(s" [$platformStr]", Style(fg = Some(Palette.textDim))),
              Span.styled(s" ($elapsed)", Style(fg = Some(Palette.textDim)))
            )
          )
        )
        itemCount += 1

      case ProjectDisplayItem.Waiting(count) if itemCount < maxItems =>
        items += ListWidget.Item(
          Text.fromSpans(
            Spans.from(
              Span.styled(s" $count projects waiting", Style(fg = Some(Palette.textDim)))
            )
          )
        )
        itemCount += 1

      case _ => // Skip if we've hit the limit
    }

    val compilingCount = displayItems.count(_.isInstanceOf[ProjectDisplayItem.Compiling])
    val testingCount = displayItems.count(_.isInstanceOf[ProjectDisplayItem.Testing])
    val title =
      if (compilingCount > 0 && testingCount > 0) s"Building $compilingCount + Testing $testingCount"
      else if (compilingCount > 0) s"Building $compilingCount"
      else if (testingCount > 0) s"Testing $testingCount projects"
      else "Waiting..."

    val list = ListWidget(
      items = items.toArray,
      block = Some(
        BlockWidget(
          title = Some(Spans.styled(title, Style(fg = Some(Palette.info)))),
          borders = Borders.ALL,
          borderStyle = Style(fg = Some(Palette.border)),
          borderType = BlockWidget.BorderType.Rounded
        )
      )
    )
    f.renderWidget(list, area)
  }

  private def renderIssues(f: Frame, area: Rect, state: State): Unit = {
    val maxItems = math.max(1, area.height - 2)

    val compileItems = state.compileFailures.take(maxItems / 3).toArray.map { cf =>
      val errorPreview = cf.errors.headOption.map(_.take(200)).getOrElse("Compilation failed")
      ListWidget.Item(
        Text.fromSpans(
          Spans.from(
            Span.styled(s" $failedIcon COMPILE ", Style(fg = Some(Palette.error))),
            Span.styled(cf.project, Style(fg = Some(Palette.accent))),
            Span.styled(s": $errorPreview", Style(fg = Some(Palette.textDim)))
          )
        )
      )
    }

    val remaining1 = maxItems - compileItems.length
    val skippedItems = state.skippedProjects.take(remaining1 / 2).toArray.map { sp =>
      ListWidget.Item(
        Text.fromSpans(
          Spans.from(
            Span.styled(s" $skippedIcon SKIPPED ", Style(fg = Some(Palette.warning))),
            Span.styled(sp.project, Style(fg = Some(Palette.accent))),
            Span.styled(s": ${sp.reason.take(150)}", Style(fg = Some(Palette.textDim)))
          )
        )
      )
    }

    val remaining2 = remaining1 - skippedItems.length

    // Show test failures newest first (they're prepended to the list)
    val testItems = scala.collection.mutable.ArrayBuffer.empty[ListWidget.Item]
    var testItemCount = 0

    // Iterate in natural order (newest first since failures are prepended)
    state.failures.iterator.takeWhile(_ => testItemCount < remaining2).foreach { failure =>
      // Test name line with project prefix
      testItems += ListWidget.Item(
        Text.fromSpans(
          Spans.from(
            Span.styled(s" $failedIcon ", Style(fg = Some(Palette.error))),
            Span.styled(s"${failure.project} ", Style(fg = Some(Palette.accent))),
            Span.styled(s"${failure.suite}.${failure.test}", Style(fg = Some(Palette.text)))
          )
        )
      )
      testItemCount += 1

      // Error message line (indented)
      if (testItemCount < remaining2) {
        val msg = failure.message.getOrElse("(no message)")
        testItems += ListWidget.Item(
          Text.fromSpans(
            Spans.from(
              Span.styled("    ", Style.DEFAULT),
              Span.styled(msg.take(300), Style(fg = Some(Palette.textMuted)))
            )
          )
        )
        testItemCount += 1
      }
    }

    val items = compileItems ++ skippedItems ++ testItems.toArray
    val totalIssues = state.compileFailures.size + state.skippedProjects.size + state.failures.size
    val title = if (totalIssues == 0) "No Issues" else s"Issues ($totalIssues)"
    val borderColor = if (totalIssues > 0) Palette.error else Palette.textDim

    val list = ListWidget(
      items = items,
      block = Some(
        BlockWidget(
          title = Some(Spans.styled(title, Style(fg = Some(borderColor)))),
          borders = Borders.ALL,
          borderStyle = Style(fg = Some(borderColor)),
          borderType = BlockWidget.BorderType.Rounded
        )
      )
    )
    f.renderWidget(list, area)
  }

  private def renderFooter(f: Frame, area: Rect, state: State): Unit = {
    val wallTimeMs = state.elapsedMs
    val totalTestTimeMs = state.totalTestTimeMs
    val speedup =
      if (wallTimeMs > 0 && totalTestTimeMs > wallTimeMs) f"${totalTestTimeMs.toDouble / wallTimeMs}%.1fx"
      else "-"

    val footer = s"q/ESC: exit | Compiled: ${state.compiledProjects} | Speedup: $speedup"

    val paragraph = ParagraphWidget(
      text = Text.fromSpans(Spans.styled(footer, Style(fg = Some(Palette.textDim)))),
      alignment = Alignment.Center
    )
    f.renderWidget(paragraph, area)
  }

  private def formatDuration(ms: Long): String =
    if (ms < 1000) s"${ms}ms"
    else if (ms < 60000) f"${ms / 1000.0}%.1fs"
    else {
      val minutes = ms / 60000
      val seconds = (ms % 60000) / 1000
      f"${minutes}m ${seconds}s"
    }
}
