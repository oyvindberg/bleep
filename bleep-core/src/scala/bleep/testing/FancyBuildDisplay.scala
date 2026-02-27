package bleep.testing

import bleep.bsp.protocol.BleepBspProtocol.BuildMode
import cats.effect._
import cats.effect.std.Queue
import tui._
import tui.crossterm.CrosstermJni
import tui.widgets._

import java.time.{Duration, Instant}
import scala.collection.mutable

/** Terminal UI for build execution - functional and readable */
object FancyBuildDisplay {

  private val spinnerFrames = Array("⠋", "⠙", "⠹", "⠸", "⠼", "⠴", "⠦", "⠧", "⠇", "⠏")

  // Icons must be width-1 per Wcwidth to avoid rendering artifacts.
  // Dingbats (U+2700-27BF) and U+25B6 are width-2 — avoid them.
  private val passedIcon = "◆" // U+25C6 BLACK DIAMOND — width 1
  private val failedIcon = "◇" // U+25C7 WHITE DIAMOND  — width 1
  private val skippedIcon = "○" // U+25CB WHITE CIRCLE   — width 1
  private val runningIcon = "●" // U+25CF BLACK CIRCLE   — width 1
  private val compilingIcon = "▸" // U+25B8 SM RIGHT TRIANGLE — width 1

  // Dark theme using 256-color indexed palette for reliable rendering.
  // Grayscale ramp (232-255) for backgrounds, 6x6x6 cube for accents.
  object Palette {
    val bg = Color.Indexed(234) // #1c1c1c — near-black base
    val surface = Color.Indexed(236) // #303030 — raised surface
    val text = Color.Indexed(253) // #dadada — primary text
    val textMuted = Color.Indexed(249) // #b2b2b2 — secondary text
    val textDim = Color.Indexed(243) // #767676 — tertiary / timestamps
    val border = Color.Indexed(240) // #585858 — borders
    val success = Color.Indexed(114) // rgb(135,215,135) — soft green
    val error = Color.Indexed(210) // rgb(255,135,135) — coral red
    val warning = Color.Indexed(222) // rgb(255,215,135) — warm amber
    val info = Color.Indexed(111) // rgb(135,175,255) — soft blue
    val accent = Color.Indexed(183) // rgb(215,175,255) — lavender
  }

  // Style helpers — every cell gets our bg so the terminal theme never bleeds through
  private def s(color: Color): Style = Style(fg = Some(color), bg = Some(Palette.bg))
  private def sb(color: Color): Style = Style(fg = Some(color), bg = Some(Palette.bg), addModifier = Modifier.BOLD)
  private val sBg: Style = Style(bg = Some(Palette.bg))
  // Surface variants for raised panels (summary block)
  private def ss(color: Color): Style = Style(fg = Some(color), bg = Some(Palette.surface))
  private def ssb(color: Color): Style = Style(fg = Some(color), bg = Some(Palette.surface), addModifier = Modifier.BOLD)
  private val sSurface: Style = Style(bg = Some(Palette.surface))

  /** Info about workspace being busy (another connection is building) */
  case class WorkspaceBusyInfo(
      operation: String,
      projects: List[String],
      startedAgoMs: Long,
      receivedAt: Instant,
      cancelRequested: Boolean
  )

  case class State(
      core: BuildState = BuildState.empty,
      mode: BuildMode = BuildMode.Test,
      suitesDiscovered: Int = 0,
      compilingProjects: mutable.LinkedHashMap[String, CompilingProject] = mutable.LinkedHashMap.empty,
      runningTests: mutable.LinkedHashMap[String, RunningTest] = mutable.LinkedHashMap.empty,
      runningSuites: mutable.LinkedHashMap[String, RunningSuite] = mutable.LinkedHashMap.empty,
      recentPasses: mutable.ArrayDeque[String] = mutable.ArrayDeque.empty,
      stalledProjects: Map[String, BuildEvent.CompileStalled] = Map.empty,
      lockedProjects: Map[String, BuildEvent.LockContention] = Map.empty,
      startTime: Instant = Instant.now(),
      frame: Int = 0,
      issueScrollOffset: Int = 0,
      issueScrollAtBottom: Boolean = false,
      workspaceBusy: Option[WorkspaceBusyInfo] = None
  ) {
    def suiteProgress: Double = {
      val total = math.max(suitesDiscovered, core.suitesTotal)
      if (total == 0) 0.0
      else core.suitesCompleted.toDouble / total.toDouble
    }

    def effectiveSuiteCount: Int = math.max(suitesDiscovered, core.suitesTotal)

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

  case class CompilingProject(
      name: String,
      startTime: Instant,
      progress: Option[Int],
      reason: Option[String],
      phase: Option[String]
  ) {
    def elapsedMs: Long = java.time.Duration.between(startTime, Instant.now()).toMillis
  }

  /** Check if TUI mode is supported (terminal with raw mode support) */
  /** Check if TUI mode is supported. Returns Right(()) or Left(reason). */
  def checkSupport: Either[String, Unit] = {
    val isWindows = System.getProperty("os.name", "").toLowerCase.contains("win")
    if (isWindows) {
      Left("TUI not supported on Windows (CrosstermJni requires SIGWINCH)")
    } else {
      try {
        val jni = new CrosstermJni
        jni.enableRawMode()
        jni.disableRawMode()
        Right(())
      } catch {
        case e: UnsatisfiedLinkError =>
          Left(s"TUI disabled: native library not found: ${e.getMessage}")
        case e: Throwable =>
          Left(s"TUI disabled: raw mode failed: ${e.getClass.getSimpleName}: ${e.getMessage}")
      }
    }
  }

  def isSupported: Boolean = checkSupport.isRight

  def run(
      eventQueue: Queue[IO, Option[BuildEvent]],
      testRunState: Option[Ref[IO, TestRunState]],
      mode: BuildMode,
      diagnosticLog: Option[java.io.PrintWriter] = None,
      userQuitSignal: Option[Deferred[IO, Unit]] = None,
      readySignal: Option[Deferred[IO, Unit]] = None,
      cancelBlockingSignal: Option[Deferred[IO, Unit]] = None
  ): IO[BuildSummary] =
    // Wait for ready signal before entering raw mode (e.g. wait for BSP connection)
    readySignal.fold(IO.unit)(_.get) >>
      IO.blocking {
        var jni: CrosstermJni = null
        var backend: CrosstermBackend = null
        var shutdownHook: Thread = null

        def restoreTerminal(): Unit = {
          if (jni != null) {
            try {
              jni.execute(new tui.crossterm.Command.DisableMouseCapture())
              jni.disableRawMode()
              jni.execute(new tui.crossterm.Command.LeaveAlternateScreen())
            } catch {
              case _: Throwable => ()
            }
          }
          if (backend != null) {
            try backend.showCursor()
            catch {
              case _: Throwable => ()
            }
          }
        }

        try {
          jni = new CrosstermJni
          jni.enableRawMode()
          jni.execute(new tui.crossterm.Command.EnterAlternateScreen(), new tui.crossterm.Command.EnableMouseCapture())

          backend = new CrosstermBackend(jni)
          val terminal = Terminal.init(backend)

          // Add shutdown hook to restore terminal on JVM exit (belt and suspenders)
          shutdownHook = new Thread(() => restoreTerminal(), "terminal-restore")
          Runtime.getRuntime.addShutdownHook(shutdownHook)

          runLoop(jni, terminal, eventQueue, testRunState, mode, diagnosticLog, userQuitSignal, cancelBlockingSignal)
        } finally {
          // ALWAYS restore terminal - this is critical
          restoreTerminal()
          // Remove shutdown hook if we cleaned up normally
          if (shutdownHook != null) {
            try Runtime.getRuntime.removeShutdownHook(shutdownHook)
            catch { case _: IllegalStateException => () } // JVM already shutting down
          }
        }
      }

  private def runLoop(
      jni: CrosstermJni,
      terminal: Terminal,
      eventQueue: Queue[IO, Option[BuildEvent]],
      testRunStateOpt: Option[Ref[IO, TestRunState]],
      mode: BuildMode,
      diagnosticLog: Option[java.io.PrintWriter],
      userQuitSignal: Option[Deferred[IO, Unit]],
      cancelBlockingSignal: Option[Deferred[IO, Unit]]
  ): BuildSummary = {
    import cats.effect.unsafe.implicits.global

    def diagLog(msg: String): Unit = diagnosticLog.foreach { w =>
      w.println(s"[TUI] ${java.time.Instant.now()} $msg")
      w.flush()
    }

    var state = State(mode = mode)
    var done = false
    var userCancelled = false
    var allTestsComplete = false
    var finishCountdown = 0 // Count ticks after completion before auto-exit
    val autoExitDelayTicks = 20 // 2 seconds at 100ms tick rate
    val tickRate = Duration.ofMillis(100)
    var lastTick = Instant.now()
    var lastIssueItemCount = 0
    var lastIssueMaxVisible = 0
    var mouseCaptureSuspendedAt = 0L // 0 = mouse capture active; >0 = timestamp when suspended
    val mouseCaptureSuspendMs = 2000L // re-enable after 2s so scroll works again

    while (!done) {
      // Re-enable mouse capture after selection timeout
      if (mouseCaptureSuspendedAt > 0 && System.currentTimeMillis() - mouseCaptureSuspendedAt >= mouseCaptureSuspendMs) {
        jni.execute(new tui.crossterm.Command.EnableMouseCapture())
        mouseCaptureSuspendedAt = 0
      }

      // Read TestRunState if available for rich project display
      val displayItems: List[ProjectDisplayItem] = testRunStateOpt match {
        case Some(ref) =>
          val testState = ref.get.unsafeRunSync()
          ProjectDisplayItem.fromState(testState, System.currentTimeMillis())
        case None =>
          Nil
      }

      // Check if all tests are complete (from Summary in displayItems)
      val summaryOpt = displayItems.collectFirst { case s: ProjectDisplayItem.Summary => s }
      val doneFromSummary = summaryOpt.exists(_.allDone)

      if (doneFromSummary && !allTestsComplete) {
        diagLog(
          s"Auto-exit triggered: doneFromSummary=$doneFromSummary, compileFailures=${state.core.compileFailures.size}, suitesCompleted=${state.core.suitesCompleted}/${state.core.suitesTotal}"
        )
        allTestsComplete = true
        finishCountdown = autoExitDelayTicks
      }

      // Auto-exit after delay when all tests complete
      if (allTestsComplete) {
        finishCountdown -= 1
        if (finishCountdown <= 0) {
          diagLog(s"Countdown expired, exiting. suitesCompleted=${state.core.suitesCompleted}/${state.core.suitesTotal}")
          done = true
        }
      }

      // Pre-compute issue pane to get item count for scroll handling
      val issuePane = buildIssuePane(state)
      lastIssueItemCount = issuePane.items.length

      // Auto-follow: snap to bottom when new items arrive
      if (state.issueScrollAtBottom) {
        val maxOffset = math.max(0, lastIssueItemCount - math.max(1, lastIssueMaxVisible))
        state = state.copy(issueScrollOffset = maxOffset)
      }

      val issueMetrics = Array(0) // mutable holder for maxVisible, written by renderUI
      terminal.draw(f => renderUI(f, state, displayItems, issuePane, issueMetrics)): Unit
      lastIssueMaxVisible = issueMetrics(0)

      val elapsed = java.time.Duration.between(lastTick, Instant.now())
      val timeout = {
        val remaining = tickRate.minus(elapsed)
        if (remaining.isNegative) Duration.ZERO else remaining
      }

      // Drain ALL pending terminal events before rendering next frame.
      // This prevents scroll lag — mouse scroll generates many events rapidly.
      var hasTermEvent = jni.poll(new tui.crossterm.Duration(0, timeout.toNanos.toInt.max(1)))
      while (hasTermEvent && !done) {
        jni.read() match {
          case key: tui.crossterm.Event.Key =>
            key.keyEvent.code match {
              case char: tui.crossterm.KeyCode.Char if (char.c() == 'c' || char.c() == 'C') && state.workspaceBusy.exists(!_.cancelRequested) =>
                diagLog("User pressed c to cancel blocking work")
                state = state.copy(workspaceBusy = state.workspaceBusy.map(_.copy(cancelRequested = true)))
                cancelBlockingSignal.foreach(_.complete(()).unsafeRunSync())
              case char: tui.crossterm.KeyCode.Char if char.c() == 'q' || char.c() == 'Q' =>
                diagLog("User pressed q, exiting")
                done = true
                userCancelled = true
              case _: tui.crossterm.KeyCode.Esc =>
                done = true
                userCancelled = true
              case _: tui.crossterm.KeyCode.Up =>
                state = scrollIssues(state, -1, lastIssueItemCount, lastIssueMaxVisible)
              case _: tui.crossterm.KeyCode.Down =>
                state = scrollIssues(state, 1, lastIssueItemCount, lastIssueMaxVisible)
              case _: tui.crossterm.KeyCode.PageUp =>
                val pageSize = math.max(1, lastIssueMaxVisible - 1)
                state = scrollIssues(state, -pageSize, lastIssueItemCount, lastIssueMaxVisible)
              case _: tui.crossterm.KeyCode.PageDown =>
                val pageSize = math.max(1, lastIssueMaxVisible - 1)
                state = scrollIssues(state, pageSize, lastIssueItemCount, lastIssueMaxVisible)
              case _ =>
                key.keyEvent.code match {
                  case char: tui.crossterm.KeyCode.Char if char.c().toInt == 3 =>
                    done = true
                    userCancelled = true
                  case _ => ()
                }
            }
          case mouse: tui.crossterm.Event.Mouse =>
            mouse.mouseEvent.kind match {
              case _: tui.crossterm.MouseEventKind.ScrollUp =>
                state = scrollIssues(state, -3, lastIssueItemCount, lastIssueMaxVisible)
              case _: tui.crossterm.MouseEventKind.ScrollDown =>
                state = scrollIssues(state, 3, lastIssueItemCount, lastIssueMaxVisible)
              case _: tui.crossterm.MouseEventKind.Down =>
                // Suspend mouse capture so terminal handles text selection for copy/paste.
                // Re-enabled automatically after mouseCaptureSuspendMs.
                jni.execute(new tui.crossterm.Command.DisableMouseCapture())
                mouseCaptureSuspendedAt = System.currentTimeMillis()
              case _ => ()
            }
          case _ => ()
        }
        // Poll again with zero timeout to check for more queued events
        hasTermEvent = jni.poll(new tui.crossterm.Duration(0, 0))
      }

      var moreEvents = true
      while (moreEvents && !done)
        eventQueue.tryTake.unsafeRunSync() match {
          case Some(Some(event)) =>
            state = processEvent(state, event)
          case Some(None) =>
            diagLog(s"Event stream ended (None received), suitesCompleted=${state.core.suitesCompleted}/${state.core.suitesTotal}")
            if (!allTestsComplete) {
              allTestsComplete = true
              finishCountdown = autoExitDelayTicks
            }
            moreEvents = false
          case None =>
            moreEvents = false
        }

      if (elapsed.compareTo(tickRate) >= 0) {
        state = state.advanceFrame()
        lastTick = Instant.now()
      }
    }

    // Signal user quit so the outer race resolves
    if (userCancelled) {
      userQuitSignal.foreach(_.complete(()).unsafeRunSync())
    }

    // Use runningSuites for cancellation tracking (format: "project:suite")
    // runningTests might be empty between tests even when suite is running
    val summary = state.core.toSummary(durationMs = state.elapsedMs, wasCancelled = userCancelled)
    summary.copy(currentlyRunning = state.runningSuites.keys.toList)
  }

  private def processEvent(state: State, event: BuildEvent): State = {
    // Update canonical state via the pure reducer
    val newCore = BuildStateReducer.reduce(state.core, event)

    // TUI-specific mutations (mutable maps for animation/display)
    event match {
      case BuildEvent.CompileStarted(project, timestamp) =>
        state.compilingProjects.put(project, CompilingProject(project, Instant.ofEpochMilli(timestamp), None, None, None)): Unit

      case BuildEvent.CompilationReason(project, reason, _, _, _, _) =>
        // Update the compiling project with the reason for display
        state.compilingProjects.get(project).foreach { cp =>
          state.compilingProjects.put(project, cp.copy(reason = Some(reason))): Unit
        }

      case BuildEvent.CompileFinished(project, _, _, _, _, _) =>
        state.compilingProjects.remove(project): Unit

      case BuildEvent.SuiteStarted(project, suite, timestamp) =>
        val key = s"$project:$suite"
        state.runningSuites.put(key, RunningSuite(project, suite, Instant.ofEpochMilli(timestamp))): Unit

      case BuildEvent.TestStarted(project, suite, test, timestamp) =>
        val key = s"$project:$suite:$test"
        state.runningTests.put(key, RunningTest(project, suite, test, Instant.ofEpochMilli(timestamp))): Unit

      case BuildEvent.TestFinished(project, suite, test, status, _, _, _, _) =>
        val key = s"$project:$suite:$test"
        state.runningTests.remove(key): Unit

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

        if (status == TestStatus.Passed) {
          state.recentPasses.prepend(s"$suite.$test"): Unit
          if (state.recentPasses.size > 5) state.recentPasses.removeLast(): Unit
        }

      case BuildEvent.SuiteFinished(project, suite, _, _, _, _, _, _) =>
        val key = s"$project:$suite"
        state.runningSuites.remove(key): Unit

      case BuildEvent.SuiteTimedOut(project, suite, _, _, _) =>
        val key = s"$project:$suite"
        state.runningSuites.remove(key): Unit

      case BuildEvent.SuiteError(project, suite, _, _, _, _, _) =>
        val key = s"$project:$suite"
        state.runningSuites.remove(key): Unit

      case BuildEvent.SuiteCancelled(project, suite, _, _) =>
        val key = s"$project:$suite"
        state.runningSuites.remove(key): Unit

      case BuildEvent.CompileProgress(project, percent, _) =>
        state.compilingProjects.get(project).foreach { cp =>
          state.compilingProjects.put(project, cp.copy(progress = Some(percent))): Unit
        }

      case BuildEvent.CompilePhaseChanged(project, phase, trackedApis, _) =>
        state.compilingProjects.get(project).foreach { cp =>
          val displayPhase = phase match {
            case "reading-analysis" if trackedApis > 0 => Some(s"reading analysis ($trackedApis APIs)")
            case "reading-analysis"                    => Some("reading analysis")
            case "analyzing"                           => Some("analyzing")
            case "compiling"                           => None // CompileProgress takes over
            case "saving-analysis"                     => Some("saving analysis")
            case _                                     => None
          }
          state.compilingProjects.put(project, cp.copy(phase = displayPhase)): Unit
        }

      case BuildEvent.LinkStarted(_, _, _) | BuildEvent.LinkSucceeded(_, _, _, _) | BuildEvent.LinkFailed(_, _, _, _, _) =>
        () // Link state tracked in core via BuildStateReducer

      case _: BuildEvent.ConnectionLost =>
        // Connection lost — clear all running suites from TUI state
        state.runningSuites.clear()
        state.runningTests.clear()

      case BuildEvent.SuitesDiscovered(_, _, _, _) | BuildEvent.Output(_, _, _, _, _) | BuildEvent.ProjectSkipped(_, _, _) | BuildEvent.Error(_, _, _, _) |
          _: BuildEvent.TestRunCompleted | _: BuildEvent.SourcegenStarted | _: BuildEvent.SourcegenFinished =>
        () // No TUI-specific state for these (core state updated via BuildStateReducer)

      case _: BuildEvent.CompileStalled | _: BuildEvent.CompileResumed =>
        () // Handled below via stalledProjects state

      case _: BuildEvent.LockContention | _: BuildEvent.LockAcquired =>
        () // Handled below via lockedProjects state

      case _: BuildEvent.WorkspaceBusy | _: BuildEvent.WorkspaceReady =>
        () // Handled below via workspaceBusy state
    }

    // Return new state with updated core + TUI-specific fields
    val updatedSuitesDiscovered = event match {
      case BuildEvent.SuitesDiscovered(_, _, totalDiscovered, _) => totalDiscovered
      case _                                                     => state.suitesDiscovered
    }
    val updatedWorkspaceBusy = event match {
      case BuildEvent.WorkspaceBusy(_, operation, projects, startedAgoMs, _) =>
        Some(WorkspaceBusyInfo(operation, projects, startedAgoMs, Instant.now(), cancelRequested = false))
      case _: BuildEvent.WorkspaceReady =>
        None
      case _ =>
        state.workspaceBusy
    }
    val updatedStalledProjects = event match {
      case e: BuildEvent.CompileStalled =>
        state.stalledProjects.updated(e.project, e)
      case e: BuildEvent.CompileResumed =>
        state.stalledProjects - e.project
      case BuildEvent.CompileStarted(project, _) =>
        state.stalledProjects - project
      case BuildEvent.CompileFinished(project, _, _, _, _, _) =>
        state.stalledProjects - project
      case _ =>
        state.stalledProjects
    }
    val updatedLockedProjects = event match {
      case e: BuildEvent.LockContention =>
        state.lockedProjects.updated(e.project, e)
      case e: BuildEvent.LockAcquired =>
        state.lockedProjects - e.project
      case BuildEvent.CompileFinished(project, _, _, _, _, _) =>
        state.lockedProjects - project
      case _ =>
        state.lockedProjects
    }
    state.copy(
      core = newCore,
      suitesDiscovered = updatedSuitesDiscovered,
      stalledProjects = updatedStalledProjects,
      lockedProjects = updatedLockedProjects,
      workspaceBusy = updatedWorkspaceBusy
    )
  }

  private def renderUI(f: Frame, state: State, displayItems: List[ProjectDisplayItem], issuePane: PaneData, issueMetrics: Array[Int]): Unit = {
    // Fill entire screen with our dark background first
    f.renderWidget(BlockWidget(style = sBg), f.size)

    if (f.size.height < 10 || f.size.width < 40) {
      val msg = ParagraphWidget(
        text = Text.fromSpans(Spans.from(Span.styled("Terminal too small", s(Palette.warning)))),
        alignment = Alignment.Center
      )
      f.renderWidget(msg, f.size)
      return
    }

    val titleHeight = 1
    val summaryHeight = 5

    // Build pane content — only non-empty panes are shown
    val projectPane = state.workspaceBusy match {
      case Some(busy) => buildWorkspaceBusyPane(state, busy)
      case None       => buildProjectPane(state, displayItems)
    }
    val testPane = if (state.workspaceBusy.isDefined) PaneData(Array.empty, "", Palette.text, Palette.border) else buildTestPane(state, displayItems)
    val activePanes = Array(projectPane, testPane).filter(_.items.nonEmpty)

    // Active panes (Projects, Tests) get exactly the space they need.
    // Issues pane gets all remaining space (it's scrollable).
    val available = math.max(0, f.size.height - titleHeight - summaryHeight)
    val activePaneHeights = activePanes.map(p => math.min(p.items.length + 2, available))
    val usedByActive = activePaneHeights.sum
    val issueHeight = if (issuePane.items.nonEmpty) math.max(3, available - usedByActive) else 0

    val allPanes = if (issuePane.items.nonEmpty) activePanes :+ issuePane else activePanes
    val paneHeights = if (issuePane.items.nonEmpty) activePaneHeights :+ issueHeight else activePaneHeights

    val constraints = Array.newBuilder[Constraint]
    constraints += Constraint.Length(titleHeight)
    constraints += Constraint.Length(summaryHeight)
    paneHeights.foreach(h => constraints += Constraint.Length(h))
    val usedByPanes = paneHeights.sum
    if (usedByPanes < available) {
      constraints += Constraint.Length(available - usedByPanes)
    }

    val chunks = Layout(
      direction = Direction.Vertical,
      constraints = constraints.result()
    ).split(f.size)

    renderTitle(f, chunks(0), state)
    renderSummary(f, chunks(1), state, displayItems)

    allPanes.zip(paneHeights).zipWithIndex.foreach { case ((pane, height), i) =>
      val maxVisible = math.max(0, height - 2)
      val area = chunks(2 + i)
      if (pane eq issuePane) {
        // Report maxVisible back to the loop for scroll calculations
        issueMetrics(0) = maxVisible

        val totalItems = pane.items.length
        val effectiveOffset = if (state.issueScrollAtBottom) {
          math.max(0, totalItems - maxVisible)
        } else {
          math.min(state.issueScrollOffset, math.max(0, totalItems - maxVisible))
        }

        val visibleItems = pane.items.slice(effectiveOffset, effectiveOffset + maxVisible)
        renderPane(f, area, pane.title, pane.titleColor, pane.borderColor, visibleItems)
        if (totalItems > maxVisible) {
          renderScrollIndicator(f, area, totalItems, effectiveOffset, maxVisible)
        }
      } else {
        val visibleItems = if (pane.items.length > maxVisible) pane.items.take(maxVisible) else pane.items
        renderPane(f, area, pane.title, pane.titleColor, pane.borderColor, visibleItems)
      }
    }

    // Park cursor at first column of last line so that if something clobbers
    // the TUI (e.g. stderr output), it overwrites from the bottom.
    f.setCursor(0, f.size.height - 1)
  }

  private def renderTitle(f: Frame, area: Rect, state: State): Unit = {
    val title = state.mode match {
      case BuildMode.Compile | BuildMode.Link(_) => "BLEEP BUILD"
      case BuildMode.Test                        => "BLEEP TEST RUNNER"
      case BuildMode.Run(_, _)                   => "BLEEP RUN"
    }
    val paragraph = ParagraphWidget(
      text = Text.fromSpans(
        Spans.from(Span.styled(title, sb(Palette.info)))
      ),
      alignment = Alignment.Center
    )
    f.renderWidget(paragraph, area)
  }

  private def renderSummary(f: Frame, area: Rect, state: State, displayItems: List[ProjectDisplayItem]): Unit = {
    val summaryOpt = displayItems.collectFirst { case s: ProjectDisplayItem.Summary => s }
    val elapsed = formatDuration(state.elapsedMs)

    // Line 1: test/compile counters (surface bg for raised panel)
    val line1: Spans = state.mode match {
      case BuildMode.Test =>
        val totalFailed = state.core.testsFailed + state.core.testsTimedOut + state.core.testsCancelled
        Spans.from(
          Span.styled(s"$passedIcon ", ss(Palette.success)),
          Span.styled(s"${state.core.testsPassed} passed", ssb(Palette.success)),
          Span.styled("  ", sSurface),
          Span.styled(s"$failedIcon ", ss(if (totalFailed > 0) Palette.error else Palette.textDim)),
          Span.styled(s"$totalFailed failed", ssb(if (totalFailed > 0) Palette.error else Palette.textDim)),
          Span.styled("  ", sSurface),
          Span.styled(s"$skippedIcon ", ss(if (state.core.testsSkipped > 0) Palette.warning else Palette.textDim)),
          Span.styled(s"${state.core.testsSkipped} skipped", ss(if (state.core.testsSkipped > 0) Palette.warning else Palette.textDim))
        )
      case BuildMode.Compile | BuildMode.Link(_) =>
        Spans.from(
          Span.styled(s"$passedIcon ", ss(Palette.success)),
          Span.styled(s"${state.core.compilesCompleted} compiled", ssb(Palette.success)),
          Span.styled("  ", sSurface),
          Span.styled(s"$failedIcon ", ss(if (state.core.compilesFailed > 0) Palette.error else Palette.textDim)),
          Span.styled(
            s"${state.core.compilesFailed} failed",
            ssb(if (state.core.compilesFailed > 0) Palette.error else Palette.textDim)
          ),
          Span.styled("  ", sSurface),
          Span.styled(s"$skippedIcon ", ss(if (state.core.skippedProjects.nonEmpty) Palette.warning else Palette.textDim)),
          Span.styled(
            s"${state.core.skippedProjects.size} skipped",
            ss(if (state.core.skippedProjects.nonEmpty) Palette.warning else Palette.textDim)
          )
        )
      case BuildMode.Run(_, _) =>
        Spans.from(
          Span.styled(s"$passedIcon ", ss(Palette.success)),
          Span.styled(s"${state.core.compilesCompleted} compiled", ssb(Palette.success)),
          Span.styled("  ", sSurface),
          Span.styled(s"$failedIcon ", ss(if (state.core.compilesFailed > 0) Palette.error else Palette.textDim)),
          Span.styled(
            s"${state.core.compilesFailed} failed",
            ssb(if (state.core.compilesFailed > 0) Palette.error else Palette.textDim)
          )
        )
    }

    // Line 2: suite progress + duration
    val line2: Spans = state.mode match {
      case BuildMode.Test =>
        val suiteTotal = state.effectiveSuiteCount
        val progress = (state.suiteProgress * 100).toInt
        Spans.from(
          Span.styled(s"Suites: ${state.core.suitesCompleted}/$suiteTotal ($progress%)", ss(Palette.text)),
          Span.styled("  |  ", ss(Palette.textDim)),
          Span.styled(s"Duration: $elapsed", ss(Palette.text))
        )
      case _ =>
        Spans.from(
          Span.styled(s"Duration: $elapsed", ss(Palette.text))
        )
    }

    // Line 3: compile summary + parallelism + exit hint
    val wallTimeMs = state.elapsedMs
    val runningTaskTimeMs = state.compilingProjects.values.map(_.elapsedMs).sum +
      state.runningTests.values.map(_.elapsedMs).sum +
      state.runningSuites.values.map(_.elapsedMs).sum
    val totalTaskTimeMs = state.core.totalTaskTimeMs + runningTaskTimeMs
    val parallelism =
      if (wallTimeMs > 0 && totalTaskTimeMs > wallTimeMs) f"${totalTaskTimeMs.toDouble / wallTimeMs}%.1fx"
      else "-"

    val compiledStr = if (state.core.compilesFailed > 0) {
      s"Compiled: ${state.core.compilesCompleted - state.core.compilesFailed}/${state.core.compilesCompleted}"
    } else {
      s"Compiled: ${state.core.compilesCompleted}"
    }

    val line3: Spans = Spans.from(
      Span.styled(compiledStr, ss(Palette.textMuted)),
      Span.styled(s"  Parallelism: $parallelism", ss(Palette.textMuted)),
      Span.styled("  |  ", ss(Palette.textDim)),
      Span.styled("q: exit", ss(Palette.textDim))
    )

    // Determine border color based on completion status
    val totalTestProblems = state.core.testsFailed + state.core.testsTimedOut + state.core.testsCancelled
    val hasProblems = state.core.compilesFailed > 0 || state.core.linksFailed > 0 || totalTestProblems > 0 || state.core.suitesCancelled > 0
    val borderColor = summaryOpt match {
      case Some(summary) if summary.allDone && !hasProblems => Palette.success
      case Some(summary) if summary.allDone                 => Palette.error
      case _                                                => Palette.border
    }

    val summaryTitle = summaryOpt match {
      case Some(summary) if summary.allDone && !hasProblems => Some(Spans.styled("FINISHED", ssb(Palette.success)))
      case Some(summary) if summary.allDone                 => Some(Spans.styled("FINISHED (with failures)", ssb(Palette.error)))
      case _                                                => Some(Spans.styled("Summary", ss(Palette.info)))
    }

    val paragraph = ParagraphWidget(
      text = Text.fromSpans(line1, line2, line3),
      block = Some(
        BlockWidget(
          title = summaryTitle,
          borders = Borders.ALL,
          borderStyle = Style(fg = Some(borderColor), bg = Some(Palette.surface)),
          borderType = BlockWidget.BorderType.Rounded,
          style = sSurface
        )
      ),
      alignment = Alignment.Center
    )
    f.renderWidget(paragraph, area)
  }

  // ── Pane infrastructure ──────────────────────────────────────────────

  private case class PaneData(
      items: Array[ListWidget.Item],
      title: String,
      titleColor: Color,
      borderColor: Color
  )

  /** Compute pane heights to fit available rows. Each pane needs itemCount + 2 (borders). */
  private def fitPaneHeights(itemCounts: Array[Int], available: Int): Array[Int] = {
    if (itemCounts.isEmpty) return Array.empty
    val desired = itemCounts.map(_ + 2)
    val total = desired.sum
    if (total <= available) {
      desired
    } else {
      val minPerPane = 3 // 1 item + 2 borders
      val totalMin = minPerPane * desired.length
      if (available <= totalMin) {
        Array.fill(desired.length)(math.max(minPerPane, available / math.max(1, desired.length)))
      } else {
        val extra = available - totalMin
        val totalAboveMin = desired.map(d => math.max(0, d - minPerPane)).sum
        if (totalAboveMin == 0) {
          Array.fill(desired.length)(minPerPane)
        } else {
          val result = desired.map { d =>
            val aboveMin = math.max(0, d - minPerPane)
            minPerPane + (aboveMin.toDouble / totalAboveMin * extra).toInt
          }
          val shortfall = available - result.sum
          if (shortfall > 0 && result.nonEmpty) result(result.length - 1) += shortfall
          result
        }
      }
    }
  }

  private def renderPane(f: Frame, area: Rect, title: String, titleColor: Color, borderColor: Color, items: Array[ListWidget.Item]): Unit = {
    val list = ListWidget(
      items = items,
      block = Some(
        BlockWidget(
          title = Some(Spans.styled(title, s(titleColor))),
          borders = Borders.ALL,
          borderStyle = s(borderColor),
          borderType = BlockWidget.BorderType.Rounded,
          style = sBg
        )
      )
    )
    f.renderWidget(list, area)
  }

  // ── Pane builders ──────────────────────────────────────────────────

  private def buildWorkspaceBusyPane(state: State, busy: WorkspaceBusyInfo): PaneData = {
    val items = scala.collection.mutable.ArrayBuffer.empty[ListWidget.Item]

    val elapsedMs = java.time.Duration.between(busy.receivedAt, Instant.now()).toMillis + busy.startedAgoMs
    val elapsedStr = formatDuration(elapsedMs)

    if (busy.cancelRequested) {
      items += ListWidget.Item(
        content = Text.fromSpans(
          Spans.from(
            Span.styled(s"${state.spinner} ", s(Palette.error)),
            Span.styled("Cancelling active build...", sb(Palette.error))
          )
        )
      )
      items += ListWidget.Item(
        content = Text.fromSpans(
          Spans.from(
            Span.styled(s"  ${busy.operation}: ", s(Palette.textMuted)),
            Span.styled(busy.projects.mkString(", "), s(Palette.text)),
            Span.styled(s" ($elapsedStr)", s(Palette.textDim))
          )
        )
      )
    } else {
      items += ListWidget.Item(
        content = Text.fromSpans(
          Spans.from(
            Span.styled(s"${state.spinner} ", s(Palette.warning)),
            Span.styled("Waiting for active build to finish...", sb(Palette.warning))
          )
        )
      )
      items += ListWidget.Item(
        content = Text.fromSpans(
          Spans.from(
            Span.styled(s"  ${busy.operation}: ", s(Palette.textMuted)),
            Span.styled(busy.projects.mkString(", "), s(Palette.text)),
            Span.styled(s" ($elapsedStr)", s(Palette.textDim))
          )
        )
      )
      items += ListWidget.Item(
        content = Text.fromSpans(Spans.from(Span.styled("", s(Palette.textDim))))
      )
      items += ListWidget.Item(
        content = Text.fromSpans(
          Spans.from(
            Span.styled("  'c'", sb(Palette.info)),
            Span.styled(" cancel running build   ", s(Palette.textMuted)),
            Span.styled("'q'", sb(Palette.info)),
            Span.styled(" quit", s(Palette.textMuted))
          )
        )
      )
    }

    val title = if (busy.cancelRequested) "Cancelling" else "Workspace Busy"
    PaneData(items.toArray, title, Palette.warning, Palette.warning)
  }

  private def buildProjectPane(state: State, displayItems: List[ProjectDisplayItem]): PaneData = {
    val items = scala.collection.mutable.ArrayBuffer.empty[ListWidget.Item]

    val hasActiveDisplayItems = displayItems.exists {
      case _: ProjectDisplayItem.Compiling   => true
      case _: ProjectDisplayItem.Linking     => true
      case _: ProjectDisplayItem.Discovering => true
      case _                                 => false
    }

    displayItems.foreach {
      case ProjectDisplayItem.Compiling(project, percent, blockedCount) =>
        val progressStr = percent.map(p => s" $p%").getOrElse("")
        val blockedStr = if (blockedCount > 0) s" (blocks $blockedCount)" else ""
        items += ListWidget.Item(
          Text.fromSpans(
            Spans.from(
              Span.styled(s" $compilingIcon ", s(Palette.accent)),
              Span.styled("BUILD ", s(Palette.accent)),
              Span.styled(project.value, s(Palette.text)),
              Span.styled(s"$progressStr$blockedStr", s(Palette.textDim))
            )
          )
        )

      case ProjectDisplayItem.Linking(project, blockedCount) =>
        val spinner = spinnerFrames(state.frame % spinnerFrames.length)
        val blockedStr = if (blockedCount > 0) s" (blocks $blockedCount)" else ""
        items += ListWidget.Item(
          Text.fromSpans(
            Spans.from(
              Span.styled(s" $spinner ", s(Palette.accent)),
              Span.styled("LINK ", s(Palette.accent)),
              Span.styled(project.value, s(Palette.text)),
              Span.styled(blockedStr, s(Palette.textDim))
            )
          )
        )

      case ProjectDisplayItem.Discovering(project) =>
        val spinner = spinnerFrames(state.frame % spinnerFrames.length)
        items += ListWidget.Item(
          Text.fromSpans(
            Spans.from(
              Span.styled(s" $spinner ", s(Palette.info)),
              Span.styled("DISCOVER ", s(Palette.info)),
              Span.styled(project.value, s(Palette.text))
            )
          )
        )

      case ProjectDisplayItem.WaitingProject(project, phase, waitingFor) =>
        val phaseIcon = phase match {
          case ProjectDisplayItem.WaitingPhase.WaitingToCompile  => "..."
          case ProjectDisplayItem.WaitingPhase.Compiling         => ">"
          case ProjectDisplayItem.WaitingPhase.WaitingToDiscover => "?"
          case ProjectDisplayItem.WaitingPhase.WaitingToTest     => "~"
        }
        val waitingForStr = if (waitingFor.nonEmpty) {
          s" (waiting: ${waitingFor.take(3).map(_.value).mkString(", ")}${if (waitingFor.size > 3) s" +${waitingFor.size - 3}" else ""})"
        } else {
          s" (${phase.description})"
        }
        items += ListWidget.Item(
          Text.fromSpans(
            Spans.from(
              Span.styled(s" $phaseIcon ", s(Palette.textDim)),
              Span.styled(project.value, s(Palette.textMuted)),
              Span.styled(waitingForStr, s(Palette.textDim))
            )
          )
        )

      case _ => ()
    }

    // Show sourcegen scripts from BuildState
    state.core.sourcegenRunning.toList.sorted.foreach { scriptMain =>
      val spinner = spinnerFrames(state.frame % spinnerFrames.length)
      // Shorten the script name for display (just class name)
      val shortName = scriptMain.split('.').lastOption.getOrElse(scriptMain)
      items += ListWidget.Item(
        Text.fromSpans(
          Spans.from(
            Span.styled(s" $spinner ", s(Palette.info)),
            Span.styled("SOURCEGEN ", s(Palette.info)),
            Span.styled(shortName, s(Palette.text))
          )
        )
      )
    }

    // Show linking projects from BuildState
    state.core.currentlyLinking.toList.sorted.foreach { project =>
      val spinner = spinnerFrames(state.frame % spinnerFrames.length)
      items += ListWidget.Item(
        Text.fromSpans(
          Spans.from(
            Span.styled(s" $spinner ", s(Palette.accent)),
            Span.styled("LINK ", s(Palette.accent)),
            Span.styled(project, s(Palette.text))
          )
        )
      )
    }

    // Show heap pressure stalls
    state.stalledProjects.values.foreach { stalled =>
      val waitSec = math.max(0, (stalled.retryAtMs - System.currentTimeMillis() + 999) / 1000)
      val spinner = spinnerFrames(state.frame % spinnerFrames.length)
      items += ListWidget.Item(
        Text.fromSpans(
          Spans.from(
            Span.styled(s" $spinner ", s(Palette.warning)),
            Span.styled("HEAP ", s(Palette.warning)),
            Span.styled(stalled.project, s(Palette.text)),
            Span.styled(s" ${stalled.heapUsedMb}MB/${stalled.heapMaxMb}MB", s(Palette.warning)),
            Span.styled(s" retry ${waitSec}s", s(Palette.textDim))
          )
        )
      )
    }

    // Show lock contention
    state.lockedProjects.values.foreach { locked =>
      val waitSec = (System.currentTimeMillis() - locked.timestamp + 999) / 1000
      val spinner = spinnerFrames(state.frame % spinnerFrames.length)
      items += ListWidget.Item(
        Text.fromSpans(
          Spans.from(
            Span.styled(s" $spinner ", s(Palette.warning)),
            Span.styled("LOCK ", s(Palette.warning)),
            Span.styled(locked.project, s(Palette.text)),
            Span.styled(s" waiting ${waitSec}s", s(Palette.textDim))
          )
        )
      )
    }

    // Fallback: show compiling projects from State when no displayItems
    if (!hasActiveDisplayItems) {
      state.compilingProjects.values.foreach { cp =>
        val phaseStr = cp.phase.map(p => s" ($p)").getOrElse("")
        val progressStr = cp.progress.map(p => s" $p%").getOrElse(phaseStr)
        val elapsed = formatDuration(cp.elapsedMs)
        items += ListWidget.Item(
          Text.fromSpans(
            Spans.from(
              Span.styled(s" $compilingIcon ", s(Palette.accent)),
              Span.styled("BUILD ", s(Palette.accent)),
              Span.styled(cp.name, s(Palette.text)),
              Span.styled(s"$progressStr ($elapsed)", s(Palette.textDim))
            )
          )
        )
      }
    }

    val activeCount = items.length
    val title = if (activeCount > 0) s"Projects ($activeCount active)" else "Projects"
    PaneData(items.toArray, title, Palette.info, Palette.border)
  }

  private def buildTestPane(state: State, displayItems: List[ProjectDisplayItem]): PaneData = {
    val items = scala.collection.mutable.ArrayBuffer.empty[ListWidget.Item]

    val hasActiveDisplayItems = displayItems.exists(_.isInstanceOf[ProjectDisplayItem.Testing])

    displayItems.foreach {
      case ProjectDisplayItem.Testing.Reactive(project, suitesCompleted, suitesTotal, failures, runningTests) =>
        val progressStr = s"$suitesCompleted/$suitesTotal"
        val progressPercent = if (suitesTotal > 0) (suitesCompleted * 100) / suitesTotal else 0
        val failureStr = if (failures > 0) s" ${failedIcon}$failures" else ""
        val spinner = spinnerFrames(state.frame % spinnerFrames.length)

        items += ListWidget.Item(
          Text.fromSpans(
            Spans.from(
              Span.styled(s" $spinner ", s(Palette.info)),
              Span.styled(project.value, s(Palette.accent)),
              Span.styled(s" $progressStr ($progressPercent%)", s(Palette.text)),
              Span.styled(failureStr, s(if (failures > 0) Palette.error else Palette.textDim))
            )
          )
        )

        val testsBySuite = runningTests.groupBy(_.suiteName).toList.sortBy(_._1.value)

        if (testsBySuite.isEmpty) {
          items += ListWidget.Item(
            Text.fromSpans(
              Spans.from(
                Span.styled("  ", sBg),
                Span.styled(s"$runningIcon ", s(Palette.info)),
                Span.styled("running tests...", s(Palette.textMuted))
              )
            )
          )
        } else {
          testsBySuite.foreach { case (suiteName, suiteTests) =>
            val suiteJvmPid = suiteTests.headOption.map(_.jvmPid).getOrElse(0L)
            val maxElapsed = suiteTests.map(_.elapsedMs).maxOption.getOrElse(0L)
            val elapsed = formatDuration(maxElapsed)
            val testCount = suiteTests.count(_.testName.value.nonEmpty)
            val testCountStr = if (testCount > 0) s"$testCount tests, " else ""

            items += ListWidget.Item(
              Text.fromSpans(
                Spans.from(
                  Span.styled("  ", sBg),
                  Span.styled(s"[${suiteJvmPid}] ", s(Palette.textDim)),
                  Span.styled(suiteName.shortName, s(Palette.text)),
                  Span.styled(s" ($testCountStr$elapsed)", s(Palette.textDim))
                )
              )
            )

            suiteTests.filter(_.testName.value.nonEmpty).foreach { rt =>
              val testElapsed = formatDuration(rt.elapsedMs)
              items += ListWidget.Item(
                Text.fromSpans(
                  Spans.from(
                    Span.styled("    ", sBg),
                    Span.styled(s"$runningIcon ", s(Palette.info)),
                    Span.styled(rt.testName.value, s(Palette.textMuted)),
                    Span.styled(s" ($testElapsed)", s(Palette.textDim))
                  )
                )
              )
            }
          }
        }

      case ProjectDisplayItem.Testing.Bsp(project, platform, elapsedMs) =>
        val elapsed = formatDuration(elapsedMs)
        val spinner = spinnerFrames(state.frame % spinnerFrames.length)
        val platformStr = platform.value.toUpperCase

        items += ListWidget.Item(
          Text.fromSpans(
            Spans.from(
              Span.styled(s" $spinner ", s(Palette.info)),
              Span.styled(project.value, s(Palette.accent)),
              Span.styled(s" [$platformStr]", s(Palette.textDim)),
              Span.styled(s" ($elapsed)", s(Palette.textDim))
            )
          )
        )

      case ProjectDisplayItem.TestsCompleted(project, passed, failed, skipped, durationMs) =>
        val duration = formatDuration(durationMs)
        val (icon, color) = if (failed > 0) (failedIcon, Palette.error) else (passedIcon, Palette.success)
        val stats = s"$passed passed, $failed failed" + (if (skipped > 0) s", $skipped skipped" else "")
        items += ListWidget.Item(
          Text.fromSpans(
            Spans.from(
              Span.styled(s" $icon ", s(color)),
              Span.styled(project.value, s(Palette.accent)),
              Span.styled(s" $stats", s(Palette.textDim)),
              Span.styled(s" ($duration)", s(Palette.textDim))
            )
          )
        )

      case _ => ()
    }

    // Fallback: show running tests from State when no displayItems
    if (!hasActiveDisplayItems) {
      val testsByProject = state.runningTests.values.toSeq.groupBy(_.project)
      val projectsWithActivity = (state.runningSuites.values.map(_.project) ++ testsByProject.keys).toSeq.distinct

      projectsWithActivity.foreach { project =>
        val spinner = spinnerFrames(state.frame % spinnerFrames.length)
        val projectSuites = state.runningSuites.values.filter(_.project == project).toSeq
        val projectTests = testsByProject.getOrElse(project, Seq.empty)
        val failedCount = projectSuites.map(_.testsFailed).sum

        items += ListWidget.Item(
          Text.fromSpans(
            Spans.from(
              Span.styled(s" $spinner ", s(Palette.info)),
              Span.styled(project, s(Palette.accent)),
              Span.styled(s" ${projectSuites.size} suites", s(Palette.textDim)),
              Span.styled(if (failedCount > 0) s" ${failedIcon}$failedCount" else "", s(Palette.error))
            )
          )
        )

        val testsBySuite = projectTests.groupBy(_.suite)
        val allSuites = (projectSuites.map(_.suite) ++ testsBySuite.keys).distinct

        allSuites.foreach { suiteName =>
          val shortSuite = suiteName.split('.').lastOption.getOrElse(suiteName)
          val suiteInfo = projectSuites.find(_.suite == suiteName)
          val testsInSuite = testsBySuite.getOrElse(suiteName, Seq.empty)
          val elapsed = suiteInfo.map(rs => formatDuration(rs.elapsedMs)).getOrElse("")

          items += ListWidget.Item(
            Text.fromSpans(
              Spans.from(
                Span.styled("  ", sBg),
                Span.styled(shortSuite, s(Palette.text)),
                Span.styled(if (elapsed.nonEmpty) s" ($elapsed)" else "", s(Palette.textDim))
              )
            )
          )

          testsInSuite.foreach { rt =>
            val testElapsed = formatDuration(rt.elapsedMs)
            items += ListWidget.Item(
              Text.fromSpans(
                Spans.from(
                  Span.styled("    ", sBg),
                  Span.styled(s"$runningIcon ", s(Palette.info)),
                  Span.styled(rt.test, s(Palette.textMuted)),
                  Span.styled(s" ($testElapsed)", s(Palette.textDim))
                )
              )
            )
          }
        }
      }
    }

    val runningCount = state.runningSuites.size + displayItems.count(_.isInstanceOf[ProjectDisplayItem.Testing])
    val title = if (runningCount > 0) s"Tests ($runningCount running)" else "Tests"
    PaneData(items.toArray, title, Palette.info, Palette.border)
  }

  private def buildIssuePane(state: State): PaneData = {
    val items = scala.collection.mutable.ArrayBuffer.empty[ListWidget.Item]
    val warningItems = scala.collection.mutable.ArrayBuffer.empty[ListWidget.Item]

    // Show compile failures: errors first, collect warnings for the bottom
    state.core.compileFailures.foreach { cf =>
      val errors = cf.diagnostics.filter(_.severity == "error")
      val warnings = cf.diagnostics.filter(_.severity == "warning")
      val skippedCount = state.core.skippedProjects.count(_.reason.contains(cf.project))
      val skippedSuffix = if (skippedCount > 0) s" (${skippedCount} skipped)" else ""

      if (errors.nonEmpty) {
        items += ListWidget.Item(
          Text.fromSpans(
            Spans.from(
              Span.styled(s" $failedIcon COMPILE ", s(Palette.error)),
              Span.styled(cf.project, s(Palette.accent)),
              Span.styled(skippedSuffix, s(Palette.warning))
            )
          )
        )
        errors.foreach { diag =>
          items += ListWidget.Item(
            Text.fromSpans(
              Spans.from(
                Span.styled("    E ", s(Palette.error)),
                Span.styled(diag.message.take(200), s(Palette.error))
              )
            )
          )
        }
      }

      warnings.foreach { diag =>
        warningItems += ListWidget.Item(
          Text.fromSpans(
            Spans.from(
              Span.styled(s"    W ", s(Palette.warning)),
              Span.styled(s"${cf.project}: ", s(Palette.accent)),
              Span.styled(diag.message.take(200), s(Palette.warning))
            )
          )
        )
      }
    }

    // Show test failures
    state.core.failures.foreach { failure =>
      val (icon, color) = failure.category match {
        case FailureCategory.TestFailed   => (failedIcon, Palette.error)
        case FailureCategory.Timeout      => ("T", Palette.error)
        case FailureCategory.Cancelled    => ("c", Palette.warning)
        case FailureCategory.ProcessError => ("!", Palette.error)
        case FailureCategory.BuildError   => ("!", Palette.error)
      }
      items += ListWidget.Item(
        Text.fromSpans(
          Spans.from(
            Span.styled(s" $icon ", s(color)),
            Span.styled(s"${failure.project} ", s(Palette.accent)),
            Span.styled(s"${failure.suite}.${failure.test}", s(Palette.text))
          )
        )
      )

      val msg = failure.message.getOrElse("(no message)")
      items += ListWidget.Item(
        Text.fromSpans(
          Spans.from(
            Span.styled("    ", sBg),
            Span.styled(msg.take(300), s(Palette.textMuted))
          )
        )
      )
    }

    // Warnings at the bottom — new warnings always appear at the end
    items ++= warningItems

    val totalErrors = state.core.compileFailures.flatMap(_.diagnostics).count(_.severity == "error") + state.core.failures.size
    val totalWarnings = state.core.compileFailures.flatMap(_.diagnostics).count(_.severity == "warning")
    val title =
      if (totalWarnings > 0) s"Issues ($totalErrors errors, $totalWarnings warnings)"
      else s"Issues ($totalErrors)"
    PaneData(items.toArray, title, Palette.error, Palette.error)
  }

  private def scrollIssues(s: State, delta: Int, totalItems: Int, maxVisible: Int): State = {
    val maxOffset = math.max(0, totalItems - maxVisible)
    val newOffset = math.max(0, math.min(maxOffset, s.issueScrollOffset + delta))
    val atBottom = newOffset >= maxOffset
    s.copy(issueScrollOffset = newOffset, issueScrollAtBottom = atBottom)
  }

  private def renderScrollIndicator(f: Frame, area: Rect, totalItems: Int, offset: Int, maxVisible: Int): Unit = {
    if (totalItems <= maxVisible) return
    val trackHeight = math.max(1, area.height - 2) // inside borders
    val thumbHeight = math.max(1, (maxVisible.toDouble / totalItems * trackHeight).toInt)
    val maxOffset = totalItems - maxVisible
    val thumbPos = if (maxOffset > 0) ((offset.toDouble / maxOffset) * (trackHeight - thumbHeight)).toInt else 0
    val x = area.right - 1 // right border column
    val yStart = area.top + 1 // skip top border
    for (i <- 0 until trackHeight) {
      val inThumb = i >= thumbPos && i < thumbPos + thumbHeight
      val ch = if (inThumb) "\u2503" else "\u2502" // ┃ for thumb, │ for track
      val style = if (inThumb) s(Palette.info) else s(Palette.border)
      f.buffer.setString(x, yStart + i, ch, style)
    }
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
