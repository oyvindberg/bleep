package bleep.testing

/** Canonical state for build/test progress tracking.
  *
  * This is the single source of truth for counting and classification. All display implementations (basic, fancy bridge, TUI) derive their state from this via
  * [[BuildStateReducer.reduce]].
  */
case class BuildState(
    sourcegenRunning: Set[String], // Currently running sourcegen scripts
    sourcegenCompleted: Int,
    sourcegenFailed: Int,
    compilesCompleted: Int,
    compilesFailed: Int,
    compilesSkipped: Int,
    compilesCancelled: Int,
    testsTotal: Int,
    testsPassed: Int,
    testsFailed: Int,
    testsTimedOut: Int,
    testsCancelled: Int,
    testsSkipped: Int,
    testsIgnored: Int,
    suitesTotal: Int,
    suitesCompleted: Int,
    suitesFailed: Int,
    suitesCancelled: Int,
    currentlyRunning: Set[String],
    suiteStartTimes: Map[String, Long], // "project:suite" -> timestamp when suite started
    currentlyCompiling: Set[String],
    compileStartTimes: Map[String, Long], // project -> timestamp when compile started
    currentlyLinking: Set[String],
    linksCompleted: Int,
    linksFailed: Int,
    linkFailures: List[LinkFailure],
    failures: List[TestFailure],
    skipped: List[TestSkipped],
    cancelledSuites: List[CancelledSuite],
    compileFailures: List[ProjectCompileFailure],
    skippedProjects: List[SkippedProject],
    pendingOutput: Map[String, List[String]],
    totalTaskTimeMs: Long
) {

  /** Project to BuildSummary (lists are reversed since we prepend during accumulation) */
  def toSummary(durationMs: Long, wasCancelled: Boolean): BuildSummary = {
    val nowMs = System.currentTimeMillis()
    val killedCompiles = currentlyCompiling.toList.sorted.map { project =>
      val startedAt = compileStartTimes.getOrElse(project, nowMs)
      KilledTask("compile", project, nowMs - startedAt)
    }
    val killedLinks = currentlyLinking.toList.sorted.map { project =>
      KilledTask("link", project, 0)
    }
    // currentlyRunning keys are "project:suite" or "project:suite:test" — only take suite-level
    val killedSuites = currentlyRunning.toList.filter(_.count(_ == ':') == 1).sorted.map { key =>
      val startedAt = suiteStartTimes.getOrElse(key, nowMs)
      KilledTask("test", key, nowMs - startedAt)
    }
    BuildSummary(
      sourcegenFailed = sourcegenFailed,
      compilesCompleted = compilesCompleted,
      compilesFailed = compilesFailed,
      compilesSkipped = compilesSkipped,
      compilesCancelled = compilesCancelled,
      suitesTotal = suitesTotal,
      suitesCompleted = suitesCompleted,
      suitesFailed = suitesFailed,
      suitesCancelled = suitesCancelled,
      testsTotal = testsTotal,
      testsPassed = testsPassed,
      testsFailed = testsFailed,
      testsTimedOut = testsTimedOut,
      testsCancelled = testsCancelled,
      testsSkipped = testsSkipped,
      testsIgnored = testsIgnored,
      currentlyRunning = currentlyRunning.toList,
      killedTasks = killedCompiles ++ killedLinks ++ killedSuites,
      failures = failures.reverse,
      skipped = skipped.reverse,
      cancelledSuites = cancelledSuites.reverse,
      compileFailures = compileFailures.reverse,
      linkFailures = linkFailures.reverse,
      skippedProjects = skippedProjects.reverse,
      durationMs = durationMs,
      totalTaskTimeMs = totalTaskTimeMs,
      wasCancelled = wasCancelled
    )
  }
}

object BuildState {
  val empty: BuildState = BuildState(
    sourcegenRunning = Set.empty,
    sourcegenCompleted = 0,
    sourcegenFailed = 0,
    compilesCompleted = 0,
    compilesFailed = 0,
    compilesSkipped = 0,
    compilesCancelled = 0,
    testsTotal = 0,
    testsPassed = 0,
    testsFailed = 0,
    testsTimedOut = 0,
    testsCancelled = 0,
    testsSkipped = 0,
    testsIgnored = 0,
    suitesTotal = 0,
    suitesCompleted = 0,
    suitesFailed = 0,
    suitesCancelled = 0,
    currentlyRunning = Set.empty,
    suiteStartTimes = Map.empty,
    currentlyCompiling = Set.empty,
    compileStartTimes = Map.empty,
    currentlyLinking = Set.empty,
    linksCompleted = 0,
    linksFailed = 0,
    linkFailures = Nil,
    failures = Nil,
    skipped = Nil,
    cancelledSuites = Nil,
    compileFailures = Nil,
    skippedProjects = Nil,
    pendingOutput = Map.empty,
    totalTaskTimeMs = 0
  )
}

/** Pure reducer: the single source of truth for how events update build state.
  *
  * No IO, no side effects. Every display implementation calls this and then layers on its own rendering concerns.
  */
object BuildStateReducer {

  def reduce(state: BuildState, event: BuildEvent): BuildState = event match {

    case BuildEvent.SourcegenStarted(scriptMain, _, _) =>
      state.copy(sourcegenRunning = state.sourcegenRunning + scriptMain)

    case BuildEvent.SourcegenFinished(scriptMain, success, _, _, _) =>
      state.copy(
        sourcegenRunning = state.sourcegenRunning - scriptMain,
        sourcegenCompleted = state.sourcegenCompleted + 1,
        sourcegenFailed = if (success) state.sourcegenFailed else state.sourcegenFailed + 1
      )

    case BuildEvent.CompileStarted(project, timestamp) =>
      state.copy(
        currentlyCompiling = state.currentlyCompiling + project,
        compileStartTimes = state.compileStartTimes + (project -> timestamp)
      )

    case BuildEvent.CompilationReason(_, _, _, _, _, _) =>
      // CompilationReason is purely informational for display - no state change needed
      state

    case BuildEvent.CompileFinished(project, status, durationMs, _, diagnostics, skippedBecause) =>
      val updatedCompileFailures = status match {
        case "failed" | "error" => ProjectCompileFailure(project, diagnostics) :: state.compileFailures
        case _                  => state.compileFailures
      }
      val updatedSkippedProjects = status match {
        case "skipped" =>
          val reason = skippedBecause.map(dep => s"dependency $dep failed").getOrElse("dependency failed")
          SkippedProject(project, reason) :: state.skippedProjects
        case _ => state.skippedProjects
      }
      state.copy(
        compilesCompleted = state.compilesCompleted + 1,
        compilesFailed = state.compilesFailed + (if (status == "failed" || status == "error") 1 else 0),
        compilesSkipped = state.compilesSkipped + (if (status == "skipped") 1 else 0),
        compilesCancelled = state.compilesCancelled + (if (status == "cancelled") 1 else 0),
        currentlyCompiling = state.currentlyCompiling - project,
        compileStartTimes = state.compileStartTimes - project,
        compileFailures = updatedCompileFailures,
        skippedProjects = updatedSkippedProjects,
        totalTaskTimeMs = state.totalTaskTimeMs + durationMs
      )

    case BuildEvent.SuiteStarted(project, suite, timestamp) =>
      val key = s"$project:$suite"
      state.copy(
        suitesTotal = state.suitesTotal + 1,
        currentlyRunning = state.currentlyRunning + key,
        suiteStartTimes = state.suiteStartTimes + (key -> timestamp)
      )

    case BuildEvent.TestStarted(project, suite, test, _) =>
      val key = s"$project:$suite:$test"
      state.copy(
        testsTotal = state.testsTotal + 1,
        currentlyRunning = state.currentlyRunning + key
      )

    case BuildEvent.TestFinished(project, suite, test, status, durationMs, message, _, _) =>
      val key = s"$project:$suite:$test"

      val updatedFailures = status match {
        case TestStatus.Failed | TestStatus.Error =>
          val output = state.pendingOutput.getOrElse(s"$project:$suite", Nil)
          TestFailure(project, suite, test, message, None, output, FailureCategory.TestFailed) :: state.failures
        case TestStatus.Timeout =>
          val output = state.pendingOutput.getOrElse(s"$project:$suite", Nil)
          TestFailure(project, suite, test, message, None, output, FailureCategory.Timeout) :: state.failures
        case TestStatus.Cancelled =>
          val output = state.pendingOutput.getOrElse(s"$project:$suite", Nil)
          TestFailure(project, suite, test, message, None, output, FailureCategory.Cancelled) :: state.failures
        case TestStatus.AssumptionFailed => state.failures // not a failure
        case _                           => state.failures
      }

      val updatedSkipped = status match {
        case TestStatus.Skipped | TestStatus.Ignored | TestStatus.Pending =>
          TestSkipped(project, suite, test, status) :: state.skipped
        case TestStatus.AssumptionFailed =>
          TestSkipped(project, suite, test, status, message) :: state.skipped
        case _ => state.skipped
      }

      state.copy(
        currentlyRunning = state.currentlyRunning - key,
        testsPassed = state.testsPassed + (if (status == TestStatus.Passed) 1 else 0),
        testsFailed = state.testsFailed + (if (status == TestStatus.Failed || status == TestStatus.Error) 1 else 0),
        testsTimedOut = state.testsTimedOut + (if (status == TestStatus.Timeout) 1 else 0),
        testsCancelled = state.testsCancelled + (if (status == TestStatus.Cancelled) 1 else 0),
        testsSkipped = state.testsSkipped + (if (status == TestStatus.Skipped || status == TestStatus.AssumptionFailed) 1 else 0),
        testsIgnored = state.testsIgnored + (if (status == TestStatus.Ignored || status == TestStatus.Pending) 1 else 0),
        failures = updatedFailures,
        skipped = updatedSkipped,
        totalTaskTimeMs = state.totalTaskTimeMs + durationMs
      )

    case BuildEvent.SuiteFinished(project, suite, _, failed, _, _, _, _) =>
      val key = s"$project:$suite"
      // If suite reports failures but no individual TestFinished events created TestFailure entries,
      // create a synthetic failure so the summary shows useful info instead of "detailed info was not captured"
      val existingFailuresForSuite = state.failures.count(f => f.project == project && f.suite == suite)
      val syntheticFailures = if (failed > 0 && existingFailuresForSuite == 0) {
        val output = state.pendingOutput.getOrElse(key, Nil)
        List(
          TestFailure(
            project = project,
            suite = suite,
            test = "(suite failed)",
            message = Some(s"Suite reported $failed failure(s) but no individual test results were captured"),
            throwable = None,
            output = output,
            category = FailureCategory.ProcessError
          )
        )
      } else Nil
      state.copy(
        suitesCompleted = state.suitesCompleted + 1,
        suitesFailed = state.suitesFailed + (if (failed > 0) 1 else 0),
        currentlyRunning = state.currentlyRunning - key,
        suiteStartTimes = state.suiteStartTimes - key,
        pendingOutput = state.pendingOutput - key,
        failures = syntheticFailures ++ state.failures
      )

    case BuildEvent.Output(project, suite, line, _, _) =>
      val key = s"$project:$suite"
      state.copy(
        pendingOutput = state.pendingOutput.updated(
          key,
          state.pendingOutput.getOrElse(key, Nil) :+ line
        )
      )

    case BuildEvent.SuitesDiscovered(_, _, _, _) =>
      state // No state change for discovery

    case BuildEvent.ProjectSkipped(project, reason, _) =>
      state.copy(skippedProjects = SkippedProject(project, reason) :: state.skippedProjects)

    case BuildEvent.CompileProgress(_, _, _) =>
      state // No core state change for progress updates

    case BuildEvent.CompilePhaseChanged(_, _, _, _) =>
      state // Display-only, no state change

    case _: BuildEvent.CompileStalled | _: BuildEvent.CompileResumed =>
      state // Heap pressure events don't affect build state — handled by display

    case _: BuildEvent.LockContention | _: BuildEvent.LockAcquired =>
      state // Lock contention events don't affect build state — handled by display

    case BuildEvent.SuiteTimedOut(project, suite, timeoutMs, threadDumpInfo, _) =>
      val key = s"$project:$suite"
      val timeoutFailure = TestFailure(
        project = project,
        suite = suite,
        test = "(timeout)",
        message = Some(s"Suite idle timeout after ${timeoutMs / 1000}s"),
        throwable = threadDumpInfo.flatMap(_.singleThreadStack),
        output = threadDumpInfo.flatMap(_.dumpFile).map(p => s"Thread dump: $p").toList,
        category = FailureCategory.Timeout
      )
      state.copy(
        suitesCompleted = state.suitesCompleted + 1,
        suitesFailed = state.suitesFailed + 1,
        testsTimedOut = state.testsTimedOut + 1,
        currentlyRunning = state.currentlyRunning - key,
        suiteStartTimes = state.suiteStartTimes - key,
        failures = timeoutFailure :: state.failures
      )

    case BuildEvent.SuiteError(project, suite, error, exitCode, signal, _, _) =>
      val key = s"$project:$suite"
      val desc = signal match {
        case Some(sig) => s"Process crashed (signal $sig)"
        case None =>
          exitCode match {
            case Some(code) => s"Process exited with code $code"
            case None       => error
          }
      }
      val output = state.pendingOutput.getOrElse(key, Nil)
      // Only count as new failure if SuiteFinished didn't already count it
      val alreadyCounted = state.failures.exists(f => f.project == project && f.suite == suite)
      val errorFailure = TestFailure(
        project = project,
        suite = suite,
        test = "(process error)",
        message = Some(desc),
        throwable = None,
        output = output,
        category = FailureCategory.ProcessError
      )
      state.copy(
        suitesCompleted = if (alreadyCounted) state.suitesCompleted else state.suitesCompleted + 1,
        suitesFailed = if (alreadyCounted) state.suitesFailed else state.suitesFailed + 1,
        testsFailed = if (alreadyCounted) state.testsFailed else state.testsFailed + 1,
        currentlyRunning = state.currentlyRunning - key,
        suiteStartTimes = state.suiteStartTimes - key,
        pendingOutput = state.pendingOutput - key,
        failures = errorFailure :: state.failures
      )

    case BuildEvent.Error(project, message, details, _) =>
      val errorFailure = TestFailure(
        project = project,
        suite = "(error)",
        test = "(error)",
        message = Some(message),
        throwable = details,
        output = Nil,
        category = FailureCategory.BuildError
      )
      state.copy(
        testsFailed = state.testsFailed + 1,
        failures = errorFailure :: state.failures
      )

    case BuildEvent.SuiteCancelled(project, suite, reason, _) =>
      val key = s"$project:$suite"
      state.copy(
        suitesCompleted = state.suitesCompleted + 1,
        suitesCancelled = state.suitesCancelled + 1,
        currentlyRunning = state.currentlyRunning - key,
        suiteStartTimes = state.suiteStartTimes - key,
        cancelledSuites = CancelledSuite(project, suite, reason) :: state.cancelledSuites
      )

    case BuildEvent.LinkStarted(project, _, _) =>
      state.copy(currentlyLinking = state.currentlyLinking + project)

    case BuildEvent.LinkSucceeded(project, _, durationMs, _) =>
      state.copy(
        currentlyLinking = state.currentlyLinking - project,
        linksCompleted = state.linksCompleted + 1,
        totalTaskTimeMs = state.totalTaskTimeMs + durationMs
      )

    case BuildEvent.LinkFailed(project, platform, durationMs, error, _) =>
      state.copy(
        currentlyLinking = state.currentlyLinking - project,
        linksCompleted = state.linksCompleted + 1,
        linksFailed = state.linksFailed + 1,
        linkFailures = LinkFailure(project, platform, error) :: state.linkFailures,
        totalTaskTimeMs = state.totalTaskTimeMs + durationMs
      )

    case _: BuildEvent.WorkspaceBusy | _: BuildEvent.WorkspaceReady =>
      // Workspace coordination events don't affect build state — handled by TUI display
      state

    case BuildEvent.ConnectionLost(_, _) =>
      // BSP connection died — mark all currently running suites as cancelled.
      // Their results will never arrive since the server is gone.
      val cancelledFromRunning = state.currentlyRunning.toList.map { key =>
        // key format is "project:suite" or "project:suite:test"
        val parts = key.split(":", 3)
        CancelledSuite(
          project = parts.headOption.getOrElse(""),
          suite = if (parts.length >= 2) parts(1) else key,
          reason = Some("BSP connection lost")
        )
      }
      // Only cancel suite-level entries (project:suite), not individual tests (project:suite:test)
      val suiteLevelCancelled = cancelledFromRunning.filter(_.suite.nonEmpty).distinctBy(cs => (cs.project, cs.suite))
      state.copy(
        suitesCancelled = state.suitesCancelled + suiteLevelCancelled.size,
        suitesCompleted = state.suitesCompleted + suiteLevelCancelled.size,
        currentlyRunning = Set.empty,
        suiteStartTimes = Map.empty,
        cancelledSuites = suiteLevelCancelled ++ state.cancelledSuites
      )

    case BuildEvent.TestRunCompleted(
          _,
          totalPassed,
          totalFailed,
          totalSkipped,
          totalIgnored,
          suitesTotal,
          suitesCompleted,
          suitesFailed,
          suitesCancelled,
          _,
          _
        ) =>
      // Authoritative source from BSP response — override accumulated counts.
      // Notification events (SuiteFinished etc.) are fire-and-forget and may be lost,
      // but the BSP response is reliable (request-response).
      // Also clear cancelledSuites list — the authoritative response tells us the real
      // cancellation count. Any ConnectionLost cancellations that fired during the race
      // between socket EOF and BSP shutdown are spurious.
      val authoritativeCancelled = if (suitesCancelled > 0) state.cancelledSuites.take(suitesCancelled) else Nil
      state.copy(
        testsPassed = totalPassed,
        testsFailed = totalFailed,
        testsSkipped = totalSkipped,
        testsIgnored = totalIgnored,
        suitesTotal = suitesTotal,
        suitesCompleted = suitesCompleted,
        suitesFailed = suitesFailed,
        suitesCancelled = suitesCancelled,
        currentlyRunning = Set.empty,
        cancelledSuites = authoritativeCancelled
      )
  }
}
