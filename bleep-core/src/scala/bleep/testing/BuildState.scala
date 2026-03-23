package bleep.testing

import bleep.bsp.protocol.{ProcessExit, TestStatus}

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
    runningSuites: Set[SuiteKey],
    runningTests: Set[TestKey],
    suiteStartTimes: Map[SuiteKey, Long],
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
    pendingOutput: Map[SuiteKey, List[String]],
    totalTaskTimeMs: Long
) {

  /** Project to BuildSummary (lists are reversed since we prepend during accumulation) */
  def toSummary(durationMs: Long, wasCancelled: Boolean): BuildSummary = {
    val nowMs = System.currentTimeMillis()
    val killedCompiles = currentlyCompiling.toList.sorted.map { project =>
      val startedAt = compileStartTimes.getOrElse(project, nowMs)
      KilledTask(TaskKind.Compile, project, nowMs - startedAt)
    }
    val killedLinks = currentlyLinking.toList.sorted.map { project =>
      KilledTask(TaskKind.Link, project, 0)
    }
    val killedSuites = runningSuites.toList.sorted.map { key =>
      val startedAt = suiteStartTimes.getOrElse(key, nowMs)
      KilledTask(TaskKind.Test, key.toString, nowMs - startedAt)
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
      currentlyRunning = runningSuites.toList.sorted.map(_.toString),
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
    runningSuites = Set.empty,
    runningTests = Set.empty,
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
      import bleep.bsp.protocol.CompileStatus
      val updatedCompileFailures = status match {
        case CompileStatus.Failed | CompileStatus.Error => ProjectCompileFailure(project, diagnostics) :: state.compileFailures
        case _                                          => state.compileFailures
      }
      val updatedSkippedProjects = status match {
        case CompileStatus.Skipped =>
          val reason = skippedBecause.map(dep => s"dependency $dep failed").getOrElse("dependency failed")
          SkippedProject(project, reason) :: state.skippedProjects
        case _ => state.skippedProjects
      }
      state.copy(
        compilesCompleted = state.compilesCompleted + 1,
        compilesFailed = state.compilesFailed + (if (!status.isSuccess && status != CompileStatus.Skipped && status != CompileStatus.Cancelled) 1 else 0),
        compilesSkipped = state.compilesSkipped + (if (status == CompileStatus.Skipped) 1 else 0),
        compilesCancelled = state.compilesCancelled + (if (status == CompileStatus.Cancelled) 1 else 0),
        currentlyCompiling = state.currentlyCompiling - project,
        compileStartTimes = state.compileStartTimes - project,
        compileFailures = updatedCompileFailures,
        skippedProjects = updatedSkippedProjects,
        totalTaskTimeMs = state.totalTaskTimeMs + durationMs
      )

    case BuildEvent.SuiteStarted(project, suite, timestamp) =>
      val key = SuiteKey(project, suite)
      state.copy(
        suitesTotal = state.suitesTotal + 1,
        runningSuites = state.runningSuites + key,
        suiteStartTimes = state.suiteStartTimes + (key -> timestamp)
      )

    case BuildEvent.TestStarted(project, suite, test, _) =>
      val key = TestKey(project, suite, test)
      state.copy(
        testsTotal = state.testsTotal + 1,
        runningTests = state.runningTests + key
      )

    case BuildEvent.TestFinished(project, suite, test, status, durationMs, message, _, _) =>
      val testKey = TestKey(project, suite, test)
      val suiteKey = testKey.suiteKey

      val updatedFailures = status match {
        case TestStatus.Failed | TestStatus.Error =>
          val output = state.pendingOutput.getOrElse(suiteKey, Nil)
          TestFailure(project, suite, test, message, None, output, FailureCategory.TestFailed) :: state.failures
        case TestStatus.Timeout =>
          val output = state.pendingOutput.getOrElse(suiteKey, Nil)
          TestFailure(project, suite, test, message, None, output, FailureCategory.Timeout) :: state.failures
        case TestStatus.Cancelled =>
          val output = state.pendingOutput.getOrElse(suiteKey, Nil)
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
        runningTests = state.runningTests - testKey,
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
      val key = SuiteKey(project, suite)
      // Check if SuiteError already counted this suite (SuiteError can arrive before SuiteFinished)
      val alreadyCounted = state.failures.exists(f => f.project == project && f.suite == suite && f.category == FailureCategory.ProcessError)
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
        suitesCompleted = if (alreadyCounted) state.suitesCompleted else state.suitesCompleted + 1,
        suitesFailed = if (alreadyCounted) state.suitesFailed else state.suitesFailed + (if (failed > 0) 1 else 0),
        runningSuites = state.runningSuites - key,
        suiteStartTimes = state.suiteStartTimes - key,
        pendingOutput = state.pendingOutput - key,
        failures = syntheticFailures ++ state.failures
      )

    case BuildEvent.Output(project, suite, line, _, _) =>
      val key = SuiteKey(project, suite)
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
      val key = SuiteKey(project, suite)
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
        runningSuites = state.runningSuites - key,
        suiteStartTimes = state.suiteStartTimes - key,
        failures = timeoutFailure :: state.failures
      )

    case BuildEvent.SuiteError(project, suite, error, processExit, _, _) =>
      val key = SuiteKey(project, suite)
      val desc = processExit match {
        case ProcessExit.Signal(sig)    => s"Process crashed (signal $sig)"
        case ProcessExit.ExitCode(code) => s"Process exited with code $code"
        case ProcessExit.Unknown        => error
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
        runningSuites = state.runningSuites - key,
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
      val key = SuiteKey(project, suite)
      state.copy(
        suitesCompleted = state.suitesCompleted + 1,
        suitesCancelled = state.suitesCancelled + 1,
        runningSuites = state.runningSuites - key,
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
      val cancelledFromRunning = state.runningSuites.toList.map { key =>
        CancelledSuite(
          project = key.project,
          suite = key.suite,
          reason = Some("BSP connection lost")
        )
      }
      state.copy(
        suitesCancelled = state.suitesCancelled + cancelledFromRunning.size,
        suitesCompleted = state.suitesCompleted + cancelledFromRunning.size,
        runningSuites = Set.empty,
        runningTests = Set.empty,
        suiteStartTimes = Map.empty,
        cancelledSuites = cancelledFromRunning ++ state.cancelledSuites
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
      // BSP response is the authoritative source for suite-level counts (suitesTotal, etc.)
      // which must be able to correct DOWN (e.g. ConnectionLost inflates cancellations).
      // For test-level counts (passed/failed/skipped/ignored), use math.max: if server counts
      // are correct, they match accumulated from individual TestFinished notifications (max = either).
      // If server counts are wrong due to cancellation race (0), accumulated counts are preserved.
      val authoritativeCancelled = if (suitesCancelled > 0) state.cancelledSuites.take(suitesCancelled) else Nil
      state.copy(
        testsPassed = math.max(state.testsPassed, totalPassed),
        testsFailed = math.max(state.testsFailed, totalFailed),
        testsSkipped = math.max(state.testsSkipped, totalSkipped),
        testsIgnored = math.max(state.testsIgnored, totalIgnored),
        suitesTotal = suitesTotal,
        suitesCompleted = suitesCompleted,
        suitesFailed = suitesFailed,
        suitesCancelled = suitesCancelled,
        runningSuites = Set.empty,
        runningTests = Set.empty,
        cancelledSuites = authoritativeCancelled
      )
  }
}
