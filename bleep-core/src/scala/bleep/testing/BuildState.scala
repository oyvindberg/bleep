package bleep.testing

import bleep.bsp.protocol.{ProcessExit, SuiteOutcome, TestStatus}
import bleep.model.{CrossProjectName, SuiteName, TestName}

/** Canonical state for build/test progress tracking.
  *
  * This is the single source of truth for counting and classification. All display implementations (basic, fancy bridge, TUI) derive their state from this via
  * [[BuildStateReducer.reduce]].
  */
case class BuildState(
    sourcegenRunning: Set[String], // Currently running sourcegen scripts
    sourcegenCompleted: Int,
    sourcegenFailed: Int,
    apResolutionFailed: Int, // Number of projects whose annotation-processor resolution DAG task failed
    kspResolutionFailed: Int, // Number of projects whose KSP processor resolution DAG task failed
    compilesCompleted: Int,
    compilesFailed: Int,
    compilesSkipped: Int,
    compilesCancelled: Int,
    /** Projects whose compile reported [[bleep.bsp.protocol.CompileReason.UpToDate]] — the compiler ran and found nothing to do. Kept as a list rather than a
      * count so a caller can name them.
      */
    upToDateProjects: List[CrossProjectName],
    /** What each successful link wrote, newest first during accumulation. See [[LinkedOutput]]. */
    linkedOutputs: List[LinkedOutput],
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
    currentlyCompiling: Set[CrossProjectName],
    compileStartTimes: Map[CrossProjectName, Long], // project -> timestamp when compile started
    currentlyLinking: Set[CrossProjectName],
    linksCompleted: Int,
    linksFailed: Int,
    linkFailures: List[LinkFailure],
    failures: List[TestFailure],
    skipped: List[TestSkipped],
    cancelledSuites: List[CancelledSuite],
    compileFailures: List[ProjectCompileFailure],
    skippedProjects: List[SkippedProject],
    /** Test projects whose classpath scan found no suites at all — before any `--only` / `--exclude` / tag filter had a say.
      *
      * A project only reaches discovery when it is a test project that compiled, so an empty scan here is never the user narrowing the run: it is compiled test
      * classes that no framework claimed. Kept as a list rather than a count so the verdict can name them.
      */
    testProjectsWithoutSuites: List[CrossProjectName],
    pendingOutput: Map[SuiteKey, List[String]],
    totalTaskTimeMs: Long,
    /** The compile server died mid-run. Not a compile failure — the compiles that finished really did finish — but the run did not complete, so a summary that
      * looked clean while the command failed left the reader to reconcile two blocks that seemed to disagree.
      */
    serverCrashed: Boolean,
    /** Id of the transcript the daemon persisted for this request, from the response. None when the response carried none (older daemon, or the write failed).
      */
    historyId: Option[Long]
) {

  /** Project to BuildSummary (lists are reversed since we prepend during accumulation) */
  def toSummary(durationMs: Long, wasCancelled: Boolean): BuildSummary = {
    val nowMs = System.currentTimeMillis()
    val killedCompiles = currentlyCompiling.toList.sorted.map { project =>
      val startedAt = compileStartTimes.getOrElse(project, nowMs)
      KilledTask(TaskKind.Compile, project.value, nowMs - startedAt)
    }
    val killedLinks = currentlyLinking.toList.sorted.map { project =>
      KilledTask(TaskKind.Link, project.value, 0)
    }
    val killedSuites = runningSuites.toList.sorted.map { key =>
      val startedAt = suiteStartTimes.getOrElse(key, nowMs)
      KilledTask(TaskKind.Test, key.toString, nowMs - startedAt)
    }
    BuildSummary(
      sourcegenFailed = sourcegenFailed,
      apResolutionFailed = apResolutionFailed,
      kspResolutionFailed = kspResolutionFailed,
      compilesCompleted = compilesCompleted,
      compilesFailed = compilesFailed,
      compilesSkipped = compilesSkipped,
      compilesCancelled = compilesCancelled,
      upToDateProjects = upToDateProjects.reverse,
      linkedOutputs = linkedOutputs.reverse,
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
      currentlyRunning = runningSuites.toList.sorted.map(_.suite),
      killedTasks = killedCompiles ++ killedLinks ++ killedSuites,
      failures = failures.reverse,
      skipped = skipped.reverse,
      cancelledSuites = cancelledSuites.reverse,
      compileFailures = compileFailures.reverse,
      linkFailures = linkFailures.reverse,
      skippedProjects = skippedProjects.reverse,
      testProjectsWithoutSuites = testProjectsWithoutSuites.reverse,
      durationMs = durationMs,
      totalTaskTimeMs = totalTaskTimeMs,
      wasCancelled = wasCancelled,
      serverCrashed = serverCrashed,
      filterContext = None,
      historyId = historyId
    )
  }
}

object BuildState {
  val empty: BuildState = BuildState(
    sourcegenRunning = Set.empty,
    sourcegenCompleted = 0,
    sourcegenFailed = 0,
    apResolutionFailed = 0,
    kspResolutionFailed = 0,
    compilesCompleted = 0,
    compilesFailed = 0,
    compilesSkipped = 0,
    compilesCancelled = 0,
    upToDateProjects = Nil,
    linkedOutputs = Nil,
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
    testProjectsWithoutSuites = Nil,
    pendingOutput = Map.empty,
    totalTaskTimeMs = 0,
    serverCrashed = false,
    historyId = None
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

    case BuildEvent.ResolveAnnotationProcessorsFinished(_, success, _, _, _) =>
      if (success) state else state.copy(apResolutionFailed = state.apResolutionFailed + 1)

    case BuildEvent.RunSymbolProcessorsFinished(_, success, _, _, _) =>
      if (success) state else state.copy(kspResolutionFailed = state.kspResolutionFailed + 1)

    case BuildEvent.CompileStarted(project, timestamp) =>
      state.copy(
        currentlyCompiling = state.currentlyCompiling + project,
        compileStartTimes = state.compileStartTimes + (project -> timestamp)
      )

    case BuildEvent.CompilationReason(project, reason, _, _, _, _) =>
      // Informational for display, except for one bit worth keeping: whether the compiler found anything to do. That is the only place a caller can learn a
      // compile was a no-op, and `bleep.Commands.compile` hands it back so a script can skip work that only matters when something recompiled.
      reason match {
        case bleep.bsp.protocol.CompileReason.UpToDate => state.copy(upToDateProjects = project :: state.upToDateProjects)
        case _                                         => state
      }

    case BuildEvent.CompileFinished(project, status, durationMs, _, diagnostics, skippedBecause) =>
      import bleep.bsp.protocol.CompileStatus
      val updatedCompileFailures = status match {
        case CompileStatus.Failed | CompileStatus.Error => ProjectCompileFailure(project, diagnostics) :: state.compileFailures
        case _                                          => state.compileFailures
      }
      val updatedSkippedProjects = status match {
        case CompileStatus.Skipped =>
          val reason = skippedBecause.map(dep => s"dependency ${dep.value} failed").getOrElse("dependency failed")
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

    // `throwable` is bound and used, not discarded. It used to be `_`, with `None` passed to every TestFailure below — so the stack trace travelled all the
    // way from the forked runner into this event and was dropped one line before display. A failing test showed its message and nothing else: for an
    // assertion that is often enough, but for an exception thrown from a constructor the message alone ("ctor boom") says nothing about where it came from,
    // and the frames were sitting right here. The JUnit XML had them the whole time, which is why the two disagreed.
    case BuildEvent.TestFinished(project, suite, test, status, durationMs, message, throwable, _, location) =>
      val testKey = TestKey(project, suite, test)
      val suiteKey = testKey.suiteKey

      val updatedFailures = status match {
        case TestStatus.Failed | TestStatus.Error =>
          val output = state.pendingOutput.getOrElse(suiteKey, Nil)
          TestFailure(project, suite, test, message, throwable, output, FailureCategory.TestFailed, location) :: state.failures
        case TestStatus.Timeout =>
          val output = state.pendingOutput.getOrElse(suiteKey, Nil)
          TestFailure(project, suite, test, message, throwable, output, FailureCategory.Timeout, location) :: state.failures
        case TestStatus.Cancelled =>
          val output = state.pendingOutput.getOrElse(suiteKey, Nil)
          TestFailure(project, suite, test, message, throwable, output, FailureCategory.Cancelled, location) :: state.failures
        case TestStatus.AssumptionFailed => state.failures // not a failure
        case _                           => state.failures
      }

      val updatedSkipped = status match {
        case TestStatus.Skipped | TestStatus.Ignored | TestStatus.Pending =>
          TestSkipped(project, suite, test, status, None) :: state.skipped
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

    case BuildEvent.SuiteFinished(project, suite, outcome, _, _) =>
      val key = SuiteKey(project, suite)
      // Check if SuiteError already counted this suite (SuiteError can arrive before SuiteFinished)
      val alreadyCounted = state.failures.exists(f => f.project == project && f.suite == suite && f.category == FailureCategory.ProcessError)
      val existingFailuresForSuite = state.failures.count(f => f.project == project && f.suite == suite)
      // The outcome variant, not count arithmetic, says why (if) the suite failed. A failing
      // outcome with no per-test TestFinished events (Empty / NoFrameworkMatched / Errored, or a
      // suite-level failure whose individual events were lost) gets one synthetic failure so the
      // summary shows a reason and count-based gates (toEither) see it.
      val failureReason: Option[String] = outcome match {
        case SuiteOutcome.Executed(_, failed, _, _) if failed > 0 =>
          Some(s"Suite reported $failed failure(s) but no individual test results were captured")
        case _: SuiteOutcome.Executed => None
        // Not a failure: a suite with no tests in it is a normal thing to have. See SuiteOutcome.isFailure.
        case SuiteOutcome.Empty               => None
        case SuiteOutcome.NoFrameworkMatched  => Some(s"No test framework/engine claimed ${suite.value}")
        case SuiteOutcome.Errored(message, _) => Some(message)
      }
      val syntheticFailures = failureReason match {
        case Some(msg) if existingFailuresForSuite == 0 =>
          List(
            TestFailure(
              project = project,
              suite = suite,
              test = TestName("(suite failed)"),
              message = Some(msg),
              throwable = None,
              output = state.pendingOutput.getOrElse(key, Nil),
              category = FailureCategory.ProcessError,
              // synthesised from a suite-level failure, so there is no throwable to recover a frame from
              location = None
            )
          )
        case _ => Nil
      }
      // The suite's complete captured output, handed to every failure it produced.
      //
      // A failure records whatever had been captured at the moment that *test* finished, and for several frameworks the explanation is written later: weaver
      // logs its failure summary once the whole suite is done, so a weaver failure was displayed with bleep's own startup chatter and nothing else, while the
      // JUnit XML — which accumulates per suite — had the reason all along. `pendingOutput` is only cleared here, so at this point it holds everything.
      val suiteOutput = state.pendingOutput.getOrElse(key, Nil)
      val failuresWithSuiteOutput = state.failures.map { failure =>
        if (failure.project == project && failure.suite == suite) failure.copy(output = suiteOutput) else failure
      }

      val isFailure = outcome.isFailure
      // For a failing suite with no per-test failures already counted, surface one failed test so
      // count-based gates see it even when other suites in the run passed. Executed(failed>0) whose
      // per-test events already incremented testsFailed is excluded via existingFailuresForSuite.
      val addFailedTest = isFailure && !alreadyCounted && existingFailuresForSuite == 0
      state.copy(
        suitesCompleted = if (alreadyCounted) state.suitesCompleted else state.suitesCompleted + 1,
        suitesFailed = if (alreadyCounted) state.suitesFailed else state.suitesFailed + (if (isFailure) 1 else 0),
        testsFailed = if (addFailedTest) state.testsFailed + 1 else state.testsFailed,
        runningSuites = state.runningSuites - key,
        suiteStartTimes = state.suiteStartTimes - key,
        pendingOutput = state.pendingOutput - key,
        failures = syntheticFailures ++ failuresWithSuiteOutput
      )

    case BuildEvent.Output(project, suite, line, _, _) =>
      val key = SuiteKey(project, suite)
      state.copy(
        pendingOutput = state.pendingOutput.updated(
          key,
          state.pendingOutput.getOrElse(key, Nil) :+ line
        )
      )

    case BuildEvent.SuitesDiscovered(project, _, _, discoveredBeforeFilters, isTestProject, _) =>
      // Two conditions, both required. `None` is an event from a peer that predates the field — no evidence, so no verdict. And a project that never claimed to
      // be a test project has not contradicted itself by holding no suites: `bleep test` and `bleep ci` pass plain libraries through discovery as a matter of
      // course, and failing those would make the whole check unusable.
      if (isTestProject && discoveredBeforeFilters.contains(0)) state.copy(testProjectsWithoutSuites = project :: state.testProjectsWithoutSuites)
      else state

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
      // jstack dump arrives via `threadDumpInfo.singleThreadStack` (see ReactiveBsp's SuiteTimedOut translation);
      // expose it as `failure.throwable` so BuildDisplay's summary Timeouts section renders it under "Stack trace:".
      val timeoutFailure = TestFailure(
        project = project,
        suite = suite,
        test = TestName("(timeout)"),
        message = Some(s"Suite idle timeout after ${timeoutMs / 1000}s"),
        throwable = threadDumpInfo.flatMap(_.singleThreadStack),
        output = threadDumpInfo.flatMap(_.dumpFile).map(p => s"Thread dump: $p").toList,
        category = FailureCategory.Timeout,
        // a jstack dump of a hung suite, not a thrown exception — no failing frame to point at
        location = None
      )
      state.copy(
        suitesCompleted = state.suitesCompleted + 1,
        suitesFailed = state.suitesFailed + 1,
        testsTimedOut = state.testsTimedOut + 1,
        runningSuites = state.runningSuites - key,
        suiteStartTimes = state.suiteStartTimes - key,
        pendingOutput = state.pendingOutput - key,
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
      // A suite that dies can produce both events: `SuiteFinished` says it reported failures with no per-test results, then `SuiteError` says the process
      // exited non-zero. They are one event from a reader's side, and reporting them as two put the same suite in the Process Errors list twice — once saying
      // nothing was captured, once saying it exited 1, neither mentioning the other. The counts were already guarded against this; the list was not.
      //
      // Merged rather than dropped, because each half carries something the other lacks: the reason, and the exit status.
      val syntheticForSuite =
        state.failures.find(f => f.project == project && f.suite == suite && (f.test.value == "(suite failed)" || f.test.value == "(process error)"))
      syntheticForSuite match {
        case Some(existing) =>
          val merged = existing.copy(
            message = existing.message.map(m => if (m.contains(desc)) m else s"$m\n$desc").orElse(Some(desc)),
            output = if (existing.output.nonEmpty) existing.output else output,
            category = FailureCategory.ProcessError
          )
          state.copy(
            runningSuites = state.runningSuites - key,
            suiteStartTimes = state.suiteStartTimes - key,
            pendingOutput = state.pendingOutput - key,
            failures = state.failures.map(f => if (f eq existing) merged else f)
          )
        case None =>
          val errorFailure = TestFailure(
            project = project,
            suite = suite,
            test = TestName("(process error)"),
            message = Some(desc),
            throwable = None,
            output = output,
            category = FailureCategory.ProcessError,
            // the forked JVM died; whatever it was doing never produced a throwable
            location = None
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
      }

    case BuildEvent.Error(message, details, _) =>
      // Error events are project-less — use a synthetic project name for the failure record
      val errorFailure = TestFailure(
        project = CrossProjectName(bleep.model.ProjectName("(build)"), None),
        suite = SuiteName("(error)"),
        test = TestName("(error)"),
        message = Some(message),
        throwable = details,
        output = Nil,
        category = FailureCategory.BuildError,
        // build-level error, not attributable to a line in any suite
        location = None
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

    case BuildEvent.LinkSucceeded(project, platform, durationMs, generatedFiles, _) =>
      // The linker's own list of what it wrote, kept rather than dropped. It is the only authority on where the output landed: the directory layout under
      // `link-output/` belongs to bleep and has changed before, so a caller reconstructing that path is guessing. Handed to scripts via
      // `bleep.Commands.link`.
      state.copy(
        currentlyLinking = state.currentlyLinking - project,
        linksCompleted = state.linksCompleted + 1,
        linkedOutputs = LinkedOutput(project, platform, generatedFiles.map(java.nio.file.Path.of(_))) :: state.linkedOutputs,
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

    case BuildEvent.ConnectionLost(_) =>
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

    case BuildEvent.HistoryRecorded(historyId, _) =>
      state.copy(historyId = Some(historyId))
  }
}
