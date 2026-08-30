package bleep.testing

import bleep.bsp.protocol.{BleepBspProtocol, CompileReason, DiagnosticSeverity, LinkPlatformName, ProcessExit, SuiteOutcome, TestStatus}
import bleep.bsp.protocol.BleepBspProtocol.BuildMode
import bleep.model.{CrossProjectName, SuiteName, TestName}
import bleep.testing.BleepConsole as SConsole
import cats.effect._
import cats.syntax.all._

import scala.collection.mutable

/** Displays build progress in real-time.
  *
  * Features:
  *   - Shows compile and test progress
  *   - Live-updates passed/failed/skipped counts
  *   - Collects failures for summary at end
  *   - Optional quiet mode (only show failures)
  */
trait BuildDisplay {

  /** Handle a test event */
  def handle(event: BuildEvent): IO[Unit]

  /** Get the current summary */
  def summary: IO[BuildSummary]

  /** Reset display state (e.g., before retry after server crash) */
  def reset: IO[Unit]

  /** Record that the compile server died, so the summary says the run did not complete rather than reading as clean next to a failure message. */
  def markServerCrashed: IO[Unit]

  /** Print final summary. Pass a [[FilterContext]] when test filters are active so the summary can show which filters ran and what they pruned.
    *
    * `failureDetails = false` keeps the one-line counts (and the `History: #N` pointer) but suppresses the verbose failure sections — used by `--diff`, where
    * the mechanical diff printed right after carries the failure messages (its newlyFailing entries / newDiagnostics) and repeating them above it is noise.
    */
  def printSummary(filterContext: Option[FilterContext], failureDetails: Boolean): IO[Unit]
}

/** Snapshot of the filters the user asked for in this run. Attached to `BuildSummary` purely for display — does not affect control flow.
  *
  *   - `candidateProjects`: the set of projects after the CLI has expanded the user's globs (`jvm3`, prefix groups, etc.) but before any test-tag pre-filter.
  *     This is what would have been built/tested if `--only-tag` were absent.
  *   - `selectedProjects`: subset of `candidateProjects` that actually got dispatched after the `--only-tag` pre-filter. Equal to `candidateProjects` when no
  *     `--only-tag` is active.
  *
  * We don't keep the raw user-typed args (globs like `jvm3` or `mylib`) because glob resolution lives in `ProjectGlobs` upstream of this layer — by the time we
  * have a `ReactiveBsp` to run, projects are already a `Set[CrossProjectName]`. The summary still shows the user a meaningful "N of M" because both N and M are
  * in their (post-expansion) terms.
  *
  * When any filter field is non-empty the summary appends a "Filters active" block so users can verify what their flags did at a glance.
  */
case class FilterContext(
    candidateProjects: Set[CrossProjectName],
    selectedProjects: Set[CrossProjectName],
    only: List[String],
    exclude: List[String],
    includeTags: List[String],
    excludeTags: List[String]
) {
  def anyActive: Boolean = only.nonEmpty || exclude.nonEmpty || includeTags.nonEmpty || excludeTags.nonEmpty
  def droppedProjects: Set[CrossProjectName] = candidateProjects -- selectedProjects
}

case class BuildSummary(
    sourcegenFailed: Int,
    apResolutionFailed: Int,
    kspResolutionFailed: Int,
    compilesCompleted: Int,
    compilesFailed: Int,
    compilesSkipped: Int,
    compilesCancelled: Int,
    /** Projects whose compile found nothing to do. See [[noOp]]. */
    upToDateProjects: List[CrossProjectName],
    suitesTotal: Int,
    suitesCompleted: Int,
    suitesFailed: Int,
    suitesCancelled: Int,
    testsTotal: Int,
    testsPassed: Int,
    testsFailed: Int,
    testsTimedOut: Int,
    testsCancelled: Int,
    testsSkipped: Int,
    testsIgnored: Int,
    currentlyRunning: List[SuiteName],
    killedTasks: List[KilledTask], // tasks that were started but never finished (cancelled builds)
    failures: List[TestFailure],
    skipped: List[TestSkipped],
    cancelledSuites: List[CancelledSuite],
    compileFailures: List[ProjectCompileFailure],
    linkFailures: List[LinkFailure],
    skippedProjects: List[SkippedProject],
    /** Test projects whose classpath scan found no suites at all, before any filter applied. See [[toEither]]. */
    testProjectsWithoutSuites: List[CrossProjectName],
    durationMs: Long,
    totalTaskTimeMs: Long, // Sum of all individual task durations (compile + link + test, for parallelism stats)
    wasCancelled: Boolean,
    /** The compile server died mid-run. The compiles that completed really did complete, so the counts stay honest — but the run did not finish, and a summary
      * that reads as clean while the command fails is a summary the reader has to reconcile against the error below it.
      */
    serverCrashed: Boolean,
    filterContext: Option[FilterContext],
    /** Id of the transcript the daemon persisted for this request (`bleep history show <id>` expands it). None when the response carried none. */
    historyId: Option[Long]
) {

  /** True when every project that compiled was already up to date, so the run produced no new class files.
    *
    * This is BSP's `CompileReport.noOp` computed from what bleep already knows: the server reports a [[bleep.bsp.protocol.CompileReason]] per project, and
    * `UpToDate` is the one that means the compiler ran and found nothing to do. A caller uses it to skip work that only matters when something recompiled — a
    * deploy step, a docker build, a downstream publish.
    *
    * A run in which nothing compiled at all is NOT a no-op: there was no compile to be a no-op about, and answering `true` there would tell a deploy script it
    * may skip on the strength of a run that never looked.
    */
  def noOp: Boolean = compilesCompleted > 0 && upToDateProjects.size == compilesCompleted

  /** Convert this summary to Either — Left for cancelled/failed builds, Right for success. Use this to gate post-build steps (publishing, etc.) */
  def toEither: Either[bleep.BleepException, Unit] =
    if (serverCrashed)
      Left(new bleep.BleepException.Text("Build did not complete: the compile server crashed"))
    else if (wasCancelled || compilesCancelled > 0)
      Left(new bleep.BleepException.Text("Build cancelled by user"))
    else if (compileFailures.nonEmpty)
      Left(new bleep.BleepException.Text(s"Build failed: ${compileFailures.size} project(s) failed to compile"))
    else if (linkFailures.nonEmpty)
      Left(new bleep.BleepException.Text(s"Build failed: ${linkFailures.size} project(s) failed to link"))
    else if (sourcegenFailed > 0)
      Left(new bleep.BleepException.Text(s"Source generation failed for $sourcegenFailed project(s)"))
    else if (apResolutionFailed > 0)
      Left(new bleep.BleepException.Text(s"Annotation processor resolution failed for $apResolutionFailed project(s)"))
    else if (kspResolutionFailed > 0)
      Left(new bleep.BleepException.Text(s"KSP processor resolution failed for $kspResolutionFailed project(s)"))
    else {
      val testProblems = testsFailed + testsTimedOut + testsCancelled
      val testsObserved = testsPassed + testsFailed + testsSkipped + testsIgnored + testsTimedOut + testsCancelled
      if (testProblems > 0 || suitesCancelled > 0) {
        val parts = List.newBuilder[String]
        parts += s"$testsPassed passed"
        if (testsFailed > 0) parts += s"$testsFailed failed"
        if (testsTimedOut > 0) parts += s"$testsTimedOut timed out"
        if (testsCancelled > 0) parts += s"$testsCancelled cancelled"
        if (suitesCancelled > 0) parts += s"$suitesCancelled suites cancelled"
        Left(new bleep.BleepException.Text(s"Tests failed: ${parts.result().mkString(", ")}"))
      } else if (testProjectsWithoutSuites.nonEmpty) {
        // A project reaches discovery only when it is `isTestProject: true` and its classes compiled, and the count checked here is the one taken *before*
        // `--only` / `--exclude` / tag filters. So this is not the user narrowing a run to nothing: it is compiled test classes that no framework recognised —
        // a missing test dependency, a framework whose fingerprints match nothing bleep scanned, suites that were renamed out of existence. Every one of those
        // used to report "0 tests executed" and exit 0, which is the worst possible answer: CI goes green precisely because the tests stopped running.
        val names = testProjectsWithoutSuites.map(_.value).sorted
        val shown = names.take(5).mkString(", ")
        val suffix = if (names.size > 5) s", … +${names.size - 5} more" else ""
        Left(
          new bleep.BleepException.Text(
            s"No test suites found in ${names.size} test project(s): $shown$suffix. " +
              "The classes compiled but no test framework claimed them — check the project's test dependencies and `testFrameworks:`."
          )
        )
      } else if (suitesCompleted > 0 && testsObserved == 0)
        // Suites ran to completion but not a single test of any status was observed. This is
        // never a legitimate green build — it is the silent-zero signature (stale test
        // discovery, a framework with no matching engine, a runner that exited 0 without
        // executing). A `Suites: N, Tests: 0` result must not gate CI as success.
        Left(new bleep.BleepException.Text(s"$suitesCompleted suite(s) completed but executed 0 tests"))
      else
        Right(())
    }
}

object BuildSummary {

  /** How a test failure's stack trace is printed: the framework's own machinery cut off the bottom, then any repeating cycle collapsed.
    *
    * In that order. [[StackTraceElision]] reads real frames and needs to see them; [[StackTraceCycles]] replaces a repeated run with a synthetic `... above N
    * frames repeated` line, which is not a frame and would stop the cut short.
    *
    * Only failures that came from a test framework go through here. A bleep crash or a build error is bleep's own stack, and cutting `bleep.testing.runner` off
    * the bottom of that would hide the thing being reported.
    */
  private def renderStack(stackTrace: String): List[String] =
    StackTraceCycles.collapse(StackTraceElision.elide(stackTrace).mkString("\n"))

  /** Format a complete summary for display after a build/test run. Returns lines to print. Used by both TUI and non-TUI paths.
    *
    * `failureDetails = false` stops after the counts/duration/history/filter block — see [[BuildDisplay.printSummary]].
    */
  def formatSummary(summary: BuildSummary, mode: BuildMode, failureDetails: Boolean): List[String] = {
    import BleepConsole as C
    val lines = List.newBuilder[String]

    // Anything other than passed/skipped/ignored means failure
    val totalProblems = summary.testsFailed + summary.testsTimedOut + summary.testsCancelled
    val hasFailures =
      summary.sourcegenFailed > 0 || summary.compileFailures.nonEmpty || summary.linkFailures.nonEmpty || totalProblems > 0 || summary.suitesCancelled > 0 ||
        summary.serverCrashed
    val wasCancelled = summary.wasCancelled || summary.compilesCancelled > 0
    val statusColor = if (hasFailures) C.RED else if (wasCancelled) C.YELLOW else C.GREEN
    val statusIcon = if (hasFailures) "x" else if (wasCancelled) "!" else "✓"
    val wasCancelledStr = if (wasCancelled) " (cancelled by user)" else ""
    lines += ""
    lines += s"$statusColor${C.BOLD}$statusIcon Build Summary$wasCancelledStr${C.RESET}"
    lines += ""

    // --- Counts ---
    mode match {
      case BuildMode.Test =>
        val passedStr = s"${C.GREEN}${summary.testsPassed} passed${C.RESET}"
        val failedStr = if (summary.testsFailed > 0) s"${C.RED}${summary.testsFailed} failed${C.RESET}" else s"${summary.testsFailed} failed"
        val parts = List.newBuilder[String]
        parts += passedStr
        parts += failedStr
        if (summary.testsTimedOut > 0) parts += s"${C.RED}${summary.testsTimedOut} timed out${C.RESET}"
        if (summary.testsCancelled > 0) parts += s"${C.RED}${summary.testsCancelled} cancelled${C.RESET}"
        if (summary.testsSkipped > 0) parts += s"${C.YELLOW}${summary.testsSkipped} skipped${C.RESET}"
        if (summary.testsIgnored > 0) parts += s"${C.YELLOW}${summary.testsIgnored} ignored${C.RESET}"
        lines += s"  Tests: ${parts.result().mkString(", ")}"
        if (summary.suitesTotal > 0) {
          val unaccounted = summary.suitesTotal - summary.suitesCompleted - summary.suitesCancelled
          if (unaccounted > 0) {
            // Something didn't finish — show X/Y to make the gap obvious
            val parts = List.newBuilder[String]
            parts += s"${C.RED}${summary.suitesCompleted}/${summary.suitesTotal} completed${C.RESET}"
            if (summary.suitesCancelled > 0) parts += s"${C.RED}${summary.suitesCancelled} cancelled${C.RESET}"
            parts += s"${C.RED}$unaccounted did not finish${C.RESET}"
            lines += s"  Suites: ${parts.result().mkString(", ")}"
            if (summary.currentlyRunning.nonEmpty) {
              lines += s"  Still running: ${summary.currentlyRunning.map(_.value).mkString(", ")}"
            }
          } else {
            // All suites accounted for — clean summary
            val cancelledSuffix = if (summary.suitesCancelled > 0) s", ${C.RED}${summary.suitesCancelled} cancelled${C.RESET}" else ""
            lines += s"  Suites: ${summary.suitesTotal} total$cancelledSuffix"
          }
        }
      case BuildMode.Compile =>
        val succeeded = summary.compilesCompleted - summary.compilesFailed - summary.compilesSkipped - summary.compilesCancelled
        val failedStr = if (summary.compilesFailed > 0) s"${C.RED}${summary.compilesFailed} failed${C.RESET}" else s"${summary.compilesFailed} failed"
        val skippedStr = if (summary.compilesSkipped > 0) s", ${C.YELLOW}${summary.compilesSkipped} skipped${C.RESET}" else ""
        val cancelledStr = if (summary.compilesCancelled > 0) s", ${C.YELLOW}${summary.compilesCancelled} cancelled${C.RESET}" else ""
        lines += s"  Compiled: ${C.GREEN}$succeeded succeeded${C.RESET}, $failedStr$skippedStr$cancelledStr"
      case BuildMode.Link(_) =>
        lines += s"  Linking completed"
      case BuildMode.Run(mainClass, _) =>
        lines += s"  Ran: $mainClass"
    }
    val durationStr = s"${summary.durationMs / 1000.0}s"
    val parallelismStr = if (summary.totalTaskTimeMs > summary.durationMs) {
      val totalSec = summary.totalTaskTimeMs / 1000.0
      val parallelism = summary.totalTaskTimeMs.toDouble / summary.durationMs
      f" (total task time: ${totalSec}%.1fs, ${parallelism}%.1fx parallelism)"
    } else ""
    lines += s"  Duration: $durationStr$parallelismStr"
    summary.historyId.foreach(id => lines += s"  History:  #$id (bleep history show $id)")

    // --- Filter accounting (test mode only; only when something was filtered) ---
    mode match {
      case BuildMode.Test =>
        summary.filterContext.foreach { ctx =>
          // Project line: "Projects: 2/5 selected (3 pre-filtered by --only-tag slow: foo, bar, baz)"
          val totalCandidates = ctx.candidateProjects.size
          val selectedCount = ctx.selectedProjects.size
          val droppedCount = ctx.droppedProjects.size
          if (droppedCount > 0) {
            val droppedNames = ctx.droppedProjects.toList.map(_.value).sorted
            val droppedShown = droppedNames.take(5).mkString(", ")
            val droppedSuffix = if (droppedNames.size > 5) s", … +${droppedNames.size - 5} more" else ""
            lines += s"  Projects: $selectedCount/$totalCandidates selected ($droppedCount pre-filtered by --only-tag ${ctx.includeTags.mkString(",")}: $droppedShown$droppedSuffix)"
          } else if (ctx.anyActive) {
            lines += s"  Projects: $selectedCount/$totalCandidates selected"
          }
          // Filter list — concrete reproduction of every flag in play, so a reader can replay the run.
          if (ctx.anyActive) {
            val parts = scala.collection.mutable.ListBuffer.empty[String]
            if (ctx.only.nonEmpty) parts += s"--only ${ctx.only.mkString(",")}"
            if (ctx.exclude.nonEmpty) parts += s"--exclude ${ctx.exclude.mkString(",")}"
            if (ctx.includeTags.nonEmpty) parts += s"--only-tag ${ctx.includeTags.mkString(",")}"
            if (ctx.excludeTags.nonEmpty) parts += s"--exclude-tag ${ctx.excludeTags.mkString(",")}"
            lines += s"  Filters active: ${parts.mkString(" · ")}"
          }
        }
      case _ => ()
    }

    lines += ""

    // `--diff` mode: the mechanical diff printed after this block carries the failure detail; stop here.
    if (!failureDetails) return lines.result()

    // === Killed tasks (cancelled builds) ===
    if (summary.killedTasks.nonEmpty) {
      lines += s"${C.YELLOW}${C.BOLD}Killed Tasks (${summary.killedTasks.size})${C.RESET}"
      summary.killedTasks.foreach { kt =>
        val elapsed = if (kt.elapsedMs > 0) {
          val secs = kt.elapsedMs / 1000.0
          f" (ran for ${secs}%.1fs)"
        } else ""
        lines += s"  ${C.YELLOW}!${C.RESET} ${kt.kind.label}: ${kt.project}$elapsed"
      }
      lines += ""
    }

    // === Story: sourcegen and compile errors and their consequences ===

    if (summary.sourcegenFailed > 0) {
      lines += s"${C.RED}${C.BOLD}Sourcegen Failures (${summary.sourcegenFailed})${C.RESET}"
      lines += s"  ${summary.sourcegenFailed} sourcegen script(s) failed. Check output above for details."
      lines += ""
    }

    if (summary.compileFailures.nonEmpty) {
      lines += s"${C.RED}${C.BOLD}Compilation Failures (${summary.compileFailures.size})${C.RESET}"
      lines += ""

      summary.compileFailures.foreach { cf =>
        val errors = cf.diagnostics.filter(_.severity == DiagnosticSeverity.Error)
        val warnings = cf.diagnostics.filter(_.severity == DiagnosticSeverity.Warning)
        val countParts = List(
          if (errors.nonEmpty) Some(s"${errors.size} error${if (errors.size != 1) "s" else ""}") else None,
          if (warnings.nonEmpty) Some(s"${warnings.size} warning${if (warnings.size != 1) "s" else ""}") else None
        ).flatten
        val countSuffix = if (countParts.nonEmpty) s" (${countParts.mkString(", ")})" else ""
        lines += s"${C.RED}x ${cf.project.value}${C.RESET}$countSuffix"
        errors.take(10).foreach { diag =>
          val text = diag.rendered.getOrElse(diag.message)
          text.linesIterator.foreach { line =>
            lines += s"  ${C.RED}|${C.RESET} $line"
          }
        }
        if (errors.size > 10)
          lines += s"  ${C.RED}|${C.RESET} ... and ${errors.size - 10} more errors"
        warnings.take(5).foreach { diag =>
          val text = diag.rendered.getOrElse(diag.message)
          text.linesIterator.foreach { line =>
            lines += s"  ${C.YELLOW}|${C.RESET} ${C.sanitize(line)}"
          }
        }
        if (warnings.size > 5)
          lines += s"  ${C.YELLOW}|${C.RESET} ... and ${warnings.size - 5} more warnings"

        // Show projects skipped due to this compile failure (deduplicated by project)
        val skippedProjects = (
          summary.skippedProjects.filter(_.reason.contains(cf.project)).map(_.project) ++
            summary.cancelledSuites.filter(_.reason.exists(_.contains(cf.project))).map(_.project)
        ).distinct
        if (skippedProjects.nonEmpty) {
          val shown = skippedProjects.take(5)
          val remaining = skippedProjects.size - shown.size
          lines += s"  ${C.YELLOW}-> Skipped ${skippedProjects.size} project(s):${C.RESET}"
          shown.foreach(p => lines += s"     ${C.YELLOW}o${C.RESET} $p")
          if (remaining > 0)
            lines += s"     ${C.YELLOW}...${C.RESET} and $remaining more"
        }
        lines += ""
      }
    }

    if (summary.linkFailures.nonEmpty) {
      lines += s"${C.RED}${C.BOLD}Link Failures (${summary.linkFailures.size})${C.RESET}"
      lines += ""
      summary.linkFailures.foreach { lf =>
        val platformStr = s" [${lf.platform.wireValue}]"
        lines += s"${C.RED}x ${lf.project.value}$platformStr${C.RESET}"
        lines += s"  ${C.RED}|${C.RESET} ${lf.error}"
        lines += ""
      }
    }

    // Cancelled suites (consequence of compile failures, user cancellation, etc.)
    mode match {
      case BuildMode.Test =>
        val cancelled = summary.cancelledSuites
        if (cancelled.nonEmpty) {
          lines += s"${C.RED}${C.BOLD}Cancelled Suites (${cancelled.size})${C.RESET}"
          cancelled.groupBy(_.project).toList.sortBy(_._1).foreach { case (project, suites) =>
            lines += s"${C.MAGENTA}$project${C.RESET}"
            suites.sortBy(_.suite).foreach { cs =>
              val reasonStr = cs.reason.map(r => s": $r").getOrElse("")
              lines += s"  - ${cs.suite.value}$reasonStr"
            }
          }
          lines += ""
        }

        // === Story: test results by category ===

        // Partition failures by category
        val allFailures = summary.failures.filter(f => !summary.skipped.exists(s => s.project == f.project && s.suite == f.suite && s.test == f.test))
        val testFailures = allFailures.filter(_.category == FailureCategory.TestFailed)
        val timeouts = allFailures.filter(_.category == FailureCategory.Timeout)
        val cancelledTests = allFailures.filter(_.category == FailureCategory.Cancelled)
        val processErrors = allFailures.filter(_.category == FailureCategory.ProcessError)
        val buildErrors = allFailures.filter(_.category == FailureCategory.BuildError)

        // Test failures (assertion failures, errors)
        if (testFailures.nonEmpty) {
          lines += s"${C.RED}${C.BOLD}Test Failures (${testFailures.size})${C.RESET}"
          lines += ""
          // Grouped by suite, with the suite's captured output printed once beneath its failures rather than repeated under each of them.
          //
          // The output belongs to the suite, not to any one test — a framework writes it as the suite runs, and bleep cannot attribute a given line to a
          // particular test. Attaching it to every failure meant a suite with two failures printed its entire output twice, and one with ten printed it ten
          // times, burying the failures it was supposed to explain.
          testFailures
            .sortBy(f => (f.project, f.suite, f.test))
            .groupBy(f => (f.project, f.suite))
            .toList
            .sortBy { case ((project, suite), _) => (project, suite) }
            .foreach { case ((project, suite), failures) =>
              // Tests that failed for the same reason are listed together and the reason printed once.
              //
              // One broken constructor becomes one failure per test method, each carrying the identical exception: JUnit 3 reports four tests all named
              // `warning`, each with the same fifty-frame trace, which is two hundred lines saying one thing. Grouping is on the exact (message, stack) pair,
              // so genuinely different failures are never merged.
              failures
                .groupBy(f => (f.message, f.throwable))
                .toList
                .sortBy { case (_, group) => group.map(_.test.value).min }
                .foreach { case ((message, throwable), group) =>
                  group.map(_.test.value).sorted.foreach(t => lines += s"${C.RED}x ${project.value} / ${suite.value} / $t${C.RESET}")
                  if (group.sizeIs > 1) lines += s"  ${C.YELLOW}|${C.RESET} ${C.BOLD}all ${group.size} failed with the same error:${C.RESET}"
                  // Through the same elision as the trace below it, because for some frameworks this *is* the trace. JUnit 3 reports a broken constructor as
                  // "Exception in constructor: testMeasures (java.lang.RuntimeException: ctor boom \n\tat …" — fifty-eight frames inside the message text,
                  // once per test method — and a message is not exempt from being unreadable.
                  message.filter(_.trim.nonEmpty).foreach { msg =>
                    renderStack(msg).foreach(line => lines += s"  ${C.YELLOW}|${C.RESET} ${C.sanitize(line)}")
                  }
                  // Only when it adds something. Several frameworks put the whole trace in the message already — JUnit 3's "Exception in constructor: …"
                  // carries it verbatim — and printing it again underneath doubles the longest thing on screen.
                  throwable.foreach { stack =>
                    val header = stack.linesIterator.nextOption().getOrElse("")
                    val alreadyShown = message.exists(m => header.nonEmpty && m.contains(header))
                    if (!alreadyShown) {
                      lines += s"  ${C.CYAN}Stack trace:${C.RESET}"
                      renderStack(stack).foreach(line => lines += s"  ${C.YELLOW}|${C.RESET} ${C.sanitize(line)}")
                    }
                  }
                  lines += ""
                }
              // Identical across a suite's failures by construction, so take it from any of them.
              val suiteOutput = failures.map(_.output).find(_.nonEmpty).getOrElse(Nil)
              if (suiteOutput.nonEmpty) {
                lines += s"  ${C.CYAN}Output from ${suite.value}:${C.RESET}"
                // Elided too, because this is where the same plumbing arrives a second time. Most frameworks print the failure to stdout as well as reporting
                // it, so a kotest constructor failure that has just been shown in three lines is followed by forty lines of the interceptor chain it was cut
                // from. The cut only ever removes a trailing run of frames it recognises, so ordinary output cannot be caught by it.
                renderStack(suiteOutput.mkString("\n")).foreach(line => lines += s"  ${C.YELLOW}|${C.RESET} ${C.sanitize(line)}")
                lines += ""
              }
            }
        }

        // Timeouts (suite or test exceeded time limit)
        if (timeouts.nonEmpty) {
          lines += s"${C.RED}${C.BOLD}Timeouts (${timeouts.size})${C.RESET}"
          lines += ""
          timeouts.sortBy(f => (f.project, f.suite)).foreach { failure =>
            lines += s"${C.RED}T ${failure.project.value} / ${failure.suite.value}${C.RESET}"
            failure.message.foreach { msg =>
              msg.split("\n").foreach(line => lines += s"  ${C.YELLOW}|${C.RESET} ${C.sanitize(line)}")
            }
            failure.throwable.foreach { stack =>
              lines += s"  ${C.CYAN}Stack trace:${C.RESET}"
              renderStack(stack).foreach(line => lines += s"  ${C.YELLOW}|${C.RESET} ${C.sanitize(line)}")
            }
            if (failure.output.nonEmpty) {
              lines += s"  ${C.CYAN}Output:${C.RESET}"
              // Elided: this is what the test process printed, not bleep's own stack, and it is where a framework that dies before reporting leaves its
              // trace. TestNG's broken constructor arrives here and nowhere else — twenty-seven `org.testng` frames under one line of cause.
              renderStack(failure.output.mkString("\n")).foreach(line => lines += s"  ${C.YELLOW}|${C.RESET} ${C.sanitize(line)}")
            }
            lines += ""
          }
        }

        // Cancelled tests (tests that were killed, e.g. remaining tests after a suite timeout)
        if (cancelledTests.nonEmpty) {
          lines += s"${C.YELLOW}${C.BOLD}Cancelled Tests (${cancelledTests.size})${C.RESET}"
          cancelledTests.groupBy(_.project).toList.sortBy(_._1).foreach { case (project, tests) =>
            lines += s"${C.MAGENTA}$project${C.RESET}"
            tests.sortBy(t => (t.suite, t.test)).foreach { t =>
              val reason = t.message.map(m => s": $m").getOrElse("")
              lines += s"  - ${t.suite.value} / ${t.test.value}$reason"
            }
          }
          lines += ""
        }

        // Process errors (crashes, non-zero exits)
        if (processErrors.nonEmpty) {
          lines += s"${C.RED}${C.BOLD}Process Errors (${processErrors.size})${C.RESET}"
          lines += ""
          processErrors.sortBy(f => (f.project, f.suite)).foreach { failure =>
            lines += s"${C.RED}! ${failure.project.value} / ${failure.suite.value}${C.RESET}"
            failure.message.foreach { msg =>
              msg.split("\n").foreach(line => lines += s"  ${C.YELLOW}|${C.RESET} ${C.sanitize(line)}")
            }
            failure.throwable.foreach { stack =>
              lines += s"  ${C.CYAN}Stack trace:${C.RESET}"
              StackTraceCycles.collapse(stack).foreach(line => lines += s"  ${C.YELLOW}|${C.RESET} ${C.sanitize(line)}")
            }
            if (failure.output.nonEmpty) {
              lines += s"  ${C.CYAN}Output:${C.RESET}"
              // Elided: this is what the test process printed, not bleep's own stack, and it is where a framework that dies before reporting leaves its
              // trace. TestNG's broken constructor arrives here and nowhere else — twenty-seven `org.testng` frames under one line of cause.
              renderStack(failure.output.mkString("\n")).foreach(line => lines += s"  ${C.YELLOW}|${C.RESET} ${C.sanitize(line)}")
            }
            lines += ""
          }
        }

        // Build errors
        if (buildErrors.nonEmpty) {
          lines += s"${C.RED}${C.BOLD}Build Errors (${buildErrors.size})${C.RESET}"
          lines += ""
          buildErrors.sortBy(f => (f.project, f.suite)).foreach { failure =>
            lines += s"${C.RED}! ${failure.project.value}${C.RESET}"
            failure.message.foreach { msg =>
              msg.split("\n").foreach(line => lines += s"  ${C.YELLOW}|${C.RESET} ${C.sanitize(line)}")
            }
            failure.throwable.foreach { stack =>
              lines += s"  ${C.CYAN}Stack trace:${C.RESET}"
              StackTraceCycles.collapse(stack).foreach(line => lines += s"  ${C.YELLOW}|${C.RESET} ${C.sanitize(line)}")
            }
            lines += ""
          }
        }

        // If anything died of memory — an in-JVM OutOfMemoryError, or a process the OS killed under
        // pressure — point at the one page that explains which knobs to turn. A user staring at "Java
        // heap space" or "killed by SIGKILL" needs to know that per-fork -Xmx, parallelism and the
        // machine budget are all adjustable, and how they trade off.
        val memoryRelated =
          (testFailures ++ processErrors ++ timeouts).exists { f =>
            val text = (f.message.getOrElse("") + " " + f.output.mkString(" ")).toLowerCase
            text.contains("outofmemory") || text.contains("heap space") || text.contains("sigkill") || text.contains("exit 137") ||
            text.contains("terminated before sending ready")
          }
        if (memoryRelated) {
          lines += s"${C.YELLOW}${C.BOLD}A process ran out of memory or was killed under memory pressure.${C.RESET}"
          lines += "  Adjust per-fork heap, how many run at once, or the machine budget — and how they trade off:"
          lines += s"  ${C.CYAN}https://bleep.build/docs/usage/resource-management${C.RESET}"
          lines += ""
        }

        // Fallback: if counters show problems but no categorized failures captured
        val categorizedCount = testFailures.size + timeouts.size + cancelledTests.size + processErrors.size + buildErrors.size
        if (categorizedCount == 0 && totalProblems > 0) {
          lines += s"${C.RED}${C.BOLD}Problems ($totalProblems)${C.RESET}"
          lines += s"  $totalProblems test(s) had issues but detailed info was not captured."
          lines += s"  Check BSP server logs for details."
          lines += ""
        }

        // Skipped tests (runtime assume() guards - test decided it can't run in this environment)
        val skippedTests = summary.skipped.filter(s => s.status == TestStatus.Skipped || s.status == TestStatus.AssumptionFailed)
        if (skippedTests.nonEmpty) {
          lines += s"${C.YELLOW}${C.BOLD}Skipped (${skippedTests.size})${C.RESET}"
          skippedTests.groupBy(_.project).toList.sortBy(_._1).foreach { case (project, tests) =>
            lines += s"${C.MAGENTA}$project${C.RESET}"
            tests.sortBy(t => (t.suite, t.test)).foreach { t =>
              val reasonStr = t.reason.map(r => s": $r").getOrElse("")
              lines += s"  - ${t.suite.value} / ${t.test.value}$reasonStr"
            }
          }
          lines += ""
        }

        // Ignored tests (marked @Ignore in source - deliberately excluded)
        val ignoredTests = summary.skipped.filter(_.status == TestStatus.Ignored)
        if (ignoredTests.nonEmpty) {
          lines += s"${C.YELLOW}${C.BOLD}Ignored (${ignoredTests.size})${C.RESET}"
          ignoredTests.groupBy(_.project).toList.sortBy(_._1).foreach { case (project, tests) =>
            lines += s"${C.MAGENTA}$project${C.RESET}"
            tests.sortBy(t => (t.suite, t.test)).foreach(t => lines += s"  - ${t.suite.value} / ${t.test.value}")
          }
          lines += ""
        }
      case _ =>
        ()
    }

    lines.result()
  }

  val empty: BuildSummary = BuildSummary(
    sourcegenFailed = 0,
    apResolutionFailed = 0,
    kspResolutionFailed = 0,
    compilesCompleted = 0,
    compilesFailed = 0,
    compilesSkipped = 0,
    compilesCancelled = 0,
    upToDateProjects = Nil,
    suitesTotal = 0,
    suitesCompleted = 0,
    suitesFailed = 0,
    suitesCancelled = 0,
    testsTotal = 0,
    testsPassed = 0,
    testsFailed = 0,
    testsTimedOut = 0,
    testsCancelled = 0,
    testsSkipped = 0,
    testsIgnored = 0,
    currentlyRunning = Nil,
    killedTasks = Nil,
    failures = Nil,
    skipped = Nil,
    cancelledSuites = Nil,
    compileFailures = Nil,
    linkFailures = Nil,
    skippedProjects = Nil,
    testProjectsWithoutSuites = Nil,
    durationMs = 0L,
    totalTaskTimeMs = 0L,
    wasCancelled = false,
    serverCrashed = false,
    filterContext = None,
    historyId = None
  )
}

/** Category of failure — each gets its own section in the summary */
sealed trait FailureCategory
object FailureCategory {
  case object TestFailed extends FailureCategory // assertion failure, error, pending
  case object Timeout extends FailureCategory // idle timeout (no test completed within timeout period)
  case object Cancelled extends FailureCategory // test cancelled (e.g. suite killed after timeout)
  case object ProcessError extends FailureCategory // process crash, non-zero exit
  case object BuildError extends FailureCategory // general build-level error
}

case class LinkFailure(
    project: CrossProjectName,
    platform: LinkPlatformName,
    error: String
)

case class TestFailure(
    project: CrossProjectName,
    suite: SuiteName,
    test: TestName,
    message: Option[String],
    throwable: Option[String],
    output: List[String],
    category: FailureCategory,
    // Where in the suite it was raised, when the forked JVM runner could recover it. Absent for the JS/Native/Kotlin
    // runners, for timeouts and cancellations (no throwable), and for failures raised outside the suite class.
    location: Option[bleep.bsp.protocol.BleepBspProtocol.SourceLocation]
)

case class TestSkipped(
    project: CrossProjectName,
    suite: SuiteName,
    test: TestName,
    status: TestStatus, // Skipped, Ignored, AssumptionFailed, Cancelled, or Pending
    reason: Option[String]
)

case class ProjectCompileFailure(
    project: CrossProjectName,
    diagnostics: List[BleepBspProtocol.Diagnostic]
)

case class SkippedProject(
    project: CrossProjectName,
    reason: String
)

case class CancelledSuite(
    project: CrossProjectName,
    suite: SuiteName,
    reason: Option[String]
)

/** Kind of build task. */
sealed trait TaskKind {
  def label: String
}
object TaskKind {
  case object Compile extends TaskKind { val label = "compile" }
  case object Link extends TaskKind { val label = "link" }
  case object Test extends TaskKind { val label = "test" }
}

/** A task that was started but never finished (e.g. compile or test suite killed by user cancellation) */
case class KilledTask(
    kind: TaskKind,
    project: String, // project name, or "project:suite" for test suites
    elapsedMs: Long
)

object BuildDisplay {

  /** Create a new build display */
  def create(
      quietMode: Boolean,
      logger: ryddig.Logger,
      mode: BuildMode = BuildMode.Test
  ): IO[BuildDisplay] =
    for {
      state <- Ref.of[IO, BuildState](BuildState.empty)
      startTime <- IO.realTime.map(_.toMillis)
      upToDateProjects <- Ref.of[IO, Set[CrossProjectName]](Set.empty)
    } yield new BuildDisplayImpl(state, startTime, quietMode, logger, mode, upToDateProjects)

  private class BuildDisplayImpl(
      state: Ref[IO, BuildState],
      startTime: Long,
      quietMode: Boolean,
      logger: ryddig.Logger,
      mode: BuildMode,
      upToDateProjects: Ref[IO, Set[CrossProjectName]]
  ) extends BuildDisplay {

    override def markServerCrashed: IO[Unit] = state.update(_.copy(serverCrashed = true))

    override def reset: IO[Unit] = state.set(BuildState.empty) >> IO {
      activePhase.clear()
    }

    // Track compile phase start times per project: project -> (phase, detail, startTimestamp)
    // Used to print completed phase with duration when next phase arrives or compile finishes
    private val activePhase: mutable.Map[CrossProjectName, (bleep.bsp.protocol.CompilePhase, String, Long)] = mutable.Map.empty

    override def handle(event: BuildEvent): IO[Unit] =
      state.update(s => BuildStateReducer.reduce(s, event)) >> printSideEffects(event)

    private def log(msg: String): IO[Unit] = IO.delay(logger.info(msg))
    private def logWarn(msg: String): IO[Unit] = IO.delay(logger.warn(msg))
    private def logError(msg: String): IO[Unit] = IO.delay(logger.error(msg))

    private def logP(project: CrossProjectName, msg: String): IO[Unit] = IO.delay(logger.withContext("project", project.value).info(msg))
    private def logWarnP(project: CrossProjectName, msg: String): IO[Unit] = IO.delay(logger.withContext("project", project.value).warn(msg))
    private def logErrorP(project: CrossProjectName, msg: String): IO[Unit] = IO.delay(logger.withContext("project", project.value).error(msg))

    /** Convert phase to past tense for completed-phase logging */
    private def phasePastTense(phase: bleep.bsp.protocol.CompilePhase): String = {
      import bleep.bsp.protocol.CompilePhase
      phase match {
        case CompilePhase.ReadingAnalysis => "read analysis"
        case CompilePhase.Analyzing       => "analyzed"
        case CompilePhase.Compiling       => "compiled"
        case CompilePhase.SavingAnalysis  => "saved analysis"
      }
    }

    /** Log the previously active phase as completed with duration, then record the new phase. */
    private def completePhase(project: CrossProjectName, now: Long): IO[Unit] =
      IO.delay(activePhase.remove(project)).flatMap {
        case Some((prevPhase, detail, startTs)) =>
          val dur = now - startTs
          logP(project, s"📦 ${phasePastTense(prevPhase)}$detail (${dur}ms)")
        case None => IO.unit
      }

    private def printSideEffects(event: BuildEvent): IO[Unit] = event match {
      case BuildEvent.CompileStarted(_, _) =>
        // Wait for CompilationReason to print — that's the meaningful start. We used to also push a redrawn "Compiling X: started, Y: 14%" progress line via
        // the ryddig progressMonitor, but it emitted ANSI clear-line escapes (`\x1b[K`) on every refresh which looked like garbage in CI logs and didn't add
        // anything useful over the per-event log lines. Killed entirely.
        IO.unit

      case BuildEvent.CompilationReason(project, reason, totalFiles, invalidatedFiles, changedDeps, _) =>
        // Track up-to-date projects so CompileFinished can suppress output for them
        val trackUpToDate = if (reason == CompileReason.UpToDate) {
          upToDateProjects.update(_ + project)
        } else IO.unit
        val printMsg = if (!quietMode) {
          val msg = reason match {
            case CompileReason.CleanBuild  => "🔨 compiling (clean build, no previous analysis)"
            case CompileReason.EmptyOutput => "🔨 compiling (clean build, output directory empty)"
            case CompileReason.UpToDate    => "✅ up to date"
            case CompileReason.Incremental =>
              val invalidatedCount = invalidatedFiles.size
              val depCount = changedDeps.size
              val invalidatedStr =
                if (invalidatedFiles.isEmpty) ""
                else {
                  val fileNames = invalidatedFiles.take(5)
                  val suffix = if (invalidatedFiles.size > 5) s", ... (${invalidatedFiles.size - 5} more)" else ""
                  s"$invalidatedCount/$totalFiles files invalidated: ${fileNames.mkString(", ")}$suffix"
                }
              val depStr =
                if (changedDeps.isEmpty) ""
                else {
                  val depNames = changedDeps.take(3)
                  val suffix = if (changedDeps.size > 3) s", ... (${changedDeps.size - 3} more)" else ""
                  s"$depCount changed deps: ${depNames.mkString(", ")}$suffix"
                }
              val parts = List(invalidatedStr, depStr).filter(_.nonEmpty)
              if (parts.isEmpty) s"🔨 compiling ($totalFiles source files)"
              else s"🔨 compiling (${parts.mkString("; ")})"
          }
          logP(project, msg)
        } else IO.unit
        trackUpToDate >> printMsg

      case BuildEvent.CompileFinished(project, status, durationMs, timestamp, _, _) =>
        import bleep.bsp.protocol.CompileStatus
        // Complete the last tracked phase (e.g. saving-analysis) and clean up
        val finishPhase = completePhase(project, timestamp)
        finishPhase >>
          // Suppress output for up-to-date projects (only show single line from CompilationReason)
          upToDateProjects.get.flatMap { upToDate =>
            if (upToDate.contains(project) && status == CompileStatus.Success) {
              IO.unit // Already showed "project: up to date" - nothing more needed
            } else if (!quietMode) {
              val (emoji, displayStatus) = status match {
                case CompileStatus.Success   => ("✅", "compiled")
                case CompileStatus.Failed    => ("❌", "compile failed")
                case CompileStatus.Error     => ("❌", "compile error")
                case CompileStatus.Skipped   => ("⏭️", "skipped")
                case CompileStatus.Cancelled => ("🚫", "cancelled")
              }
              logP(project, s"$emoji $displayStatus (${durationMs}ms)")
            } else IO.unit
          }

      case BuildEvent.SuiteStarted(_, _, _) =>
        if (!quietMode) printStatus else IO.unit

      case BuildEvent.TestStarted(_, _, _, _) =>
        IO.unit

      case BuildEvent.TestFinished(_, suite, test, status, durationMs, _, _, _, _) =>
        if (!quietMode) printTestResult(suite, test, status) else IO.unit

      case BuildEvent.SuiteFinished(_, suite, outcome, durationMs, _) =>
        if (!quietMode) printSuiteResult(suite, outcome, durationMs) else IO.unit

      case BuildEvent.Output(_, _, line, _, _) =>
        // Suppress test framework stdout — structural events (TestFinished, SuiteFinished)
        // already provide the info. Forwarding stdout causes duplicate lines because
        // ScalaTest/JUnit print their own started/finished lines to stdout.
        IO.unit

      case BuildEvent.SuitesDiscovered(project, suites, totalDiscovered, _, _, _) =>
        if (!quietMode) {
          if (suites.isEmpty)
            logP(project, "🔍 discovered 0 test suites")
          else
            logP(project, s"🔍 discovered ${suites.size} test suites (total: $totalDiscovered)")
        } else IO.unit

      case BuildEvent.ProjectSkipped(project, reason, _) =>
        if (!quietMode) logWarnP(project, s"⏭️ skipped ($reason)") else IO.unit

      case BuildEvent.CompileStalled(project, usedMb, maxMb, retryAtMs, _) =>
        val waitSec = math.max(0, (retryAtMs - System.currentTimeMillis()) / 1000)
        if (waitSec > 0) logWarnP(project, s"⏳ waiting to ensure sufficient memory (heap: ${usedMb}MB/${maxMb}MB) — retrying in ${waitSec}s")
        else IO.unit

      case _: BuildEvent.CompileResumed =>
        IO.unit // silence — compile will proceed and emit its own events

      case BuildEvent.LockContention(project, _, _) =>
        logWarnP(project, "⏳ waiting for compile lock")

      case BuildEvent.LockAcquired(project, waitedMs, _) =>
        logP(project, s"🔓 lock acquired (${waitedMs}ms)")

      case BuildEvent.CompilePhaseChanged(project, phase, trackedApis, timestamp) =>
        if (!quietMode) {
          val detail = if (trackedApis > 0) s", $trackedApis APIs" else ""
          // Log the previous phase as completed, then record the new one
          completePhase(project, timestamp) >>
            IO.delay { activePhase(project) = (phase, detail, timestamp) }
        } else IO.unit

      case BuildEvent.CompileProgress(_, _, _) =>
        // Compile-percent updates used to drive the redrawn in-place "Compiling X: 14%, Y: 35%" line; the redraw emitted ANSI clear-line escapes that looked
        // like garbage in CI logs and were of no use over the existing per-event lines. Killed entirely; we ignore the event.
        IO.unit

      case BuildEvent.SuiteTimedOut(_, suite, timeoutMs, _, _) =>
        // The dump is rendered once at the end under "Timeouts" in the summary (BuildState
        // hangs the jstack output off failure.throwable). Skip inline here to avoid printing
        // the same multi-hundred-line dump twice during the run.
        val timeoutSec = timeoutMs / 1000
        IO.delay(logger.withContext("suite", suite.value).error(s"⏰ timed out after ${timeoutSec}s"))

      case BuildEvent.SuiteError(_, suite, error, processExit, _, _) =>
        val desc = processExit match {
          case ProcessExit.Signal(sig)    => s"crashed (signal $sig)"
          case ProcessExit.ExitCode(code) => s"exited with code $code"
          case ProcessExit.Unknown        => error
        }
        IO.delay(logger.withContext("suite", suite.value).error(s"❌ $desc"))

      case BuildEvent.Error(message, details, _) =>
        for {
          _ <- logError(s"❌ $message")
          _ <- details match {
            case Some(d) => d.split("\n").toList.traverse_(line => log(s"  $line"))
            case None    => IO.unit
          }
        } yield ()

      case BuildEvent.SuiteCancelled(_, suite, reason, _) =>
        val reasonStr = reason.getOrElse("unknown reason (likely exceeded timeout)")
        IO.delay(logger.withContext("suite", suite.value).warn(s"🚫 cancelled ($reasonStr)"))

      case BuildEvent.LinkStarted(project, platform, _) =>
        if (!quietMode) logP(project, s"🔗 linking [${platform.wireValue}]") else IO.unit

      case BuildEvent.LinkSucceeded(project, platform, durationMs, _, _) =>
        if (!quietMode) logP(project, s"✅ linked [${platform.wireValue}] (${durationMs}ms)") else IO.unit

      case BuildEvent.LinkFailed(project, platform, durationMs, error, _) =>
        logErrorP(project, s"❌ link failed [${platform.wireValue}] (${durationMs}ms): $error")

      case BuildEvent.SourcegenStarted(scriptMain, forProjects, _) =>
        if (!quietMode) IO.delay(logger.withContext("script", scriptMain).info(s"⚙️ sourcegen for ${forProjects.map(_.value).mkString(", ")}")) else IO.unit

      case BuildEvent.SourcegenFinished(scriptMain, success, durationMs, error, _) =>
        if (success) {
          if (!quietMode) IO.delay(logger.withContext("script", scriptMain).info(s"✅ sourcegen done (${durationMs}ms)")) else IO.unit
        } else {
          IO.delay(logger.withContext("script", scriptMain).error(s"❌ sourcegen failed (${durationMs}ms): ${error.getOrElse("unknown error")}"))
        }

      case BuildEvent.ResolveAnnotationProcessorsFinished(project, success, durationMs, error, _) =>
        if (success) IO.unit
        else
          IO.delay(
            logger
              .withContext("project", project.value)
              .error(s"❌ annotation processor resolution failed (${durationMs}ms): ${error.getOrElse("unknown error")}")
          )

      case BuildEvent.RunSymbolProcessorsFinished(project, success, durationMs, error, _) =>
        if (success) IO.unit
        else
          IO.delay(
            logger
              .withContext("project", project.value)
              .error(s"❌ KSP run failed (${durationMs}ms): ${error.getOrElse("unknown error")}")
          )

      case _: BuildEvent.ConnectionLost =>
        logWarn("💀 connection lost — server may have been killed")

      case BuildEvent.WorkspaceBusy(operation, projects, startedAgoMs, _) =>
        val elapsed = startedAgoMs / 1000
        logWarn(s"⏳ workspace busy ($operation on ${projects.map(_.value).mkString(", ")}, started ${elapsed}s ago)")

      case _: BuildEvent.WorkspaceReady =>
        log("✅ workspace available, proceeding")

      case _: BuildEvent.TestRunCompleted =>
        IO.unit // State updated via BuildStateReducer; no side effects needed

      case _: BuildEvent.HistoryRecorded =>
        IO.unit // Surfaces in the summary via BuildStateReducer; nothing to print mid-run
    }

    private def printStatus: IO[Unit] =
      for {
        s <- state.get
        running = s.runningSuites.toList.sorted.take(3).mkString(", ")
        more = if (s.runningSuites.size > 3) s" (+${s.runningSuites.size - 3} more)" else ""
        _ <- log(s"Running: $running$more")
      } yield ()

    /** One line per finished test, qualified by its suite when the test's own name does not already say which suite it belongs to.
      *
      * Suites run concurrently, so these lines interleave. Unqualified, two suites containing a test of the same name produced two identical lines — a run with
      * a working suite and a broken copy of it printed `x throwsOnPurpose()` twice with nothing to tell them apart. Frameworks differ on whether the name they
      * report is already qualified (munit reports `example.MunitFixture.adds`, JUnit reports `adds()`), so the suite is prepended only when it is missing
      * rather than unconditionally, which would double it up for half the frameworks.
      */
    private def printTestResult(
        suite: SuiteName,
        test: TestName,
        status: TestStatus
    ): IO[Unit] = {
      val icon = status match {
        case TestStatus.Passed           => SConsole.GREEN + "+" + SConsole.RESET
        case TestStatus.Failed           => SConsole.RED + "x" + SConsole.RESET
        case TestStatus.Error            => SConsole.RED + "!" + SConsole.RESET
        case TestStatus.Timeout          => SConsole.RED + "T" + SConsole.RESET
        case TestStatus.Skipped          => SConsole.YELLOW + "-" + SConsole.RESET
        case TestStatus.Ignored          => SConsole.YELLOW + "o" + SConsole.RESET
        case TestStatus.Cancelled        => SConsole.YELLOW + "c" + SConsole.RESET
        case TestStatus.AssumptionFailed => SConsole.YELLOW + "a" + SConsole.RESET
        case TestStatus.Pending          => SConsole.YELLOW + "?" + SConsole.RESET
      }
      val simpleSuite = suite.value.split('.').lastOption.getOrElse(suite.value)
      val label = if (test.value.contains(simpleSuite)) test.value else s"$simpleSuite.${test.value}"
      log(s"  $icon $label")
    }

    private def printSuiteResult(
        suite: SuiteName,
        outcome: SuiteOutcome,
        durationMs: Long
    ): IO[Unit] = {
      // The outcome variant carries the distinction that used to be inferred from count arithmetic:
      // a discovered-but-empty suite, a framework mismatch, and a hard error are each their own red
      // line, never a green "0 passed".
      def red(s: String) = SConsole.RED + s + SConsole.RESET
      val line = outcome match {
        case SuiteOutcome.Executed(passed, failed, skipped, ignored) =>
          val ignoredStr = if (ignored > 0) s", $ignored ignored" else ""
          val status = if (failed > 0) red("FAILED") else SConsole.GREEN + "PASSED" + SConsole.RESET
          s"$status ${suite.value}: $passed passed, $failed failed, $skipped skipped$ignoredStr ($durationMs ms)"
        case SuiteOutcome.Empty =>
          s"${red("NO TESTS")} ${suite.value}: discovered but executed 0 tests ($durationMs ms)"
        case SuiteOutcome.NoFrameworkMatched =>
          s"${red("NO FRAMEWORK")} ${suite.value}: no test framework/engine claimed this suite ($durationMs ms)"
        case SuiteOutcome.Errored(message, _) =>
          s"${red("ERRORED")} ${suite.value}: $message ($durationMs ms)"
      }
      log(line)
    }

    override def summary: IO[BuildSummary] =
      for {
        s <- state.get
        now <- IO.realTime.map(_.toMillis)
      } yield s.toSummary(durationMs = now - startTime, wasCancelled = false)

    override def printSummary(filterContext: Option[FilterContext], failureDetails: Boolean): IO[Unit] = mode match {
      case BuildMode.Compile | BuildMode.Link(_) =>
        printCompileSummary(failureDetails)
      case BuildMode.Test =>
        printBuildSummary(filterContext, failureDetails)
      case BuildMode.Run(_, _) =>
        IO.unit // Run mode doesn't need a summary
    }

    private def printCompileSummary(failureDetails: Boolean): IO[Unit] =
      for {
        s <- summary
        // A compile run also fails when what runs BEFORE the compiles fails — sourcegen, or
        // annotation-processor / KSP resolution. Those make the dependent compiles `skipped`, not
        // `failed`, so a summary counting only compile failures reported "0 failed, N skipped" for a
        // run that did not succeed. Fold them in, so the header and counts tell the same story the
        // exit code does. Reported for a failed sourcegen showing as `0 failed, 4 skipped`.
        preCompileFailed = s.sourcegenFailed + s.apResolutionFailed + s.kspResolutionFailed
        anyFailure = s.compileFailures.nonEmpty || s.linkFailures.nonEmpty || preCompileFailed > 0 || s.serverCrashed
        _ <- log("")
        _ <- log("=" * 60)
        _ <- log(if (anyFailure) "Build Summary — FAILED" else "Build Summary")
        _ <- log("=" * 60)
        failedCount = s.compileFailures.size
        skippedCount = s.skippedProjects.size
        _ <- log(s"Projects: ${s.compilesCompleted} compiled, $failedCount failed, $skippedCount skipped")
        _ <-
          if (s.serverCrashed)
            log("Server:   crashed mid-run — the counts above are what finished before it died, not the whole build")
          else IO.unit
        _ <-
          if (s.sourcegenFailed > 0) log(s"Sourcegen: ${s.sourcegenFailed} script(s) failed — see the errors above and the BSP server log")
          else IO.unit
        _ <- if (s.apResolutionFailed > 0) log(s"Annotation processors: ${s.apResolutionFailed} project(s) failed to resolve") else IO.unit
        _ <- if (s.kspResolutionFailed > 0) log(s"KSP: ${s.kspResolutionFailed} project(s) failed to resolve") else IO.unit
        wallTimeSeconds = s.durationMs / 1000.0
        _ <- log(f"Time:     ${wallTimeSeconds}%.1fs")
        _ <- s.historyId.fold(IO.unit)(id => log(s"History:  #$id (bleep history show $id)"))
        _ <- if (s.compileFailures.nonEmpty && failureDetails) printCompileFailures(s.compileFailures) else IO.unit
        _ <- log("=" * 60)
      } yield ()

    private def printCompileFailures(failures: List[ProjectCompileFailure]): IO[Unit] =
      for {
        _ <- log("")
        _ <- log(SConsole.RED + "Compilation Failures:" + SConsole.RESET)
        _ <- failures.traverse_ { f =>
          val errors = f.diagnostics.filter(_.severity == DiagnosticSeverity.Error)
          val warnings = f.diagnostics.filter(_.severity == DiagnosticSeverity.Warning)
          val errorCount = errors.size
          val warningCount = warnings.size
          val countSuffix = {
            val parts = List(
              if (errorCount > 0) Some(s"$errorCount error${if (errorCount != 1) "s" else ""}") else None,
              if (warningCount > 0) Some(s"$warningCount warning${if (warningCount != 1) "s" else ""}") else None
            ).flatten
            if (parts.nonEmpty) s" (${parts.mkString(", ")})" else ""
          }
          for {
            _ <- log(s"  ${SConsole.RED}x${SConsole.RESET} ${f.project.value}$countSuffix")
            // Show errors first (use rendered when available for source line + caret)
            _ <- errors.take(10).traverse_ { e =>
              val text = e.rendered.getOrElse(e.message)
              text.linesIterator.toList.traverse_(line => log(s"    ${SConsole.RED}|${SConsole.RESET} $line"))
            }
            _ <- if (errors.size > 10) log(s"    ${SConsole.RED}|${SConsole.RESET} ... and ${errors.size - 10} more errors") else IO.unit
            // Then warnings
            _ <- warnings.take(5).traverse_ { w =>
              val text = w.rendered.getOrElse(w.message)
              text.linesIterator.toList.traverse_(line => log(s"    ${SConsole.YELLOW}|${SConsole.RESET} $line"))
            }
            _ <- if (warnings.size > 5) log(s"    ${SConsole.YELLOW}|${SConsole.RESET} ... and ${warnings.size - 5} more warnings") else IO.unit
          } yield ()
        }
      } yield ()

    private def printBuildSummary(filterContext: Option[FilterContext], failureDetails: Boolean): IO[Unit] =
      for {
        s <- summary
        enriched = s.copy(filterContext = filterContext)
        _ <- BuildSummary.formatSummary(enriched, mode, failureDetails).traverse_(log)
      } yield ()
  }

  /** Create a fancy TUI display. Returns:
    *   - display: The BuildDisplay to send events to
    *   - signalCompletionAndWait: IO that signals completion and waits for summary (use when build finishes)
    *   - waitForUserQuit: IO that waits for user to quit (without signaling) - returns summary when user presses 'q'
    *   - cancelBlockingSignal: IO that completes when user presses 'c' to cancel blocking work
    */
  def createFancy(
      mode: BuildMode = BuildMode.Test,
      diagnosticLog: Option[java.io.PrintWriter] = None,
      readySignal: Option[Deferred[IO, Unit]] = None
  ): IO[(BuildDisplay, IO[BuildSummary], IO[BuildSummary], Deferred[IO, Unit])] =
    createFancyWithState(None, mode, diagnosticLog, readySignal)

  /** Create a fancy TUI display with access to TestRunState for richer display. When testRunState is provided, the running section shows projects with compile
    * progress, test progress, JVM assignments, and failure counts.
    */
  def createFancyWithState(
      testRunState: Option[Ref[IO, TestRunState]],
      mode: BuildMode = BuildMode.Test,
      diagnosticLog: Option[java.io.PrintWriter] = None,
      readySignal: Option[Deferred[IO, Unit]] = None
  ): IO[(BuildDisplay, IO[BuildSummary], IO[BuildSummary], Deferred[IO, Unit])] =
    for {
      eventQueue <- cats.effect.std.Queue.unbounded[IO, Option[BuildEvent]]
      state <- Ref.of[IO, BuildState](BuildState.empty)
      startTime <- IO.realTime.map(_.toMillis)
      userQuitSignal <- Deferred[IO, Unit]
      cancelBlockingSignal <- Deferred[IO, Unit]
      // Start the fancy display in a background fiber
      fancyFiber <- FancyBuildDisplay
        .run(
          eventQueue,
          testRunState,
          mode,
          diagnosticLog,
          userQuitSignal = Some(userQuitSignal),
          readySignal = readySignal,
          cancelBlockingSignal = Some(cancelBlockingSignal)
        )
        .start
    } yield {
      val display = new FancyBridgeDisplay(eventQueue, state, startTime)
      // Signal completion to the fancy display, then wait for summary
      val signalCompletionAndWait = for {
        _ <- eventQueue.offer(None)
        summary <- fancyFiber.joinWithNever
      } yield summary
      // Only resolves when user explicitly presses q/Esc/Ctrl+C, NOT on auto-exit or poison pill
      val waitForUserQuit = userQuitSignal.get >> fancyFiber.joinWithNever
      (display, signalCompletionAndWait, waitForUserQuit, cancelBlockingSignal)
    }

  /** Create a diff-watch display that shows terse per-project diffs against a previous run */
  def createDiffWatch(
      logger: ryddig.Logger,
      mode: BuildMode,
      previousRun: PreviousRunState
  ): IO[BuildDisplay] =
    for {
      state <- Ref.of[IO, BuildState](BuildState.empty)
      startTime <- IO.realTime.map(_.toMillis)
      currentTestResults <- Ref.of[IO, List[BuildEvent.TestFinished]](Nil)
    } yield new DiffWatchBuildDisplayImpl(state, startTime, logger, mode, previousRun, currentTestResults)

  private class DiffWatchBuildDisplayImpl(
      state: Ref[IO, BuildState],
      startTime: Long,
      logger: ryddig.Logger,
      mode: BuildMode,
      previousRun: PreviousRunState,
      currentTestResults: Ref[IO, List[BuildEvent.TestFinished]]
  ) extends BuildDisplay {

    override def markServerCrashed: IO[Unit] = state.update(_.copy(serverCrashed = true))

    override def reset: IO[Unit] = state.set(BuildState.empty) >> currentTestResults.set(Nil)

    private def log(msg: String): IO[Unit] = IO.delay(logger.info(msg))
    private def logWarn(msg: String): IO[Unit] = IO.delay(logger.warn(msg))
    private def logError(msg: String): IO[Unit] = IO.delay(logger.error(msg))

    override def handle(event: BuildEvent): IO[Unit] =
      state.update(s => BuildStateReducer.reduce(s, event)) >>
        trackTestResults(event) >>
        printDiffSideEffects(event)

    private def trackTestResults(event: BuildEvent): IO[Unit] = event match {
      case tf: BuildEvent.TestFinished =>
        currentTestResults.update(tf :: _)
      case _ => IO.unit
    }

    private def printDiffSideEffects(event: BuildEvent): IO[Unit] = event match {
      case cf: BuildEvent.CompileFinished =>
        val previousDiags = previousRun.compileDiagnostics.getOrElse(cf.project, Nil)
        val diff = BuildDiff.diffCompile(cf.project, cf.status, cf.diagnostics, previousDiags, cf.durationMs)
        val line = BuildDiff.formatCompileDiff(diff)
        // For failed compiles, also show the actual errors. `isFailure`, not `== Failed`: a compile
        // that threw arrives as Error carrying the exception as its single diagnostic, and printing
        // a bare ❌ with the cause withheld is precisely the "detail not captured" hole.
        if (cf.status.isFailure && cf.diagnostics.nonEmpty) {
          val errors = cf.diagnostics.filter(_.severity == DiagnosticSeverity.Error)
          val errorLines = errors.take(10).flatMap { d =>
            val text = d.rendered.getOrElse(d.message)
            text.linesIterator.map(l => s"  ${SConsole.RED}|${SConsole.RESET} $l").toList
          }
          val truncation = if (errors.size > 10) List(s"  ${SConsole.RED}|${SConsole.RESET} ... and ${errors.size - 10} more errors") else Nil
          log(line) >> (errorLines ++ truncation).traverse_(log)
        } else {
          log(line)
        }

      case sf: BuildEvent.SuiteFinished =>
        currentTestResults.get.flatMap { allResults =>
          val suiteTests = allResults.filter(t => t.project == sf.project && t.suite == sf.suite)
          val diff = BuildDiff.diffSuite(
            sf.project,
            sf.suite,
            suiteTests,
            previousRun.testResults,
            sf.outcome,
            sf.durationMs
          )
          log(BuildDiff.formatSuiteDiff(diff))
        }

      case BuildEvent.SuiteTimedOut(_, suite, timeoutMs, _, _) =>
        logError(s"[TIMEOUT] ${suite.value} after ${timeoutMs / 1000}s")

      case BuildEvent.SuiteError(_, suite, error, processExit, _, _) =>
        val desc = processExit match {
          case ProcessExit.Signal(sig)    => s"Process crashed (signal $sig)"
          case ProcessExit.ExitCode(code) => s"Process exited with code $code"
          case ProcessExit.Unknown        => error
        }
        logError(s"[ERROR] ${suite.value}: $desc")

      case BuildEvent.Error(message, _, _) =>
        logError(s"[ERROR] $message")

      case BuildEvent.CompileStalled(project, usedMb, maxMb, retryAtMs, _) =>
        val waitSec = math.max(0, (retryAtMs - System.currentTimeMillis()) / 1000)
        if (waitSec > 0) logWarn(s"${project.value}: compilation stalled (heap: ${usedMb}MB/${maxMb}MB) — retrying in ${waitSec}s")
        else IO.unit

      case _ => IO.unit
    }

    override def summary: IO[BuildSummary] =
      for {
        s <- state.get
        now <- IO.realTime.map(_.toMillis)
      } yield s.toSummary(durationMs = now - startTime, wasCancelled = false)

    override def printSummary(filterContext: Option[FilterContext], failureDetails: Boolean): IO[Unit] =
      // DiffWatch focuses on per-cycle deltas; FilterContext isn't surfaced here since the user already chose the filter and watches its outcome cycle by cycle.
      // We accept the parameters for trait conformance and ignore them — the summary here is already a single line, so there is nothing to suppress.
      for {
        s <- summary.map(_.copy(filterContext = filterContext))
        allTests <- currentTestResults.get
        _ <- mode match {
          case BuildMode.Compile | BuildMode.Link(_) =>
            import bleep.bsp.protocol.CompileStatus
            val totalNew: Int = s.compileFailures.map { cf =>
              val prev = previousRun.compileDiagnostics.getOrElse(cf.project, Nil)
              val diff = BuildDiff.diffCompile(cf.project, CompileStatus.Failed, cf.diagnostics, prev, 0)
              diff.newErrors
            }.sum
            val totalFixed: Int = previousRun.compileDiagnostics.keys.toList.map { project =>
              val prev = previousRun.compileDiagnostics.getOrElse(project, Nil)
              val current = s.compileFailures.find(_.project == project).map(_.diagnostics).getOrElse(Nil)
              val diff = BuildDiff.diffCompile(project, CompileStatus.Success, current, prev, 0)
              diff.fixedErrors
            }.sum
            val totalErrors = s.compileFailures.flatMap(_.diagnostics).count(_.severity == DiagnosticSeverity.Error)
            log(BuildDiff.formatCompileSummary(s.compilesCompleted, totalErrors, totalNew, totalFixed))

          case BuildMode.Test =>
            val newFailures = allTests.count { t =>
              val key = TestKey(t.project, t.suite, t.test)
              val prev = previousRun.testResults.get(key)
              val prevFailed = prev.exists(_.isFailure)
              t.status.isFailure && !prevFailed
            }
            val fixedTests = previousRun.testResults.count { case (key, prev) =>
              val prevFailed = prev.isFailure
              val currentResult = allTests.find(t => TestKey(t.project, t.suite, t.test) == key)
              prevFailed && currentResult.exists(t => !t.status.isFailure)
            }
            log(BuildDiff.formatTestSummary(s.testsPassed, s.testsFailed, newFailures, fixedTests))

          case BuildMode.Run(_, _) => IO.unit
        }
      } yield ()
  }

  private class FancyBridgeDisplay(
      eventQueue: cats.effect.std.Queue[IO, Option[BuildEvent]],
      state: Ref[IO, BuildState],
      startTime: Long
  ) extends BuildDisplay {

    override def markServerCrashed: IO[Unit] = state.update(_.copy(serverCrashed = true))

    override def reset: IO[Unit] = state.set(BuildState.empty)

    override def handle(event: BuildEvent): IO[Unit] =
      for {
        _ <- state.update(s => BuildStateReducer.reduce(s, event))
        _ <- eventQueue.offer(Some(event))
      } yield ()

    override def summary: IO[BuildSummary] =
      for {
        s <- state.get
        now <- IO.realTime.map(_.toMillis)
      } yield s.toSummary(durationMs = now - startTime, wasCancelled = false)

    override def printSummary(filterContext: Option[FilterContext], failureDetails: Boolean): IO[Unit] =
      IO.unit // Fancy display handles this; filterContext is rendered there.
  }
}
