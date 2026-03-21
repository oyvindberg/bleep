package bleep.testing

import bleep.bsp.protocol.{BleepBspProtocol, DiagnosticSeverity, TestStatus}
import bleep.bsp.protocol.BleepBspProtocol.BuildMode
import cats.effect._
import cats.syntax.all._
import ryddig.{LoggerFn, TypedLogger}

import scala.collection.mutable
import scala.{Console => SConsole}

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

  /** Print final summary */
  def printSummary: IO[Unit]
}

case class BuildSummary(
    sourcegenFailed: Int,
    compilesCompleted: Int,
    compilesFailed: Int,
    compilesSkipped: Int,
    compilesCancelled: Int,
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
    currentlyRunning: List[String],
    killedTasks: List[KilledTask], // tasks that were started but never finished (cancelled builds)
    failures: List[TestFailure],
    skipped: List[TestSkipped],
    cancelledSuites: List[CancelledSuite],
    compileFailures: List[ProjectCompileFailure],
    linkFailures: List[LinkFailure],
    skippedProjects: List[SkippedProject],
    durationMs: Long,
    totalTaskTimeMs: Long, // Sum of all individual task durations (compile + link + test, for parallelism stats)
    wasCancelled: Boolean
)

object BuildSummary {

  /** Format a complete summary for display after a build/test run. Returns lines to print. Used by both TUI and non-TUI paths.
    */
  def formatSummary(summary: BuildSummary, mode: BuildMode): List[String] = {
    import scala.{Console => C}
    val lines = List.newBuilder[String]

    // Anything other than passed/skipped/ignored means failure
    val totalProblems = summary.testsFailed + summary.testsTimedOut + summary.testsCancelled
    val hasFailures =
      summary.sourcegenFailed > 0 || summary.compileFailures.nonEmpty || summary.linkFailures.nonEmpty || totalProblems > 0 || summary.suitesCancelled > 0
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
              lines += s"  Still running: ${summary.currentlyRunning.mkString(", ")}"
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
    lines += ""

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
        lines += s"${C.RED}x ${cf.project}${C.RESET}$countSuffix"
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
            lines += s"  ${C.YELLOW}|${C.RESET} $line"
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
        val platformStr = if (lf.platform.nonEmpty) s" [${lf.platform}]" else ""
        lines += s"${C.RED}x ${lf.project}$platformStr${C.RESET}"
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
              lines += s"  - ${cs.suite}$reasonStr"
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
          testFailures.sortBy(f => (f.project, f.suite, f.test)).foreach { failure =>
            lines += s"${C.RED}x ${failure.project} / ${failure.suite} / ${failure.test}${C.RESET}"
            failure.message.foreach { msg =>
              msg.split("\n").foreach(line => lines += s"  ${C.YELLOW}|${C.RESET} $line")
            }
            failure.throwable.foreach { stack =>
              lines += s"  ${C.CYAN}Stack trace:${C.RESET}"
              stack.split("\n").take(20).foreach(line => lines += s"  ${C.YELLOW}|${C.RESET} $line")
              val stackLines = stack.split("\n").length
              if (stackLines > 20)
                lines += s"  ${C.YELLOW}|${C.RESET} ... and ${stackLines - 20} more lines"
            }
            if (failure.output.nonEmpty) {
              lines += s"  ${C.CYAN}Output:${C.RESET}"
              failure.output.take(30).foreach(line => lines += s"  ${C.YELLOW}|${C.RESET} $line")
              if (failure.output.size > 30)
                lines += s"  ${C.YELLOW}|${C.RESET} ... and ${failure.output.size - 30} more lines"
            }
            lines += ""
          }
        }

        // Timeouts (suite or test exceeded time limit)
        if (timeouts.nonEmpty) {
          lines += s"${C.RED}${C.BOLD}Timeouts (${timeouts.size})${C.RESET}"
          lines += ""
          timeouts.sortBy(f => (f.project, f.suite)).foreach { failure =>
            lines += s"${C.RED}T ${failure.project} / ${failure.suite}${C.RESET}"
            failure.message.foreach { msg =>
              msg.split("\n").foreach(line => lines += s"  ${C.YELLOW}|${C.RESET} $line")
            }
            failure.throwable.foreach { stack =>
              lines += s"  ${C.CYAN}Stack trace:${C.RESET}"
              stack.split("\n").take(20).foreach(line => lines += s"  ${C.YELLOW}|${C.RESET} $line")
              val stackLines = stack.split("\n").length
              if (stackLines > 20)
                lines += s"  ${C.YELLOW}|${C.RESET} ... and ${stackLines - 20} more lines"
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
              lines += s"  - ${t.suite} / ${t.test}$reason"
            }
          }
          lines += ""
        }

        // Process errors (crashes, non-zero exits)
        if (processErrors.nonEmpty) {
          lines += s"${C.RED}${C.BOLD}Process Errors (${processErrors.size})${C.RESET}"
          lines += ""
          processErrors.sortBy(f => (f.project, f.suite)).foreach { failure =>
            lines += s"${C.RED}! ${failure.project} / ${failure.suite}${C.RESET}"
            failure.message.foreach { msg =>
              msg.split("\n").foreach(line => lines += s"  ${C.YELLOW}|${C.RESET} $line")
            }
            failure.throwable.foreach { stack =>
              lines += s"  ${C.CYAN}Stack trace:${C.RESET}"
              stack.split("\n").take(20).foreach(line => lines += s"  ${C.YELLOW}|${C.RESET} $line")
              val stackLines = stack.split("\n").length
              if (stackLines > 20)
                lines += s"  ${C.YELLOW}|${C.RESET} ... and ${stackLines - 20} more lines"
            }
            if (failure.output.nonEmpty) {
              lines += s"  ${C.CYAN}Output:${C.RESET}"
              failure.output.take(30).foreach(line => lines += s"  ${C.YELLOW}|${C.RESET} $line")
              if (failure.output.size > 30)
                lines += s"  ${C.YELLOW}|${C.RESET} ... and ${failure.output.size - 30} more lines"
            }
            lines += ""
          }
        }

        // Build errors
        if (buildErrors.nonEmpty) {
          lines += s"${C.RED}${C.BOLD}Build Errors (${buildErrors.size})${C.RESET}"
          lines += ""
          buildErrors.sortBy(f => (f.project, f.suite)).foreach { failure =>
            lines += s"${C.RED}! ${failure.project}${C.RESET}"
            failure.message.foreach { msg =>
              msg.split("\n").foreach(line => lines += s"  ${C.YELLOW}|${C.RESET} $line")
            }
            failure.throwable.foreach { stack =>
              lines += s"  ${C.CYAN}Stack trace:${C.RESET}"
              stack.split("\n").take(20).foreach(line => lines += s"  ${C.YELLOW}|${C.RESET} $line")
              val stackLines = stack.split("\n").length
              if (stackLines > 20)
                lines += s"  ${C.YELLOW}|${C.RESET} ... and ${stackLines - 20} more lines"
            }
            lines += ""
          }
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
              lines += s"  - ${t.suite} / ${t.test}$reasonStr"
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
            tests.sortBy(t => (t.suite, t.test)).foreach(t => lines += s"  - ${t.suite} / ${t.test}")
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
    compilesCompleted = 0,
    compilesFailed = 0,
    compilesSkipped = 0,
    compilesCancelled = 0,
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
    durationMs = 0L,
    totalTaskTimeMs = 0L,
    wasCancelled = false
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
    project: String,
    platform: String,
    error: String
)

case class TestFailure(
    project: String,
    suite: String,
    test: String,
    message: Option[String],
    throwable: Option[String],
    output: List[String],
    category: FailureCategory
)

case class TestSkipped(
    project: String,
    suite: String,
    test: String,
    status: TestStatus, // Skipped, Ignored, AssumptionFailed, Cancelled, or Pending
    reason: Option[String] = None
)

case class ProjectCompileFailure(
    project: String,
    diagnostics: List[BleepBspProtocol.Diagnostic]
)

case class SkippedProject(
    project: String,
    reason: String
)

case class CancelledSuite(
    project: String,
    suite: String,
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
      upToDateProjects <- Ref.of[IO, Set[String]](Set.empty)
    } yield new BuildDisplayImpl(state, startTime, quietMode, logger, mode, upToDateProjects)

  private class BuildDisplayImpl(
      state: Ref[IO, BuildState],
      startTime: Long,
      quietMode: Boolean,
      logger: ryddig.Logger,
      mode: BuildMode,
      upToDateProjects: Ref[IO, Set[String]]
  ) extends BuildDisplay {

    override def reset: IO[Unit] = state.set(BuildState.empty) >> IO {
      activeCompileProgress.clear()
      lastProgressLine = ""
      activePhase.clear()
    }

    // Track active compilations and their progress for progressMonitor display
    private val activeCompileProgress: mutable.Map[String, Int] = mutable.Map.empty
    private var lastProgressLine: String = ""

    // Track compile phase start times per project: project -> (phase, detail, startTimestamp)
    // Used to print completed phase with duration when next phase arrives or compile finishes
    private val activePhase: mutable.Map[String, (bleep.bsp.protocol.CompilePhase, String, Long)] = mutable.Map.empty

    private val progressMonitor: Option[LoggerFn] = logger match {
      case tl: TypedLogger[?] => tl.progressMonitor
      case _                  => None
    }

    private def renderCompileProgress(): Unit = progressMonitor.foreach { pm =>
      val items = activeCompileProgress.toList.sortBy(-_._2)
      if (items.nonEmpty) {
        val rendered = items.take(4).map { case (project, pct) =>
          if (pct > 0) s"$project: $pct%" else s"$project: started"
        }
        val rest = items.size - rendered.size
        val suffix = if (rest > 0) s" +$rest" else ""
        val line = s"Compiling ${rendered.mkString(", ")}$suffix"
        if (line != lastProgressLine) {
          lastProgressLine = line
          pm.info(line)
        }
      }
    }

    override def handle(event: BuildEvent): IO[Unit] =
      state.update(s => BuildStateReducer.reduce(s, event)) >> printSideEffects(event)

    private def log(msg: String): IO[Unit] = IO.delay(logger.info(msg))
    private def logWarn(msg: String): IO[Unit] = IO.delay(logger.warn(msg))
    private def logError(msg: String): IO[Unit] = IO.delay(logger.error(msg))

    private def logP(project: String, msg: String): IO[Unit] = IO.delay(logger.withContext("project", project).info(msg))
    private def logWarnP(project: String, msg: String): IO[Unit] = IO.delay(logger.withContext("project", project).warn(msg))
    private def logErrorP(project: String, msg: String): IO[Unit] = IO.delay(logger.withContext("project", project).error(msg))

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
    private def completePhase(project: String, now: Long): IO[Unit] =
      IO.delay(activePhase.remove(project)).flatMap {
        case Some((prevPhase, detail, startTs)) =>
          val dur = now - startTs
          logP(project, s"📦 ${phasePastTense(prevPhase)}$detail (${dur}ms)")
        case None => IO.unit
      }

    private def printSideEffects(event: BuildEvent): IO[Unit] = event match {
      case BuildEvent.CompileStarted(project, _) =>
        // Track for progressMonitor, but don't print — wait for CompilationReason
        IO.delay {
          activeCompileProgress(project) = 0
          renderCompileProgress()
        }

      case BuildEvent.CompilationReason(project, reason, totalFiles, invalidatedFiles, changedDeps, _) =>
        // Track up-to-date projects so CompileFinished can suppress output for them
        val trackUpToDate = if (reason == "up-to-date") {
          upToDateProjects.update(_ + project)
        } else IO.unit
        val printMsg = if (!quietMode) {
          val msg = reason match {
            case "clean-build"  => "🔨 compiling (clean build, no previous analysis)"
            case "empty-output" => "🔨 compiling (clean build, output directory empty)"
            case "up-to-date"   => "✅ up to date"
            case "incremental" =>
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
            case other => s"🔨 compiling ($other)"
          }
          logP(project, msg)
        } else IO.unit
        trackUpToDate >> printMsg

      case BuildEvent.CompileFinished(project, status, durationMs, timestamp, _, _) =>
        import bleep.bsp.protocol.CompileStatus
        // Complete the last tracked phase (e.g. saving-analysis) and clean up
        val finishPhase = completePhase(project, timestamp)
        // Remove from active progress tracking
        val removeProgress = IO.delay {
          val _ = activeCompileProgress.remove(project)
          renderCompileProgress()
        }
        finishPhase >> removeProgress >>
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

      case BuildEvent.TestFinished(_, suite, test, status, durationMs, _, _, _) =>
        if (!quietMode) printTestResult(suite, test, status, durationMs) else IO.unit

      case BuildEvent.SuiteFinished(_, suite, passed, failed, skipped, ignored, durationMs, _) =>
        if (!quietMode) printSuiteResult(suite, passed, failed, skipped, ignored, durationMs) else IO.unit

      case BuildEvent.Output(_, _, line, _, _) =>
        // Suppress test framework stdout — structural events (TestFinished, SuiteFinished)
        // already provide the info. Forwarding stdout causes duplicate lines because
        // ScalaTest/JUnit print their own started/finished lines to stdout.
        IO.unit

      case BuildEvent.SuitesDiscovered(project, suites, totalDiscovered, _) =>
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

      case BuildEvent.CompileProgress(project, percent, _) =>
        IO.delay {
          activeCompileProgress(project) = percent
          renderCompileProgress()
        }

      case BuildEvent.SuiteTimedOut(_, suite, timeoutMs, threadDumpInfo, _) =>
        val timeoutSec = timeoutMs / 1000
        for {
          _ <- IO.delay(logger.withContext("suite", suite).error(s"⏰ timed out after ${timeoutSec}s"))
          _ <- threadDumpInfo.flatMap(_.singleThreadStack) match {
            case Some(stack) => log(s"  Stack trace:\n$stack")
            case None        => IO.unit
          }
          _ <- threadDumpInfo.flatMap(_.dumpFile) match {
            case Some(path) => log(s"  Full thread dump: $path")
            case None       => IO.unit
          }
        } yield ()

      case BuildEvent.SuiteError(_, suite, error, exitCode, signal, _, _) =>
        val desc = signal match {
          case Some(sig) => s"crashed (signal $sig)"
          case None =>
            exitCode match {
              case Some(code) => s"exited with code $code"
              case None       => error
            }
        }
        IO.delay(logger.withContext("suite", suite).error(s"❌ $desc"))

      case BuildEvent.Error(project, message, details, _) =>
        for {
          _ <-
            if (project.nonEmpty) logErrorP(project, s"❌ $message")
            else logError(s"❌ $message")
          _ <- details match {
            case Some(d) => d.split("\n").toList.traverse_(line => log(s"  $line"))
            case None    => IO.unit
          }
        } yield ()

      case BuildEvent.SuiteCancelled(_, suite, reason, _) =>
        val reasonStr = reason.getOrElse("unknown reason (likely exceeded timeout)")
        IO.delay(logger.withContext("suite", suite).warn(s"🚫 cancelled ($reasonStr)"))

      case BuildEvent.LinkStarted(project, platform, _) =>
        if (!quietMode) logP(project, s"🔗 linking [$platform]") else IO.unit

      case BuildEvent.LinkSucceeded(project, platform, durationMs, _) =>
        if (!quietMode) logP(project, s"✅ linked [$platform] (${durationMs}ms)") else IO.unit

      case BuildEvent.LinkFailed(project, platform, durationMs, error, _) =>
        logErrorP(project, s"❌ link failed [$platform] (${durationMs}ms): $error")

      case BuildEvent.SourcegenStarted(scriptMain, forProjects, _) =>
        if (!quietMode) IO.delay(logger.withContext("script", scriptMain).info(s"⚙️ sourcegen for ${forProjects.mkString(", ")}")) else IO.unit

      case BuildEvent.SourcegenFinished(scriptMain, success, durationMs, error, _) =>
        if (success) {
          if (!quietMode) IO.delay(logger.withContext("script", scriptMain).info(s"✅ sourcegen done (${durationMs}ms)")) else IO.unit
        } else {
          IO.delay(logger.withContext("script", scriptMain).error(s"❌ sourcegen failed (${durationMs}ms): ${error.getOrElse("unknown error")}"))
        }

      case _: BuildEvent.ConnectionLost =>
        logWarn("💀 connection lost — server may have been killed")

      case BuildEvent.WorkspaceBusy(_, operation, projects, startedAgoMs, _) =>
        val elapsed = startedAgoMs / 1000
        logWarn(s"⏳ workspace busy ($operation on ${projects.mkString(", ")}, started ${elapsed}s ago)")

      case _: BuildEvent.WorkspaceReady =>
        log("✅ workspace available, proceeding")

      case _: BuildEvent.TestRunCompleted =>
        IO.unit // State updated via BuildStateReducer; no side effects needed
    }

    private def printStatus: IO[Unit] =
      for {
        s <- state.get
        running = s.currentlyRunning.take(3).mkString(", ")
        more = if (s.currentlyRunning.size > 3) s" (+${s.currentlyRunning.size - 3} more)" else ""
        _ <- log(s"Running: $running$more")
      } yield ()

    private def printTestResult(
        @annotation.nowarn("msg=is never used") suite: String,
        test: String,
        status: TestStatus,
        durationMs: Long
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
      log(s"  $icon $test")
    }

    private def printSuiteResult(
        suite: String,
        passed: Int,
        failed: Int,
        skipped: Int,
        ignored: Int,
        durationMs: Long
    ): IO[Unit] = {
      val ignoredStr = if (ignored > 0) s", $ignored ignored" else ""
      val status =
        if (failed > 0) SConsole.RED + "FAILED" + SConsole.RESET
        else SConsole.GREEN + "PASSED" + SConsole.RESET
      log(s"$status $suite: $passed passed, $failed failed, $skipped skipped$ignoredStr ($durationMs ms)")
    }

    override def summary: IO[BuildSummary] =
      for {
        s <- state.get
        now <- IO.realTime.map(_.toMillis)
      } yield s.toSummary(durationMs = now - startTime, wasCancelled = false)

    override def printSummary: IO[Unit] = mode match {
      case BuildMode.Compile | BuildMode.Link(_) =>
        printCompileSummary
      case BuildMode.Test =>
        printBuildSummary
      case BuildMode.Run(_, _) =>
        IO.unit // Run mode doesn't need a summary
    }

    private def printCompileSummary: IO[Unit] =
      for {
        s <- summary
        _ <- log("")
        _ <- log("=" * 60)
        _ <- log("Build Summary")
        _ <- log("=" * 60)
        compiledCount = s.compileFailures.size + (if (s.compileFailures.isEmpty && s.durationMs > 0) 1 else 0)
        failedCount = s.compileFailures.size
        skippedCount = s.skippedProjects.size
        _ <- log(s"Projects: compiled, $failedCount failed, $skippedCount skipped")
        wallTimeSeconds = s.durationMs / 1000.0
        _ <- log(f"Time:     ${wallTimeSeconds}%.1fs")
        _ <- if (s.compileFailures.nonEmpty) printCompileFailures(s.compileFailures) else IO.unit
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
            _ <- log(s"  ${SConsole.RED}x${SConsole.RESET} ${f.project}$countSuffix")
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

    private def printBuildSummary: IO[Unit] =
      for {
        s <- summary
        _ <- BuildSummary.formatSummary(s, mode).traverse_(log)
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
        // For failed compiles, also show the actual errors
        if (cf.status == bleep.bsp.protocol.CompileStatus.Failed && cf.diagnostics.nonEmpty) {
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
            sf.passed,
            sf.failed,
            sf.skipped,
            sf.ignored,
            sf.durationMs
          )
          log(BuildDiff.formatSuiteDiff(diff))
        }

      case BuildEvent.SuiteTimedOut(_, suite, timeoutMs, _, _) =>
        logError(s"[TIMEOUT] $suite after ${timeoutMs / 1000}s")

      case BuildEvent.SuiteError(_, suite, error, _, signal, _, _) =>
        val desc = signal.map(s => s"Process crashed (signal $s)").getOrElse(error)
        logError(s"[ERROR] $suite: $desc")

      case BuildEvent.Error(project, message, _, _) =>
        val projectInfo = if (project.nonEmpty) s" [$project]" else ""
        logError(s"[ERROR]$projectInfo $message")

      case BuildEvent.CompileStalled(project, usedMb, maxMb, retryAtMs, _) =>
        val waitSec = math.max(0, (retryAtMs - System.currentTimeMillis()) / 1000)
        if (waitSec > 0) logWarn(s"$project: compilation stalled (heap: ${usedMb}MB/${maxMb}MB) — retrying in ${waitSec}s")
        else IO.unit

      case _ => IO.unit
    }

    override def summary: IO[BuildSummary] =
      for {
        s <- state.get
        now <- IO.realTime.map(_.toMillis)
      } yield s.toSummary(durationMs = now - startTime, wasCancelled = false)

    override def printSummary: IO[Unit] =
      for {
        s <- summary
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
            log(BuildDiff.formatCompileSummary(s.compilesCompleted, totalErrors, 0, totalNew, totalFixed))

          case BuildMode.Test =>
            val newFailures = allTests.count { t =>
              val key = (t.project, t.suite, t.test)
              val prev = previousRun.testResults.get(key)
              val prevFailed = prev.exists(_.isFailure)
              t.status.isFailure && !prevFailed
            }
            val fixedTests = previousRun.testResults.count { case (key, prev) =>
              val prevFailed = prev.isFailure
              val currentResult = allTests.find(t => (t.project, t.suite, t.test) == key)
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

    override def printSummary: IO[Unit] = IO.unit // Fancy display handles this
  }
}
