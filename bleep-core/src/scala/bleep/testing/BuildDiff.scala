package bleep.testing

import bleep.bsp.protocol.{BleepBspProtocol, CompileStatus, DiagnosticSeverity, TestStatus}

/** Indexed view of a previous run's results, built from events, for fast per-project diff lookups.
  *
  * Used by both CLI `--diff-watch` mode and MCP server to compute terse per-project diffs between consecutive build cycles.
  */
case class PreviousRunState(
    compileDiagnostics: Map[String, List[BleepBspProtocol.Diagnostic]],
    testResults: Map[(String, String, String), TestStatus] // (project, suite, test) -> status
)

object PreviousRunState {
  val empty: PreviousRunState = PreviousRunState(Map.empty, Map.empty)

  /** Build from a list of BuildEvents collected during a run */
  def fromEvents(events: List[BuildEvent]): PreviousRunState = {
    val compileDiags = Map.newBuilder[String, List[BleepBspProtocol.Diagnostic]]
    val testRes = Map.newBuilder[(String, String, String), TestStatus]

    events.foreach {
      case e: BuildEvent.CompileFinished =>
        compileDiags += e.project -> e.diagnostics
      case e: BuildEvent.TestFinished =>
        testRes += (e.project, e.suite, e.test) -> e.status
      case _ => ()
    }

    PreviousRunState(compileDiags.result(), testRes.result())
  }

  /** Build from a list of BleepBspProtocol events (used by MCP server which stores protocol events directly) */
  def fromProtocolEvents(events: List[BleepBspProtocol.Event]): PreviousRunState = {
    val compileDiags = Map.newBuilder[String, List[BleepBspProtocol.Diagnostic]]
    val testRes = Map.newBuilder[(String, String, String), TestStatus]

    events.foreach {
      case e: BleepBspProtocol.Event.CompileFinished =>
        compileDiags += e.project -> e.diagnostics
      case e: BleepBspProtocol.Event.TestFinished =>
        testRes += (e.project, e.suite, e.test) -> e.status
      case _ => ()
    }

    PreviousRunState(compileDiags.result(), testRes.result())
  }
}

/** Per-project diff results for compile and test operations. */
object BuildDiff {

  /** Diagnostic matching key — file path (without line:col) + message.
    *
    * Line numbers are stripped because they shift between edits and would cause noisy "1 fixed, 1 new" churn. Message alone is specific enough (includes
    * concrete types, variable names, etc.) to distinguish different errors.
    */
  case class DiagKey(file: Option[String], message: String)

  private def diagKey(d: BleepBspProtocol.Diagnostic): DiagKey = {
    val fileOnly = d.path.map { p =>
      val colonIdx = p.indexOf(':')
      if (colonIdx > 0) p.substring(0, colonIdx) else p
    }
    DiagKey(fileOnly, d.message)
  }

  /** Result of diffing diagnostics for a single project */
  case class CompileDiff(
      project: String,
      status: CompileStatus,
      totalErrors: Int,
      totalWarnings: Int,
      newErrors: Int,
      fixedErrors: Int,
      newWarnings: Int,
      fixedWarnings: Int,
      durationMs: Long
  )

  /** Format a terse one-liner for a compile diff */
  def formatCompileDiff(diff: CompileDiff): String = {
    val project = diff.project
    val duration = s"(${diff.durationMs}ms)"

    if (diff.status == CompileStatus.Success || diff.status == CompileStatus.Skipped) {
      if (diff.fixedErrors > 0) {
        val fixedStr = s"fixed ${diff.fixedErrors} error${plural(diff.fixedErrors)}"
        val warningStr = if (diff.totalWarnings > 0) s", ${diff.totalWarnings} warning${plural(diff.totalWarnings)}" else ""
        s"$project: OK ($fixedStr$warningStr) $duration"
      } else if (diff.totalWarnings > 0) {
        s"$project: OK (${diff.totalWarnings} warning${plural(diff.totalWarnings)}) $duration"
      } else {
        s"$project: OK $duration"
      }
    } else {
      // Failed
      val errorCount = diff.totalErrors
      val detail = if (diff.newErrors > 0 && diff.fixedErrors > 0) {
        s"${diff.newErrors} new, ${diff.fixedErrors} fixed"
      } else if (diff.newErrors > 0 && diff.newErrors == diff.totalErrors) {
        "all new"
      } else if (diff.newErrors > 0) {
        val remaining = diff.totalErrors - diff.newErrors
        s"${diff.newErrors} new, $remaining remaining"
      } else {
        s"${diff.totalErrors} remaining"
      }
      val warningStr = if (diff.totalWarnings > 0) s", ${diff.totalWarnings} warning${plural(diff.totalWarnings)}" else ""
      s"$project: $errorCount error${plural(errorCount)} ($detail$warningStr) $duration"
    }
  }

  /** Count occurrences of each key — a multiset */
  private def countKeys(diags: List[BleepBspProtocol.Diagnostic]): Map[DiagKey, Int] =
    diags.map(diagKey).groupBy(identity).map { case (k, vs) => k -> vs.size }

  /** Diff two multisets: returns (newCount, fixedCount).
    *
    * For each key, if current has more occurrences than previous, the excess are "new". If previous had more, the deficit are "fixed".
    */
  private def diffMultiset(current: Map[DiagKey, Int], previous: Map[DiagKey, Int]): (Int, Int) = {
    val allKeys = current.keySet ++ previous.keySet
    var newCount = 0
    var fixedCount = 0
    allKeys.foreach { k =>
      val cur = current.getOrElse(k, 0)
      val prev = previous.getOrElse(k, 0)
      if (cur > prev) newCount += (cur - prev)
      else if (prev > cur) fixedCount += (prev - cur)
    }
    (newCount, fixedCount)
  }

  /** Compute compile diff for a single project */
  def diffCompile(
      project: String,
      status: CompileStatus,
      currentDiagnostics: List[BleepBspProtocol.Diagnostic],
      previousDiagnostics: List[BleepBspProtocol.Diagnostic],
      durationMs: Long
  ): CompileDiff = {
    val currentErrors = currentDiagnostics.filter(_.severity == DiagnosticSeverity.Error)
    val currentWarnings = currentDiagnostics.filter(_.severity == DiagnosticSeverity.Warning)
    val previousErrors = previousDiagnostics.filter(_.severity == DiagnosticSeverity.Error)
    val previousWarnings = previousDiagnostics.filter(_.severity == DiagnosticSeverity.Warning)

    val (newErrors, fixedErrors) = diffMultiset(countKeys(currentErrors), countKeys(previousErrors))
    val (newWarnings, fixedWarnings) = diffMultiset(countKeys(currentWarnings), countKeys(previousWarnings))

    CompileDiff(
      project = project,
      status = status,
      totalErrors = currentErrors.size,
      totalWarnings = currentWarnings.size,
      newErrors = newErrors,
      fixedErrors = fixedErrors,
      newWarnings = newWarnings,
      fixedWarnings = fixedWarnings,
      durationMs = durationMs
    )
  }

  /** Result of diffing test results for a single suite */
  case class SuiteDiff(
      project: String,
      suite: String,
      passed: Int,
      failed: Int,
      skipped: Int,
      ignored: Int,
      newFailures: List[String], // test names that flipped to fail
      fixedTests: List[String], // test names that flipped to pass
      stillFailing: List[String], // test names that were failing and still are
      durationMs: Long
  )

  /** Format a terse one-liner for a suite diff */
  def formatSuiteDiff(diff: SuiteDiff): String = {
    val id = s"${diff.project} ${diff.suite}"
    val parts = List.newBuilder[String]
    parts += s"${diff.passed} passed"
    if (diff.failed > 0) parts += s"${diff.failed} failed"
    if (diff.skipped > 0) parts += s"${diff.skipped} skipped"
    if (diff.ignored > 0) parts += s"${diff.ignored} ignored"
    val countsStr = parts.result().mkString(", ")

    val diffParts = List.newBuilder[String]
    diff.fixedTests.foreach(t => diffParts += s"$t fixed")
    diff.newFailures.foreach(t => diffParts += s"$t new failure")
    diff.stillFailing.foreach(t => diffParts += s"$t still failing")
    val diffDetails = diffParts.result()

    if (diffDetails.nonEmpty) {
      s"$id: $countsStr (${diffDetails.mkString(", ")})"
    } else {
      s"$id: $countsStr"
    }
  }

  /** Compute suite diff by comparing current test results with previous */
  def diffSuite(
      project: String,
      suite: String,
      currentTests: List[BuildEvent.TestFinished],
      previousTestResults: Map[(String, String, String), TestStatus],
      passed: Int,
      failed: Int,
      skipped: Int,
      ignored: Int,
      durationMs: Long
  ): SuiteDiff = {
    val newFailures = List.newBuilder[String]
    val fixedTests = List.newBuilder[String]
    val stillFailing = List.newBuilder[String]

    currentTests.foreach { t =>
      val key = (t.project, t.suite, t.test)
      val prevStatus = previousTestResults.get(key)
      val currentFailed = t.status.isFailure

      prevStatus match {
        case Some(prev) =>
          val prevFailed = prev.isFailure
          if (currentFailed && !prevFailed) newFailures += t.test
          else if (!currentFailed && prevFailed) fixedTests += t.test
          else if (currentFailed && prevFailed) stillFailing += t.test
        case None =>
          if (currentFailed) newFailures += t.test
      }
    }

    SuiteDiff(
      project = project,
      suite = suite,
      passed = passed,
      failed = failed,
      skipped = skipped,
      ignored = ignored,
      newFailures = newFailures.result(),
      fixedTests = fixedTests.result(),
      stillFailing = stillFailing.result(),
      durationMs = durationMs
    )
  }

  /** Format a terse end-of-build summary line for compile diff */
  def formatCompileSummary(
      totalProjects: Int,
      totalErrors: Int,
      totalWarnings: Int,
      newErrors: Int,
      fixedErrors: Int
  ): String =
    if (totalErrors == 0 && fixedErrors > 0) {
      s"Build: $totalProjects projects compiled, all clear (fixed $fixedErrors error${plural(fixedErrors)})"
    } else if (totalErrors == 0) {
      s"Build: $totalProjects projects compiled, all clear"
    } else {
      val parts = List.newBuilder[String]
      if (fixedErrors > 0) parts += s"$fixedErrors fixed"
      if (newErrors > 0) parts += s"$newErrors new"
      val remaining = totalErrors - newErrors
      if (remaining > 0 && newErrors > 0) parts += s"$remaining remaining"
      val detail = parts.result()
      val detailStr = if (detail.nonEmpty) s" (${detail.mkString(", ")})" else ""
      s"Build: $totalProjects projects compiled, $totalErrors error${plural(totalErrors)}$detailStr"
    }

  /** Format a terse end-of-build summary line for test diff */
  def formatTestSummary(
      totalPassed: Int,
      totalFailed: Int,
      newFailures: Int,
      fixedTests: Int
  ): String =
    if (totalFailed == 0 && fixedTests > 0) {
      s"Tests: $totalPassed passed (${fixedTests} fixed)"
    } else if (totalFailed == 0) {
      s"Tests: $totalPassed passed"
    } else {
      val parts = List.newBuilder[String]
      if (fixedTests > 0) parts += s"$fixedTests fixed"
      if (newFailures > 0) parts += s"$newFailures new failure${plural(newFailures)}"
      val detail = parts.result()
      val detailStr = if (detail.nonEmpty) s" (${detail.mkString(", ")})" else ""
      s"Tests: $totalPassed passed, $totalFailed failed$detailStr"
    }

  private def plural(n: Int): String = if (n == 1) "" else "s"
}
