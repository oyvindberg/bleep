package bleep.mcp

import bleep.BleepException
import bleep.bsp.protocol.{BleepBspProtocol, CompileReason, CompileStatus, DiagnosticSeverity, SuiteOutcome, TestStatus}
import bleep.bsp.protocol.BleepBspProtocol.Diagnostic
import bleep.model.{CrossProjectName, ProjectName, SuiteName, TestName}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

/** Scenario tests for [[RequestDiff]], built from synthetic transcripts.
  *
  * The load-bearing property is determinism of the MECHANICAL diff: it is computed over a projection of the transcript that contains no time, so two runs with
  * the same logical outcome — but wildly different durations and timestamps — must diff as `identical`. Timing comparisons are a separate operation with a
  * jitter threshold. Every scenario here mirrors a question an agent actually asks after an edit-and-rerun cycle.
  */
class RequestDiffTest extends AnyFunSuite with Matchers {

  import BleepBspProtocol.{Event => E}

  // ==========================================================================
  // Builders
  // ==========================================================================

  private def proj(name: String): CrossProjectName = CrossProjectName(ProjectName(name), crossId = None)

  /** A finished test. `at`/`tookMs` are deliberately varied across runs in scenarios — the mechanical diff must never see them. */
  private def test(
      project: String,
      suite: String,
      name: String,
      status: TestStatus,
      message: Option[String] = None,
      tookMs: Long = 10L,
      at: Long = 0L
  ): E.TestFinished =
    E.TestFinished(proj(project), SuiteName(suite), TestName(name), status, tookMs, message, throwable = None, timestamp = at, location = None)

  private def suiteFinished(project: String, suite: String, outcome: SuiteOutcome, tookMs: Long = 100L): E.SuiteFinished =
    E.SuiteFinished(proj(project), SuiteName(suite), outcome, tookMs, timestamp = 0L)

  private def compiled(
      project: String,
      reason: CompileReason,
      invalidated: List[String],
      status: CompileStatus = CompileStatus.Success,
      diagnostics: List[Diagnostic] = Nil,
      changedDeps: List[String] = Nil,
      tookMs: Long = 100L
  ): List[E] =
    List(
      E.CompilationReason(proj(project), reason, totalFiles = 10, invalidatedFiles = invalidated, changedDependencies = changedDeps, timestamp = 0L),
      E.CompileFinished(proj(project), status, tookMs, diagnostics, skippedBecause = None, timestamp = 0L)
    )

  private def diag(severity: DiagnosticSeverity, message: String, path: String, line: Int): Diagnostic =
    Diagnostic(severity = severity, message = message, rendered = None, path = Some(path), line = Some(line), column = None)

  private def logWith(entries: (String, List[E])*): (RequestLog, Map[String, Long]) =
    entries.zipWithIndex.foldLeft((RequestLog.empty, Map.empty[String, Long])) { case ((log, ids), ((mode, events), i)) =>
      val workspace = s"/ws/main"
      val (next, id) = log.push(timestampMs = 1000L + i, workspace = workspace, mode = mode, events = events, testRunResult = None)
      (next, ids.updated(s"run${i + 1}", id))
    }

  private def field(json: io.circe.Json, name: String): io.circe.Json =
    json.hcursor.downField(name).focus.getOrElse(fail(s"expected field '$name' in ${json.noSpaces}"))

  private def arrayNames(json: io.circe.Json, section: String, nameField: String): List[String] =
    json.hcursor.downField(section).focus match {
      case None      => Nil
      case Some(arr) => arr.asArray.getOrElse(fail(s"$section is not an array")).toList.map(j => field(j, nameField).asString.get)
    }

  private def isIdentical(json: io.circe.Json): Boolean = field(json, "identical").asBoolean.get

  // ==========================================================================
  // Mechanical: determinism
  // ==========================================================================

  test("mechanical: identical logical outcome with completely different timings diffs as identical") {
    val (log, ids) = logWith(
      "test" -> List(
        test("app", "S", "a", TestStatus.Passed, tookMs = 10, at = 1),
        test("app", "S", "b", TestStatus.Failed, message = Some("boom"), tookMs = 20, at = 2),
        test("app", "S", "c", TestStatus.AssumptionFailed, message = Some("no libgc"), tookMs = 0, at = 3)
      ),
      "test" -> List(
        // same statuses and messages, wildly different durations and timestamps, different event order
        test("app", "S", "c", TestStatus.AssumptionFailed, message = Some("no libgc"), tookMs = 999, at = 77777),
        test("app", "S", "a", TestStatus.Passed, tookMs = 5000, at = 99999),
        test("app", "S", "b", TestStatus.Failed, message = Some("boom"), tookMs = 1, at = 88888)
      )
    )
    val d = RequestDiff.mechanical(log, ids("run1"), ids("run2"))
    withClue(s"timing jitter must be invisible to the mechanical diff: ${d.noSpaces} ") {
      isIdentical(d) shouldBe true
    }
    field(d, "summary").asString.get shouldBe "No logical differences."
    // the unchanged failure is still listed as context, but does not break identity
    arrayNames(d, "stillFailing", "test") shouldBe List("b")
  }

  // ==========================================================================
  // Mechanical: transitions
  // ==========================================================================

  test("mechanical: break and fix — pass->fail then fail->pass") {
    val (log, ids) = logWith(
      "test" -> List(test("app", "S", "a", TestStatus.Passed)),
      "test" -> List(test("app", "S", "a", TestStatus.Failed, message = Some("expected 2, got 3"))),
      "test" -> List(test("app", "S", "a", TestStatus.Passed))
    )
    val broke = RequestDiff.mechanical(log, ids("run1"), ids("run2"))
    isIdentical(broke) shouldBe false
    arrayNames(broke, "newlyFailing", "test") shouldBe List("a")
    val entry = field(broke, "newlyFailing").asArray.get.head
    field(entry, "from").asString.get shouldBe "passed"
    field(entry, "to").asString.get shouldBe "failed"
    field(entry, "message").asString.get shouldBe "expected 2, got 3"

    val fixed = RequestDiff.mechanical(log, ids("run2"), ids("run3"))
    arrayNames(fixed, "fixed", "test") shouldBe List("a")
    fixed.hcursor.downField("newlyFailing").focus shouldBe None
  }

  test("mechanical: still failing distinguishes same failure from changed failure") {
    val (log, ids) = logWith(
      "test" -> List(
        test("app", "S", "same", TestStatus.Failed, message = Some("boom")),
        test("app", "S", "changed", TestStatus.Failed, message = Some("boom"))
      ),
      "test" -> List(
        test("app", "S", "same", TestStatus.Failed, message = Some("boom")),
        test("app", "S", "changed", TestStatus.Failed, message = Some("different boom"))
      )
    )
    val d = RequestDiff.mechanical(log, ids("run1"), ids("run2"))
    // one changed failure IS a logical difference; the unchanged one is context
    isIdentical(d) shouldBe false
    val bySuite = field(d, "stillFailing").asArray.get.map(j => field(j, "test").asString.get -> field(j, "messageChanged").asBoolean.get).toMap
    bySuite shouldBe Map("same" -> false, "changed" -> true)
    field(d, "summary").asString.get should include("1 stillFailing with changed failure")
  }

  test("mechanical: skip transitions — newly skipped carries the reason, unskipping and skip-to-fail are visible") {
    val (log, ids) = logWith(
      "test" -> List(
        test("app", "S", "becomesSkipped", TestStatus.Passed),
        test("app", "S", "becomesUnskipped", TestStatus.Skipped),
        test("app", "S", "skippedToFailing", TestStatus.AssumptionFailed, message = Some("no docker"))
      ),
      "test" -> List(
        test("app", "S", "becomesSkipped", TestStatus.AssumptionFailed, message = Some("Boehm GC (libgc) not installed")),
        test("app", "S", "becomesUnskipped", TestStatus.Passed),
        test("app", "S", "skippedToFailing", TestStatus.Failed, message = Some("docker now present but test broken"))
      )
    )
    val d = RequestDiff.mechanical(log, ids("run1"), ids("run2"))
    arrayNames(d, "newlySkipped", "test") shouldBe List("becomesSkipped")
    field(field(d, "newlySkipped").asArray.get.head, "reason").asString.get shouldBe "Boehm GC (libgc) not installed"
    arrayNames(d, "unskipped", "test") shouldBe List("becomesUnskipped")
    withClue("a test that was skipped and now fails is a NEW failure, not an unskip: ") {
      arrayNames(d, "newlyFailing", "test") shouldBe List("skippedToFailing")
    }
  }

  test("mechanical: scope drift shows as added/removed, never as fixed") {
    val (log, ids) = logWith(
      "test" -> List(
        test("app", "S", "a", TestStatus.Passed),
        test("app", "S", "failing", TestStatus.Failed, message = Some("boom"))
      ),
      // rerun with --only S2: different scope entirely
      "test" -> List(test("app", "S2", "b", TestStatus.Passed))
    )
    val d = RequestDiff.mechanical(log, ids("run1"), ids("run2"))
    withClue("a failing test that left the scope must NOT be reported as fixed: ") {
      d.hcursor.downField("fixed").focus shouldBe None
    }
    arrayNames(d, "removed", "test").toSet shouldBe Set("a", "failing")
    arrayNames(d, "added", "test") shouldBe List("b")
  }

  test("mechanical: suite outcome kind changes surface — a suite that stops executing is not just missing tests") {
    val (log, ids) = logWith(
      "test" -> List(
        suiteFinished("app", "S", SuiteOutcome.Executed(passed = 2, failed = 0, skipped = 0, ignored = 0)),
        test("app", "S", "a", TestStatus.Passed),
        test("app", "S", "b", TestStatus.Passed)
      ),
      "test" -> List(
        E.SuiteError(proj("app"), SuiteName("S"), "OOM while loading suite", bleep.bsp.protocol.ProcessExit.ExitCode(137), durationMs = 5L, timestamp = 0L)
      )
    )
    val d = RequestDiff.mechanical(log, ids("run1"), ids("run2"))
    val changes = field(d, "suiteOutcomeChanges").asArray.get
    changes should have size 1
    field(changes.head, "from").asString.get shouldBe "executed"
    field(changes.head, "to").asString.get shouldBe "errored"
  }

  // ==========================================================================
  // Mechanical: compile mode
  // ==========================================================================

  test("mechanical compile: the copy-state question — everything up-to-date except the edited project") {
    val (log, ids) = logWith(
      "compile" -> (compiled("core", CompileReason.UpToDate, Nil) ++ compiled("app", CompileReason.UpToDate, Nil)),
      "compile" -> (compiled("core", CompileReason.UpToDate, Nil, tookMs = 999) ++ compiled("app", CompileReason.Incremental, List("App.scala"), tookMs = 5000))
    )
    val d = RequestDiff.mechanical(log, ids("run1"), ids("run2"))
    isIdentical(d) shouldBe false
    val changed = field(d, "changed").asArray.get
    withClue("only the edited project may appear — core's duration change is not a logical change: ") {
      changed.map(j => field(j, "project").asString.get) shouldBe Vector("app")
    }
    val app = changed.head
    field(field(app, "reason"), "from").asString.get shouldBe "up-to-date"
    field(field(app, "reason"), "to").asString.get shouldBe "incremental"
    field(app, "invalidatedFilesAdded").asArray.get.map(_.asString.get) shouldBe Vector("App.scala")
  }

  test("mechanical compile: new and resolved diagnostics; a line-only move is neither") {
    val warnOld = diag(DiagnosticSeverity.Warning, "unused import", "src/A.scala", line = 3)
    val warnMoved = diag(DiagnosticSeverity.Warning, "unused import", "src/A.scala", line = 17) // same identity, different line
    val errNew = diag(DiagnosticSeverity.Error, "not found: value oops", "src/A.scala", line = 9)
    val warnGone = diag(DiagnosticSeverity.Warning, "deprecated api", "src/B.scala", line = 1)

    val (log, ids) = logWith(
      "compile" -> compiled("app", CompileReason.Incremental, List("A.scala"), diagnostics = List(warnOld, warnGone)),
      "compile" -> compiled("app", CompileReason.Incremental, List("A.scala"), status = CompileStatus.Failed, diagnostics = List(warnMoved, errNew))
    )
    val d = RequestDiff.mechanical(log, ids("run1"), ids("run2"))
    val app = field(d, "changed").asArray.get.head

    val newMsgs = field(app, "newDiagnostics").asArray.get.map(j => field(j, "message").asString.get)
    withClue("the moved-but-identical warning must not be reported as new: ") {
      newMsgs shouldBe Vector("not found: value oops")
    }
    val resolvedMsgs = field(app, "resolvedDiagnostics").asArray.get.map(j => field(j, "message").asString.get)
    withClue("nor as resolved: ") {
      resolvedMsgs shouldBe Vector("deprecated api")
    }
    field(field(app, "status"), "to").asString.get shouldBe "failed"
  }

  test("mechanical compile: identical outcomes with different durations diff as identical") {
    val (log, ids) = logWith(
      "compile" -> (compiled("core", CompileReason.Incremental, List("Core.scala"), tookMs = 100) ++ compiled("app", CompileReason.UpToDate, Nil, tookMs = 5)),
      "compile" -> (compiled("core", CompileReason.Incremental, List("Core.scala"), tookMs = 9999) ++ compiled(
        "app",
        CompileReason.UpToDate,
        Nil,
        tookMs = 800
      ))
    )
    val d = RequestDiff.mechanical(log, ids("run1"), ids("run2"))
    withClue(s"durations are not part of the compile projection: ${d.noSpaces} ") {
      isIdentical(d) shouldBe true
    }
  }

  test("mechanical compile: cross-workspace diff is allowed and flagged") {
    val (logA, idA) = RequestLog.empty.push(1L, "/ws/parent", "compile", compiled("core", CompileReason.UpToDate, Nil), None)
    val (logB, idB) = logA.push(2L, "/ws/fork", "compile", compiled("core", CompileReason.UpToDate, Nil), None)
    val d = RequestDiff.mechanical(logB, idA, idB)
    isIdentical(d) shouldBe true
    field(d, "crossWorkspace").asBoolean.get shouldBe true
  }

  // ==========================================================================
  // Errors
  // ==========================================================================

  test("refuses to diff compile against test, and unknown/evicted ids fail loudly") {
    val (log, ids) = logWith(
      "compile" -> compiled("app", CompileReason.UpToDate, Nil),
      "test" -> List(test("app", "S", "a", TestStatus.Passed))
    )
    val modeMix = intercept[BleepException.Text](RequestDiff.mechanical(log, ids("run1"), ids("run2")))
    modeMix.getMessage should include("Cannot diff a compile request")

    val missing = intercept[BleepException.Text](RequestDiff.mechanical(log, 999L, ids("run2")))
    missing.getMessage should include("No request with id 999")
  }

  // ==========================================================================
  // Timing
  // ==========================================================================

  test("timing: significant regressions and improvements are reported, jitter is suppressed") {
    val (log, ids) = logWith(
      "test" -> List(
        test("app", "S", "regressed", TestStatus.Passed, tookMs = 1000),
        test("app", "S", "improved", TestStatus.Passed, tookMs = 1000),
        test("app", "S", "jitterSmall", TestStatus.Passed, tookMs = 100), // +19ms: under both thresholds
        test("app", "S", "jitterPercent", TestStatus.Passed, tookMs = 10000) // +1500ms: over 50ms but under 20%
      ),
      "test" -> List(
        test("app", "S", "regressed", TestStatus.Passed, tookMs = 2500),
        test("app", "S", "improved", TestStatus.Passed, tookMs = 300),
        test("app", "S", "jitterSmall", TestStatus.Passed, tookMs = 119),
        test("app", "S", "jitterPercent", TestStatus.Passed, tookMs = 11500)
      )
    )
    val d = RequestDiff.timing(log, ids("run1"), ids("run2"), limit = 10)

    arrayNames(d, "slower", "test") shouldBe List("regressed")
    field(field(d, "slower").asArray.get.head, "deltaMs").asNumber.get.toLong.get shouldBe 1500L
    arrayNames(d, "faster", "test") shouldBe List("improved")
    field(d, "insignificantDeltasSuppressed").asNumber.get.toInt.get shouldBe 2
    field(d, "totalDeltaMs").asNumber.get.toLong.get shouldBe (2500 + 300 + 119 + 11500) - (1000 + 1000 + 100 + 10000)
  }

  test("timing: slowestInTarget answers the absolute question, ordered and limited") {
    val (log, ids) = logWith(
      "test" -> List(test("app", "S", "a", TestStatus.Passed, tookMs = 1)),
      "test" -> List(
        test("app", "S", "slowest", TestStatus.Passed, tookMs = 5000),
        test("app", "S", "middle", TestStatus.Passed, tookMs = 3000),
        test("app", "S", "fast", TestStatus.Passed, tookMs = 10)
      )
    )
    val d = RequestDiff.timing(log, ids("run1"), ids("run2"), limit = 2)
    arrayNames(d, "slowestInTarget", "test") shouldBe List("slowest", "middle")
  }

  test("timing compile: per-project compile durations compare with the same threshold") {
    val (log, ids) = logWith(
      "compile" -> (compiled("core", CompileReason.Incremental, List("Core.scala"), tookMs = 1000) ++ compiled(
        "app",
        CompileReason.UpToDate,
        Nil,
        tookMs = 40
      )),
      "compile" -> (compiled("core", CompileReason.Incremental, List("Core.scala"), tookMs = 4000) ++ compiled("app", CompileReason.UpToDate, Nil, tookMs = 45))
    )
    val d = RequestDiff.timing(log, ids("run1"), ids("run2"), limit = 10)
    arrayNames(d, "slower", "project") shouldBe List("core")
    withClue("app's 5ms wiggle is jitter: ") {
      field(d, "insignificantDeltasSuppressed").asNumber.get.toInt.get shouldBe 1
    }
  }
}
