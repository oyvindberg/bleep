package bleep.history

import bleep.BleepException
import bleep.bsp.protocol.{BleepBspProtocol, CompileReason, CompileStatus, DiagnosticSeverity, OutputChannel, SuiteOutcome, TestStatus}
import bleep.model.{CrossProjectName, ProjectName, SuiteName, TestName}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

/** The details view is now one implementation shared by `bleep history show` (CLI) and `bleep.history.show` (MCP). These tests pin its contract: the transcript
  * header always rides along, project/query narrowing filters items while summary counts keep describing the full run, and a bad regex fails loudly.
  */
class TranscriptFormatTest extends AnyFunSuite with Matchers {

  import BleepBspProtocol.{Event => E}

  private def proj(name: String): CrossProjectName = CrossProjectName(ProjectName(name), crossId = None)

  private def diag(severity: DiagnosticSeverity, message: String, path: String): BleepBspProtocol.Diagnostic =
    BleepBspProtocol.Diagnostic(severity = severity, message = message, rendered = None, path = Some(path), line = Some(3), column = Some(1))

  private def transcript(mode: String, events: List[E]): Transcript =
    Transcript(
      id = 7L,
      timestampMs = 1234L,
      workspace = "/ws",
      variant = "normal",
      mode = mode,
      targets = List("app"),
      client = "bleep",
      events = events,
      testRunResult = None
    )

  private val compileTranscript: Transcript = transcript(
    "compile",
    List(
      E.CompilationReason(
        proj("app"),
        CompileReason.Incremental,
        totalFiles = 2,
        invalidatedFiles = List("A.scala"),
        changedDependencies = Nil,
        timestamp = 1L
      ),
      E.CompileFinished(
        proj("app"),
        CompileStatus.Failed,
        durationMs = 42L,
        diagnostics = List(
          diag(DiagnosticSeverity.Error, "value frobnicate is not a member", "/ws/src/A.scala"),
          diag(DiagnosticSeverity.Warning, "unused import", "/ws/src/B.scala")
        ),
        skippedBecause = None,
        timestamp = 2L
      )
    )
  )

  test("details carries the transcript header alongside the formatted result") {
    val json = TranscriptFormat.details(compileTranscript, project = None, query = None, limit = None, offset = None, maxTraceLines = None)
    json.hcursor.get[Long]("historyId") shouldBe Right(7L)
    json.hcursor.get[String]("mode") shouldBe Right("compile")
    json.hcursor.get[String]("workspace") shouldBe Right("/ws")
    json.hcursor.get[String]("client") shouldBe Right("bleep")
    json.hcursor.get[List[String]]("targets") shouldBe Right(List("app"))
    json.hcursor.get[Boolean]("success") shouldBe Right(false)
    json.hcursor.get[Int]("totalDiagnostics") shouldBe Right(2)
  }

  test("query narrows the diagnostics array while summary counts keep describing the full run") {
    val json = TranscriptFormat.details(compileTranscript, project = None, query = Some("frobnicate"), limit = None, offset = None, maxTraceLines = None)
    json.hcursor.get[Int]("totalDiagnostics") shouldBe Right(1)
    json.hcursor.get[Int]("errors") shouldBe Right(1)
    json.hcursor.get[Int]("warnings") shouldBe Right(1) // full-run count, not narrowed
    json.hcursor.downField("diagnostics").downArray.get[String]("message").toOption.get should include("frobnicate")
  }

  test("an invalid query regex fails loudly") {
    a[BleepException.Text] should be thrownBy
      TranscriptFormat.details(compileTranscript, project = None, query = Some("["), limit = None, offset = None, maxTraceLines = None)
  }

  test("test-mode details include collapsed stack traces and filter by project") {
    val events: List[E] = List(
      E.TestFinished(
        proj("app"),
        SuiteName("MySuite"),
        TestName("boom"),
        TestStatus.Failed,
        durationMs = 5L,
        message = Some("expected 1 but got 2"),
        throwable = Some("java.lang.AssertionError: nope\n  at MySuite.boom(MySuite.scala:9)"),
        timestamp = 1L,
        location = None
      ),
      E.TestFinished(
        proj("other"),
        SuiteName("OtherSuite"),
        TestName("fine"),
        TestStatus.Passed,
        durationMs = 5L,
        message = None,
        throwable = None,
        timestamp = 2L,
        location = None
      )
    )
    val json = TranscriptFormat.details(transcript("test", events), project = Some("app"), query = None, limit = None, offset = None, maxTraceLines = None)
    json.hcursor.get[Int]("totalFailures") shouldBe Right(1)
    val failure = json.hcursor.downField("failures").downArray
    failure.get[String]("suite") shouldBe Right("MySuite")
    failure.get[String]("throwable").toOption.get should include("AssertionError")
  }

  // === Everything that can sink a test run before or beside the tests must fail the result. ===
  // The founding incident: a dependency failed to compile, the test project was skipped, zero suites
  // ran — and the run reported `success: true, "0 tests passed"`.

  private def formatTest(events: List[E]): io.circe.Json =
    TranscriptFormat.formatTestResult(
      events,
      testRunResult = None,
      includeThrowables = false,
      includePassedSuiteOutput = false,
      maxTraceLines = None,
      query = None,
      limit = None,
      offset = None
    )

  private def formatTestWith(events: List[E], trr: BleepBspProtocol.TestRunResult): io.circe.Json =
    TranscriptFormat.formatTestResult(
      events,
      testRunResult = Some(trr),
      includeThrowables = false,
      includePassedSuiteOutput = false,
      maxTraceLines = None,
      query = None,
      limit = None,
      offset = None
    )

  private def runResult(passed: Int, suitesTotal: Int, suitesCompleted: Int): BleepBspProtocol.TestRunResult =
    BleepBspProtocol.TestRunResult(
      totalPassed = passed,
      totalFailed = 0,
      totalSkipped = 0,
      totalIgnored = 0,
      suitesTotal = suitesTotal,
      suitesCompleted = suitesCompleted,
      suitesFailed = 0,
      suitesCancelled = 0,
      durationMs = 0L,
      historyId = None
    )

  test("a partial run — fewer suites completed than discovered — is success:false over MCP and carries the suite counts") {
    // The `bleep_test dfmt/test` report: the MCP surface returned {"success":true,"passed":64,...} with NO suite line at all, so an agent driving bleep over MCP
    // could not know 19 of 29 suites never ran. The `success` boolean is the only verdict it has, and it lied. This pins both halves of the fix: the verdict flips
    // to false, and the discovered-vs-completed counts are present in the JSON so the gap is actionable, not merely implied by a summary string.
    val json = formatTestWith(Nil, runResult(passed = 64, suitesTotal = 29, suitesCompleted = 10))
    json.hcursor.get[Boolean]("success") shouldBe Right(false)
    json.hcursor.get[String]("summary").toOption.get should include("did not finish")
    json.hcursor.get[Int]("suitesTotal") shouldBe Right(29)
    json.hcursor.get[Int]("suitesCompleted") shouldBe Right(10)
    json.hcursor.get[Int]("suitesDidNotFinish") shouldBe Right(19)
  }

  test("a run where every discovered suite completed is success:true and omits the did-not-finish count") {
    val json = formatTestWith(List(passedTest("app")), runResult(passed = 29, suitesTotal = 29, suitesCompleted = 29))
    json.hcursor.get[Boolean]("success") shouldBe Right(true)
    json.hcursor.get[Int]("suitesTotal") shouldBe Right(29)
    json.hcursor.get[Int]("suitesCompleted") shouldBe Right(29)
    json.hcursor.get[Int]("suitesDidNotFinish").toOption shouldBe None
  }

  private def passedTest(p: String): E =
    E.TestFinished(
      proj(p),
      SuiteName("OkSuite"),
      TestName("ok"),
      TestStatus.Passed,
      durationMs = 1L,
      message = None,
      throwable = None,
      timestamp = 1L,
      location = None
    )

  test("a test run where a dependency failed to compile is a failure, with the compile error inlined") {
    val events: List[E] = List(
      E.CompileFinished(
        proj("core"),
        CompileStatus.Failed,
        durationMs = 42L,
        diagnostics = List(diag(DiagnosticSeverity.Error, "constructor Clause is undefined", "/ws/src/Doc.java")),
        skippedBecause = None,
        timestamp = 1L
      ),
      E.CompileFinished(proj("tests"), CompileStatus.Skipped, durationMs = 0L, diagnostics = Nil, skippedBecause = Some(proj("core")), timestamp = 2L)
    )
    val json = formatTest(events)
    json.hcursor.get[Boolean]("success") shouldBe Right(false)
    json.hcursor.get[String]("summary").toOption.get should include("failed to compile")
    json.hcursor.get[List[String]]("failedProjects") shouldBe Right(List("core"))
    json.hcursor.get[Int]("compileErrors") shouldBe Right(1)
    json.hcursor.downField("topErrors").downArray.get[String]("message").toOption.get should include("Clause")
    val skippedProject = json.hcursor.downField("skippedProjects").downArray
    skippedProject.get[List[String]]("projects") shouldBe Right(List("tests"))
    skippedProject.get[Int]("count") shouldBe Right(1)
    skippedProject.get[String]("reason").toOption.get should include("core")
  }

  test("skipped projects are grouped by reason: compact names the first few, details names them all") {
    val downstream = (1 to 40).map(i => f"p$i%02d").toList
    val events: List[E] =
      E.CompileFinished(proj("core"), CompileStatus.Failed, durationMs = 1L, diagnostics = Nil, skippedBecause = None, timestamp = 1L) ::
        downstream.map(p =>
          E.CompileFinished(proj(p), CompileStatus.Skipped, durationMs = 0L, diagnostics = Nil, skippedBecause = Some(proj("core")), timestamp = 2L)
        )

    val compact = TranscriptFormat.formatCompileResult(events, verbose = false, query = None, limit = None, offset = None)
    val groups = compact.hcursor.downField("skippedProjects").values.get.toList
    groups should have size 1
    groups.head.hcursor.get[Int]("count") shouldBe Right(40)
    groups.head.hcursor.get[List[String]]("projects") shouldBe Right(downstream.take(TranscriptFormat.SkippedProjectsShownPerReason))

    val verbose = TranscriptFormat.formatCompileResult(events, verbose = true, query = None, limit = None, offset = None)
    verbose.hcursor.downField("skippedProjects").downArray.get[List[String]]("projects") shouldBe Right(downstream)
  }

  test("an errored suite fails the run with a synthetic failure entry even though no test failed") {
    val events: List[E] = List(
      E.SuiteFinished(proj("app"), SuiteName("BrokenSuite"), SuiteOutcome.Errored("class initialization failed", None), durationMs = 1L, timestamp = 1L)
    )
    val json = formatTest(events)
    json.hcursor.get[Boolean]("success") shouldBe Right(false)
    json.hcursor.get[Int]("failed") shouldBe Right(1)
    val failure = json.hcursor.downField("failures").downArray
    failure.get[String]("suite") shouldBe Right("BrokenSuite")
    failure.get[String]("message").toOption.get should include("class initialization failed")
  }

  test("a cancelled suite fails the run and is listed with its reason") {
    val events: List[E] = List(
      passedTest("app"),
      E.SuiteFinished(proj("app"), SuiteName("OkSuite"), SuiteOutcome.Executed(1, 0, 0, 0), durationMs = 1L, timestamp = 2L),
      E.SuiteCancelled(proj("app"), SuiteName("NeverRan"), reason = Some("dependency core failed"), timestamp = 3L)
    )
    val json = formatTest(events)
    json.hcursor.get[Boolean]("success") shouldBe Right(false)
    json.hcursor.get[Int]("suitesNotRun") shouldBe Right(1)
    val cancelled = json.hcursor.downField("cancelledSuites").downArray
    cancelled.get[String]("suite") shouldBe Right("NeverRan")
    cancelled.get[String]("reason").toOption.get should include("core")
  }

  private def failedTest(name: String, message: String): E =
    E.TestFinished(
      proj("app"),
      SuiteName("Many"),
      TestName(name),
      TestStatus.Failed,
      durationMs = 1L,
      message = Some(message),
      throwable = None,
      timestamp = 1L,
      location = None
    )

  test("the compact result lists at most CompactFailures failures, messages cut at CompactMessageChars; details lists them whole") {
    val longMessage = "x" * 2000
    val events = (1 to 30).map(i => failedTest(f"t$i%02d", longMessage)).toList

    val compact = formatTest(events)
    compact.hcursor.get[Int]("totalFailures") shouldBe Right(30)
    compact.hcursor.get[Int]("failuresOmitted") shouldBe Right(30 - TranscriptFormat.CompactFailures)
    val failures = compact.hcursor.downField("failures").values.get.toList
    failures should have size TranscriptFormat.CompactFailures
    failures.head.hcursor.get[String]("message").map(_.length) shouldBe Right(TranscriptFormat.CompactMessageChars)
    failures.head.hcursor.get[Boolean]("messageTruncated") shouldBe Right(true)

    val details = TranscriptFormat.details(transcript("test", events), project = None, query = None, limit = None, offset = None, maxTraceLines = None)
    details.hcursor.downField("failures").values.get.toList should have size 30
    details.hcursor.downField("failures").downArray.get[String]("message").map(_.length) shouldBe Right(2000)
  }

  test("passed suite output is one string per line, stderr marked, only the tail of each suite") {
    val output = (1 to 80).map(i => E.Output(proj("app"), SuiteName("OkSuite"), s"line $i", OutputChannel.Stdout, timestamp = i.toLong)).toList :+
      E.Output(proj("app"), SuiteName("OkSuite"), "oops", OutputChannel.Stderr, timestamp = 81L)
    val events = output ++ List(
      passedTest("app"),
      E.SuiteFinished(proj("app"), SuiteName("OkSuite"), SuiteOutcome.Executed(1, 0, 0, 0), durationMs = 1L, timestamp = 100L)
    )
    val json = TranscriptFormat.formatTestResult(
      events,
      testRunResult = None,
      includeThrowables = false,
      includePassedSuiteOutput = true,
      maxTraceLines = None,
      query = None,
      limit = None,
      offset = None
    )
    val suite = json.hcursor.downField("passedSuiteOutput").downArray
    suite.get[Int]("lines") shouldBe Right(81)
    suite.get[Int]("omittedLines") shouldBe Right(81 - TranscriptFormat.PassedSuiteOutputTailLines)
    val lines = suite.get[List[String]]("output").toOption.get
    lines should have size TranscriptFormat.PassedSuiteOutputTailLines
    lines.last shouldBe "! oops"
    lines.init.last shouldBe "line 80"
  }

  test("details cuts each stack trace to maxTraceLines, pages with nextOffset, and names only the first targets") {
    val dump = (1 to 500).map(i => s"  at Frame$i.run(Frame.java:$i)").mkString("java.lang.AssertionError: timed out\n", "\n", "")
    val events = (1 to 7).map { i =>
      E.TestFinished(
        proj("app"),
        SuiteName("Slow"),
        TestName(s"t$i"),
        TestStatus.Failed,
        durationMs = 1L,
        message = Some("timed out"),
        throwable = Some(dump),
        timestamp = 1L,
        location = None
      )
    }.toList
    val wide = transcript("test", events).copy(targets = (1 to 30).map(i => s"p$i").toList)

    val json = TranscriptFormat.details(wide, project = None, query = None, limit = Some(5), offset = None, maxTraceLines = Some(60))
    json.hcursor.get[Int]("targetCount") shouldBe Right(30)
    json.hcursor.get[List[String]]("targets").map(_.size) shouldBe Right(TranscriptFormat.TargetsShown)
    json.hcursor.get[Int]("totalFailures") shouldBe Right(7)
    json.hcursor.get[Int]("nextOffset") shouldBe Right(5)
    val first = json.hcursor.downField("failures").downArray
    first.get[String]("throwable").map(_.linesIterator.size) shouldBe Right(60)
    first.get[Int]("throwableLinesOmitted") shouldBe Right(501 - 60)

    val lastPage = TranscriptFormat.details(wide, project = None, query = None, limit = Some(5), offset = Some(5), maxTraceLines = None)
    lastPage.hcursor.get[Int]("nextOffset").toOption shouldBe None
    lastPage.hcursor.downField("failures").downArray.get[String]("throwable").map(_.linesIterator.size) shouldBe Right(501)
  }

  test("a run where nothing at all happened never claims tests passed") {
    val json = formatTest(Nil)
    json.hcursor.get[Boolean]("success") shouldBe Right(true)
    json.hcursor.get[String]("summary").toOption.get should include("no test suites ran")
  }

  test("a compile run where sourcegen failed and every compile was skipped is a failure, not 'Build succeeded'") {
    val events: List[E] = List(
      E.SourcegenFinished(scriptMain = "scripts.Gen", success = false, durationMs = 5L, error = Some("script exploded"), timestamp = 1L),
      E.CompileFinished(proj("app"), CompileStatus.Skipped, durationMs = 0L, diagnostics = Nil, skippedBecause = None, timestamp = 2L)
    )
    List(true, false).foreach { verbose =>
      val json = TranscriptFormat.formatCompileResult(events, verbose = verbose, query = None, limit = None, offset = None)
      withClue(s"verbose=$verbose: ") {
        json.hcursor.get[Boolean]("success") shouldBe Right(false)
        json.hcursor.downField("sourcegenFailures").downArray.get[String]("error") shouldBe Right("script exploded")
      }
    }
    val compact = TranscriptFormat.formatCompileResult(events, verbose = false, query = None, limit = None, offset = None)
    compact.hcursor.get[String]("summary").toOption.get should include("Source generation failed")
  }

  test("a clean compile run still reads as a success") {
    val events: List[E] = List(
      E.CompileFinished(proj("app"), CompileStatus.Success, durationMs = 10L, diagnostics = Nil, skippedBecause = None, timestamp = 1L)
    )
    val json = TranscriptFormat.formatCompileResult(events, verbose = false, query = None, limit = None, offset = None)
    json.hcursor.get[Boolean]("success") shouldBe Right(true)
    json.hcursor.get[String]("summary").toOption.get should include("Build succeeded")
  }

  test("a clean run still reads as a pass") {
    val events: List[E] = List(
      E.CompileFinished(proj("app"), CompileStatus.Success, durationMs = 10L, diagnostics = Nil, skippedBecause = None, timestamp = 1L),
      passedTest("app"),
      E.SuiteFinished(proj("app"), SuiteName("OkSuite"), SuiteOutcome.Executed(1, 0, 0, 0), durationMs = 1L, timestamp = 3L)
    )
    val json = formatTest(events)
    json.hcursor.get[Boolean]("success") shouldBe Right(true)
    json.hcursor.get[Int]("passed") shouldBe Right(1)
    json.hcursor.get[String]("summary") shouldBe Right("1 test passed")
  }
}
