package bleep.history

import bleep.BleepException
import bleep.bsp.protocol.{BleepBspProtocol, CompileReason, CompileStatus, DiagnosticSeverity, SuiteOutcome, TestStatus}
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
    val json = TranscriptFormat.details(compileTranscript, project = None, query = None, limit = None, offset = None)
    json.hcursor.get[Long]("historyId") shouldBe Right(7L)
    json.hcursor.get[String]("mode") shouldBe Right("compile")
    json.hcursor.get[String]("workspace") shouldBe Right("/ws")
    json.hcursor.get[String]("client") shouldBe Right("bleep")
    json.hcursor.get[List[String]]("targets") shouldBe Right(List("app"))
    json.hcursor.get[Boolean]("success") shouldBe Right(false)
    json.hcursor.get[Int]("totalDiagnostics") shouldBe Right(2)
  }

  test("query narrows the diagnostics array while summary counts keep describing the full run") {
    val json = TranscriptFormat.details(compileTranscript, project = None, query = Some("frobnicate"), limit = None, offset = None)
    json.hcursor.get[Int]("totalDiagnostics") shouldBe Right(1)
    json.hcursor.get[Int]("errors") shouldBe Right(1)
    json.hcursor.get[Int]("warnings") shouldBe Right(1) // full-run count, not narrowed
    json.hcursor.downField("diagnostics").downArray.get[String]("message").toOption.get should include("frobnicate")
  }

  test("an invalid query regex fails loudly") {
    a[BleepException.Text] should be thrownBy
      TranscriptFormat.details(compileTranscript, project = None, query = Some("["), limit = None, offset = None)
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
    val json = TranscriptFormat.details(transcript("test", events), project = Some("app"), query = None, limit = None, offset = None)
    json.hcursor.get[Int]("totalFailures") shouldBe Right(1)
    val failure = json.hcursor.downField("failures").downArray
    failure.get[String]("suite") shouldBe Right("MySuite")
    failure.get[String]("throwable").toOption.get should include("AssertionError")
  }

  // === Everything that can sink a test run before or beside the tests must fail the result. ===
  // The founding incident: a dependency failed to compile, the test project was skipped, zero suites
  // ran — and the run reported `success: true, "0 tests passed"`.

  private def formatTest(events: List[E]): io.circe.Json =
    TranscriptFormat.formatTestResult(events, testRunResult = None, includeThrowables = false, query = None, limit = None, offset = None)

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
    skippedProject.get[String]("project") shouldBe Right("tests")
    skippedProject.get[String]("reason").toOption.get should include("core")
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
