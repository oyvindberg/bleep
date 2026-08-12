package bleep.requests

import bleep.BleepException
import bleep.bsp.protocol.{BleepBspProtocol, CompileReason, CompileStatus, DiagnosticSeverity, TestStatus}
import bleep.model.{CrossProjectName, ProjectName, SuiteName, TestName}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

/** The details view is now one implementation shared by `bleep details` (CLI) and `bleep.details` (MCP). These tests pin its contract: the transcript header
  * always rides along, project/query narrowing filters items while summary counts keep describing the full run, and a bad regex fails loudly.
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
    json.hcursor.get[Long]("requestId") shouldBe Right(7L)
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
}
