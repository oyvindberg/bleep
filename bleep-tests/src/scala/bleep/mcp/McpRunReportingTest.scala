package bleep.mcp

import bleep.bsp.protocol.{BleepBspProtocol, CompileStatus, DiagnosticSeverity}
import bleep.logs.{LogShape, StreamRecorder}
import bleep.model.{CrossProjectName, ProjectName}
import ch.epfl.scala.bsp4j
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

/** What `bleep.run` tells the agent about the compile in front of it, and about the command it is about to execute.
  *
  * The compile used to go through a BSP client that collected no events, so a failure reported the single sentence "Compilation failed with status ERROR" — the
  * diagnostics had streamed past and been dropped on the floor. What the forked program itself reports is [[SubprocessRunnerTest]].
  */
class McpRunReportingTest extends AnyFunSuite with Matchers {

  import BleepBspProtocol.Event as E

  private def proj(name: String): CrossProjectName = CrossProjectName(ProjectName(name), crossId = None)

  private def diag(message: String, path: String): BleepBspProtocol.Diagnostic =
    BleepBspProtocol.Diagnostic(severity = DiagnosticSeverity.Error, message = message, rendered = None, path = Some(path), line = Some(3), column = Some(1))

  // ------------------------------------------------------------------ command display

  test("the command is shown without its classpath, which is thousands of characters nobody wants") {
    val cp = List("/a/one.jar", "/a/two.jar", "/a/three.jar").mkString(java.io.File.pathSeparator)
    val shown = abbreviateCommand(List("/jvm/bin/java", "-Xmx2g", "-classpath", cp, "com.example.Main", "--flag"))

    shown should include("/jvm/bin/java")
    shown should include("-Xmx2g")
    shown should include("com.example.Main")
    shown should include("--flag")
    shown should include("<3 classpath entries>")
    shown should not include "one.jar"
  }

  test("-cp is collapsed the same as -classpath") {
    abbreviateCommand(List("java", "-cp", "/a/one.jar", "Main")) should include("<1 classpath entries>")
  }

  // ------------------------------------------------------------------ run result

  private def recorded(stream: String, lines: Seq[String]): StreamRecorder.Result = {
    val b = new LogShape.Builder(stream)
    lines.foreach(b.add)
    StreamRecorder.Result(b.result(), bytes = 0L, droppedLines = 0L, droppedBytes = 0L)
  }

  private val record = RunRecord(
    id = 9L,
    workspace = "/ws",
    project = "gen",
    script = Some("generate"),
    sourcegenFor = Nil,
    mainClass = "gen.Main",
    args = Nil,
    command = "java gen.Main",
    timeoutSeconds = 60,
    ownerPid = 1L,
    pid = Some(2L),
    startedAtMs = 0L,
    state = RunState.Exited,
    exitCode = Some(0),
    durationMs = Some(1000L)
  )

  test("a run that printed thousands of lines returns their shape, not the lines, in a bounded result") {
    // The measured cost this replaces: source generators printing ~20k characters of near-identical warnings per run, all of it in the tool result.
    val stdout = recorded("stdout", (1 to 4000).map(i => s"WARNING: PNCompiler: $i unresolved in /src/F$i.sql") :+ "generated 12 files in 340ms")
    val stderr = recorded("stderr", List("WARNING: sun.misc.Unsafe::allocateMemory has been called"))
    val outcome = RunOutcome(Some(0), timedOut = false, 1000L, 2L, stdout, stderr, LogShape.Timing(38000L, Some(("stdout", 812)), 2000L, anyOutput = true))

    val json = BleepMcpServer.runResult(record, outcome)
    json.noSpaces.length should be < 1500
    json.noSpaces should not include "F3999.sql"
    json.hcursor.get[Long]("runId") shouldBe Right(9L)
    withClue("a program started as a script says which script: ") {
      json.hcursor.get[String]("script") shouldBe Right("generate")
    }
    json.hcursor.downField("stdout").get[Int]("lines") shouldBe Right(4001)
    json.hcursor.downField("stderr").get[List[String]]("text") shouldBe Right(List("WARNING: sun.misc.Unsafe::allocateMemory has been called"))
    json.hcursor.get[List[String]]("rareLines").toOption.get.head should include("generated 12 files")
    json.hcursor.downField("timing").get[String]("longestSilenceBefore") shouldBe Right("stdout L812")
    json.hcursor.get[String]("read").toOption.get should include("bleep.run.output")
  }

  test("a run with short output returns it verbatim and no paging hint") {
    val outcome = RunOutcome(
      Some(0),
      timedOut = false,
      5L,
      2L,
      recorded("stdout", List("hello")),
      recorded("stderr", Nil),
      LogShape.Timing(5L, Some(("stdout", 1)), 0L, anyOutput = true)
    )
    val json = BleepMcpServer.runResult(record.copy(script = None), outcome)
    json.hcursor.downField("stdout").get[List[String]]("text") shouldBe Right(List("hello"))
    json.hcursor.get[String]("script").toOption shouldBe None
    json.hcursor.get[String]("read").toOption shouldBe None
  }

  test("bleep.sourcegen reports each script as a recorded run, and fails if any script did") {
    def outcome(exit: Int) =
      RunOutcome(Some(exit), timedOut = false, 5L, 2L, recorded("stdout", List("ok")), recorded("stderr", Nil), LogShape.Timing(5L, None, 0L, anyOutput = true))
    val gen = record.copy(id = 3L, script = None, sourcegenFor = List("a", "b"))
    val other = record.copy(id = 4L, script = None, sourcegenFor = List("c"))

    val json = BleepMcpServer.sourcegenResult(List(gen -> outcome(0), other -> outcome(1)))

    json.hcursor.get[Boolean]("success") shouldBe Right(false)
    val scripts = json.hcursor.downField("scripts")
    scripts.downN(0).get[Long]("runId") shouldBe Right(3L)
    scripts.downN(0).get[List[String]]("sourcegenFor") shouldBe Right(List("a", "b"))
    scripts.downN(1).get[Int]("exitCode") shouldBe Right(1)
    BleepMcpServer.sourcegenResult(List(gen -> outcome(0))).hcursor.get[Boolean]("success") shouldBe Right(true)
  }

  // ------------------------------------------------------------------ compile failure

  test("a failed compile under run raises the diagnostics, not just a status code") {
    val events = List(
      E.CompileFinished(
        proj("app"),
        CompileStatus.Failed,
        durationMs = 42L,
        diagnostics = List(diag("value frobnicate is not a member", "/ws/src/A.scala")),
        skippedBecause = None,
        timestamp = 2L
      )
    )
    val message = BleepMcpServer.compileFailureMessage(events, bsp4j.StatusCode.ERROR, historyId = Some(17L))

    message should include("value frobnicate is not a member")
    message should include("/ws/src/A.scala:3:1")
    message should include("app")
    // A failed tool call returns no result JSON, so the way back to the full diagnostics has to be in the message itself.
    message should include("bleep.history.show")
    message should include("17")
  }

  test("a wall of errors is capped, and says how many it capped") {
    val errors = (1 to 12).toList.map(i => diag(s"error $i", s"/ws/src/F$i.scala"))
    val events = List(E.CompileFinished(proj("app"), CompileStatus.Failed, durationMs = 1L, diagnostics = errors, skippedBecause = None, timestamp = 1L))
    val message = BleepMcpServer.compileFailureMessage(events, bsp4j.StatusCode.ERROR, historyId = None)

    message should include("error 1")
    message should include(s"(+${12 - BleepMcpServer.MaxCompileFailureDiagnostics} more errors)")
    message should not include "error 12"
  }

  test("warnings are not reported as the reason a compile failed") {
    val mixed = List(
      diag("real error", "/ws/src/A.scala"),
      BleepBspProtocol.Diagnostic(DiagnosticSeverity.Warning, "unused import", None, Some("/ws/src/B.scala"), Some(1), Some(1))
    )
    val events = List(E.CompileFinished(proj("app"), CompileStatus.Failed, durationMs = 1L, diagnostics = mixed, skippedBecause = None, timestamp = 1L))
    val message = BleepMcpServer.compileFailureMessage(events, bsp4j.StatusCode.ERROR, historyId = None)

    message should include("real error")
    message should not include "unused import"
  }

  test("a status-only failure still names the status, so the message is never empty") {
    val message = BleepMcpServer.compileFailureMessage(Nil, bsp4j.StatusCode.CANCELLED, historyId = None)
    message should include("CANCELLED")
  }
}
