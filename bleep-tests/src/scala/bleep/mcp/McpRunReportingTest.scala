package bleep.mcp

import bleep.bsp.protocol.{BleepBspProtocol, CompileStatus, DiagnosticSeverity}
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
