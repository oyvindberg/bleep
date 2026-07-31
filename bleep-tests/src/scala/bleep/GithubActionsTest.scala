package bleep

import bleep.bsp.protocol.{BleepBspProtocol, DiagnosticSeverity}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import java.nio.file.{Files, Path}

class GithubActionsTest extends AnyFunSuite with Matchers {

  private def diag(path: Option[String], line: Option[Int], column: Option[Int]): BleepBspProtocol.Diagnostic =
    BleepBspProtocol.Diagnostic(DiagnosticSeverity.Error, "boom", None, path, line, column)

  test("detects the runner from GITHUB_ACTIONS, and stays silent everywhere else") {
    GithubActions.fromEnv(Map("GITHUB_ACTIONS" -> "true")).annotationsEnabled shouldBe true
    // A local shell that merely has the variable set to something else is not a runner.
    GithubActions.fromEnv(Map("GITHUB_ACTIONS" -> "false")).annotationsEnabled shouldBe false
    GithubActions.fromEnv(Map.empty).annotationsEnabled shouldBe false
    GithubActions.disabled.annotationsEnabled shouldBe false
  }

  test("step summary file is only picked up when the runner actually provided one") {
    GithubActions.fromEnv(Map("GITHUB_STEP_SUMMARY" -> "")).stepSummaryFile shouldBe None
    GithubActions.fromEnv(Map.empty).stepSummaryFile shouldBe None
    GithubActions.fromEnv(Map("GITHUB_STEP_SUMMARY" -> "/tmp/summary.md")).stepSummaryFile shouldBe Some(Path.of("/tmp/summary.md"))
  }

  test("workflow command data escapes what would otherwise truncate the message") {
    // A raw newline ends the command, so a multi-line compiler message would lose everything after the first line.
    GithubActions.escapeData("a\nb") shouldBe "a%0Ab"
    GithubActions.escapeData("a\r\nb") shouldBe "a%0D%0Ab"
    // '%' first, or the escapes of the other characters would themselves get re-escaped.
    GithubActions.escapeData("100%") shouldBe "100%25"
    GithubActions.escapeData("%0A") shouldBe "%250A"
  }

  test("property values additionally escape the k=v,k=v delimiters") {
    // Windows paths carry a drive colon, which would otherwise terminate the property list.
    GithubActions.escapeProperty("C:/foo/Bar.scala") shouldBe "C%3A/foo/Bar.scala"
    GithubActions.escapeProperty("a,b") shouldBe "a%2Cb"
  }

  test("annotation paths are repo-relative, and files outside the build get none") {
    val build = Files.createTempDirectory("gha-build")
    val inside = build.resolve("bleep-core/src/scala/bleep/Foo.scala")

    GithubActions.relativize(build, inside.toString) shouldBe Some("bleep-core/src/scala/bleep/Foo.scala")

    // An absolute path outside the workspace cannot attach to a line in the diff. Better to drop the position than to
    // point the annotation at a file the PR does not contain.
    GithubActions.relativize(build, Files.createTempDirectory("gha-other").resolve("Other.scala").toString) shouldBe None
  }

  test("annotation budget is spent worst-first") {
    val positionedError = diag(Some("/x/A.scala"), Some(1), Some(1))
    val unpositionedError = diag(None, None, None)
    val warning = positionedError.copy(severity = DiagnosticSeverity.Warning)

    // errors before warnings, and within a severity, diagnostics that can actually appear inline first
    List(warning, unpositionedError, positionedError).sortBy(GithubActions.rank) shouldBe
      List(positionedError, unpositionedError, warning)
  }

  test("displayPath reassembles only the parts the compiler reported") {
    diag(Some("/x/A.scala"), Some(12), Some(5)).displayPath shouldBe Some("/x/A.scala:12:5")
    diag(Some("/x/A.scala"), Some(12), None).displayPath shouldBe Some("/x/A.scala:12")
    diag(Some("/x/A.scala"), None, None).displayPath shouldBe Some("/x/A.scala")
    diag(None, None, None).displayPath shouldBe None
  }

  test("appendStepSummary accumulates across calls") {
    val file = Files.createTempFile("summary", ".md")
    val gha = GithubActions(annotationsEnabled = false, stepSummaryFile = Some(file))
    gha.appendStepSummary("## one")
    gha.appendStepSummary("## two\n")
    Files.readString(file) shouldBe "## one\n## two\n"
  }
}
