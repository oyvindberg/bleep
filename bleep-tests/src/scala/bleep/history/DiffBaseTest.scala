package bleep.history

import bleep.{model, BleepException, BuildPaths}
import bleep.bsp.protocol.{BleepBspProtocol, CompileReason, CompileStatus}
import bleep.model.{CrossProjectName, ProjectName}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import java.nio.file.Files

/** [[DiffBase]] is the validation gate of `--diff` / MCP `diffBase`: it resolves the base transcript BEFORE the build runs, so these tests pin exactly what
  * fails fast — an unparsable spelling, a missing id, a mode mismatch, no same-mode history — and that "previous" means same-mode, not merely latest.
  */
class DiffBaseTest extends AnyFunSuite with Matchers {

  import BleepBspProtocol.{Event => E}

  private def proj(name: String): CrossProjectName = CrossProjectName(ProjectName(name), crossId = None)

  private def freshPaths(): BuildPaths = {
    val dir = Files.createTempDirectory("diff-base").toRealPath()
    BuildPaths(cwd = dir, bleepYamlFile = dir.resolve("bleep.yaml"), variant = model.BuildVariant.Normal, wantedBleepVersion = None)
  }

  private val someEvents: List[E] = List(
    E.CompilationReason(proj("app"), CompileReason.Incremental, totalFiles = 3, invalidatedFiles = List("A.scala"), changedDependencies = Nil, timestamp = 1L),
    E.CompileFinished(proj("app"), CompileStatus.Success, durationMs = 42L, diagnostics = Nil, skippedBecause = None, timestamp = 2L)
  )

  private def write(paths: BuildPaths, mode: String): Transcript =
    TranscriptStore.write(paths, timestampMs = 1234L, mode = mode, targets = List("app"), client = "test", events = someEvents, testRunResult = None)

  test("parse: numeric historyId, the literal previous, junk fails loudly") {
    DiffBase.parse("42") shouldBe DiffBase.Id(42L)
    DiffBase.parse("previous") shouldBe DiffBase.Previous
    val e = intercept[BleepException.Text](DiffBase.parse("latest"))
    e.getMessage should include("""numeric historyId or the literal "previous"""")
  }

  test("previous means the most recent SAME-MODE entry, skipping newer entries of the other mode") {
    val paths = freshPaths()
    write(paths, mode = "test") // #1
    write(paths, mode = "compile") // #2
    write(paths, mode = "compile") // #3
    withClue("a test --diff right after someone compiled must not trip on the compile entries: ") {
      DiffBase.previous(paths, "test").map(_.id) shouldBe Some(1L)
    }
    DiffBase.previous(paths, "compile").map(_.id) shouldBe Some(3L)
    DiffBase.previous(freshPaths(), "compile") shouldBe None
  }

  test("resolve: an explicit id must exist (the store's text) and must mode-match the command") {
    val paths = freshPaths()
    val compileEntry = write(paths, mode = "compile")

    DiffBase.resolve(paths, "compile", DiffBase.Id(compileEntry.id)).id shouldBe compileEntry.id

    val missing = intercept[BleepException.Text](DiffBase.resolve(paths, "compile", DiffBase.Id(99L)))
    missing.getMessage should include("No history entry #99")

    val mismatch = intercept[BleepException.Text](DiffBase.resolve(paths, "test", DiffBase.Id(compileEntry.id)))
    mismatch.getMessage should include(s"#${compileEntry.id} is a compile run, not a test run")
  }

  test("resolve Previous fails loudly when no same-mode entry exists") {
    val paths = freshPaths()
    write(paths, mode = "compile")
    val e = intercept[BleepException.Text](DiffBase.resolve(paths, "test", DiffBase.Previous))
    e.getMessage should include("No previous test run recorded")
  }
}
