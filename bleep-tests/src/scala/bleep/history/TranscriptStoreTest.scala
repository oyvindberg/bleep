package bleep.history

import bleep.{model, BleepException, BuildPaths}
import bleep.bsp.protocol.{BleepBspProtocol, CompileReason, CompileStatus, TestStatus}
import bleep.model.{CrossProjectName, ProjectName, SuiteName, TestName}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import java.nio.file.Files

/** The transcript store is the durable half of the diff story: what the daemon writes per completed run, what `bleep history show` / `bleep history diff` and
  * the MCP tools read. These tests pin the storage contract: ids are per-workspace monotonic and never reused, files roundtrip the full event model, and
  * retention is bounded while eviction only ever eats the oldest.
  */
class TranscriptStoreTest extends AnyFunSuite with Matchers {

  import BleepBspProtocol.{Event => E}

  private def proj(name: String): CrossProjectName = CrossProjectName(ProjectName(name), crossId = None)

  private def freshPaths(): BuildPaths = {
    val dir = Files.createTempDirectory("transcript-store").toRealPath()
    BuildPaths(cwd = dir, bleepYamlFile = dir.resolve("bleep.yaml"), variant = model.BuildVariant.Normal, wantedBleepVersion = None)
  }

  private val someEvents: List[E] = List(
    E.CompilationReason(proj("app"), CompileReason.Incremental, totalFiles = 3, invalidatedFiles = List("A.scala"), changedDependencies = Nil, timestamp = 1L),
    E.CompileFinished(proj("app"), CompileStatus.Success, durationMs = 42L, diagnostics = Nil, skippedBecause = None, timestamp = 2L),
    E.TestFinished(
      proj("app"),
      SuiteName("S"),
      TestName("t"),
      TestStatus.AssumptionFailed,
      durationMs = 7L,
      message = Some("no libgc"),
      throwable = None,
      timestamp = 3L,
      location = None
    )
  )

  private def write(paths: BuildPaths, mode: String = "compile"): Transcript =
    TranscriptStore.write(paths, timestampMs = 1234L, mode = mode, targets = List("app"), client = "test", events = someEvents, testRunResult = None)

  test("ids are per-workspace monotonic starting at 1, and files roundtrip the full event model") {
    val paths = freshPaths()
    val t1 = write(paths)
    val t2 = write(paths, mode = "test")
    t1.id shouldBe 1L
    t2.id shouldBe 2L

    val readBack = TranscriptStore.read(paths, 1L)
    withClue("a transcript must roundtrip through disk byte-exactly at the model level: ") {
      readBack shouldBe t1
    }
    TranscriptStore.readLatest(paths) shouldBe t2
    TranscriptStore.list(paths) shouldBe List(1L, 2L)
  }

  test("two workspaces do not share id sequences") {
    val a = freshPaths()
    val b = freshPaths()
    write(a).id shouldBe 1L
    write(a).id shouldBe 2L
    write(b).id shouldBe 1L
  }

  test("reading a missing id fails loudly and names the retention policy") {
    val paths = freshPaths()
    write(paths)
    val e = intercept[BleepException.Text](TranscriptStore.read(paths, 999L))
    e.getMessage should include("No history entry #999")
    val empty = intercept[BleepException.Text](TranscriptStore.readLatest(freshPaths()))
    empty.getMessage should include("No history recorded")
  }

  test("retention keeps the newest MaxEntries; ids keep increasing past eviction, never reused") {
    val paths = freshPaths()
    (1 to TranscriptStore.MaxEntries + 3).foreach(_ => write(paths))
    val ids = TranscriptStore.list(paths)
    ids should have size TranscriptStore.MaxEntries
    withClue("eviction eats the oldest, keeps the newest: ") {
      ids.head shouldBe 4L
      ids.last shouldBe (TranscriptStore.MaxEntries + 3).toLong
    }
    withClue("the next id continues past evicted history: ") {
      write(paths).id shouldBe (TranscriptStore.MaxEntries + 4).toLong
    }
  }
}
