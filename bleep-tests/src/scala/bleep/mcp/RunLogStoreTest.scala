package bleep.mcp

import bleep.{model, BleepException, BuildPaths}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import java.nio.file.Files
import java.util.concurrent.{Executors, TimeUnit}

/** The run-log store: ids minted before a program starts, a record that is rewritten when it ends, a run whose server died reading as abandoned, and retention
  * that is separate from compile/test history and never evicts a run that is still going.
  */
class RunLogStoreTest extends AnyFunSuite with Matchers {

  private def freshPaths(): BuildPaths = {
    val dir = Files.createTempDirectory("run-log-store").toRealPath()
    BuildPaths(cwd = dir, bleepYamlFile = dir.resolve("bleep.yaml"), variant = model.BuildVariant.Normal, wantedBleepVersion = None)
  }

  private def create(paths: BuildPaths): RunRecord =
    RunLogStore.create(
      paths,
      project = "app",
      script = None,
      sourcegenFor = Nil,
      mainClass = "app.Main",
      args = List("--x"),
      command = "java app.Main --x",
      timeoutSeconds = 60,
      startedAtMs = 1L
    )

  private def exited(paths: BuildPaths, record: RunRecord): Unit =
    RunLogStore.update(paths, record.copy(state = RunState.Exited, pid = Some(42L), exitCode = Some(0), durationMs = Some(5L)))

  test("a run gets its id and directory before it starts, and its record roundtrips through the end-of-run update") {
    val paths = freshPaths()
    val r1 = create(paths)
    val r2 = create(paths)
    r1.id shouldBe 1L
    r2.id shouldBe 2L
    Files.isDirectory(RunLogStore.runDir(paths, 1L)) shouldBe true
    RunLogStore.read(paths, 1L).state shouldBe RunState.Running

    exited(paths, r1)
    val back = RunLogStore.read(paths, 1L)
    back.state shouldBe RunState.Exited
    back.exitCode shouldBe Some(0)
    back.args shouldBe List("--x")
    RunLogStore.readLatest(paths).id shouldBe 2L
  }

  test("run logs live apart from compile/test history") {
    val paths = freshPaths()
    create(paths)
    paths.runLogsDir.getParent shouldBe paths.historyDir.getParent
    Files.exists(paths.historyDir) shouldBe false
  }

  test("a run left running by a server that has died reads as abandoned") {
    val paths = freshPaths()
    val r = create(paths)
    val dead = new ProcessBuilder(javaExe, "-version").start()
    dead.waitFor(60, TimeUnit.SECONDS) shouldBe true
    RunLogStore.update(paths, r.copy(ownerPid = dead.pid()))
    RunLogStore.read(paths, r.id).state shouldBe RunState.Abandoned
  }

  test("retention keeps the newest MaxRuns finished runs, and never evicts one that is still running") {
    val paths = freshPaths()
    val first = create(paths) // left running: owned by this live JVM
    (1 to RunLogStore.MaxRuns + 5).foreach(_ => exited(paths, create(paths)))
    val ids = RunLogStore.list(paths)
    ids should contain(first.id)
    withClue("the newest MaxRuns are kept, and the old run that is still going is kept on top of them: ") {
      ids.size shouldBe RunLogStore.MaxRuns + 1
    }
    ids should not contain 2L
    ids.last shouldBe (RunLogStore.MaxRuns + 6).toLong
  }

  test("concurrent creates never mint the same id") {
    val paths = freshPaths()
    val pool = Executors.newFixedThreadPool(8)
    try {
      val futures = (1 to 32).map(_ => pool.submit(() => create(paths).id))
      futures.map(_.get(60, TimeUnit.SECONDS)).toSet.size shouldBe 32
    } finally pool.shutdown()
  }

  test("updating a run that retention already removed fails loudly") {
    val paths = freshPaths()
    val r = create(paths)
    Files.delete(RunLogStore.runDir(paths, r.id).resolve(RunLogStore.RecordFile))
    Files.delete(RunLogStore.runDir(paths, r.id))
    a[BleepException.Text] should be thrownBy exited(paths, r)
  }

  test("reading a run that does not exist fails loudly") {
    a[BleepException.Text] should be thrownBy RunLogStore.read(freshPaths(), 3L)
    a[BleepException.Text] should be thrownBy RunLogStore.readLatest(freshPaths())
  }

  private def javaExe: String = {
    val exe = if (System.getProperty("os.name").toLowerCase.contains("win")) "java.exe" else "java"
    java.nio.file.Path.of(System.getProperty("java.home")).resolve("bin").resolve(exe).toString
  }
}
