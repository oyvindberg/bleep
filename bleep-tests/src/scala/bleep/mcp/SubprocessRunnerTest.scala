package bleep.mcp

import bleep.logs.{LogShape, RunLogReader, StreamRecorder}
import cats.effect.IO
import cats.effect.unsafe.implicits.global

import java.nio.file.{Files, Path}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

/** `bleep.run`'s forked program, driven against real processes.
  *
  * The old implementation could not be tested this way and was wrong in ways only a real process shows: it drained both streams into `ByteArrayOutputStream`s,
  * which by construction produce nothing until exit, and on timeout it raised — discarding every byte the program had written. A hung program's output is the
  * whole reason to look at it.
  *
  * The program under test is a forked JVM, not a shell script. That is both what `bleep.run` actually forks and the only portable choice: these first ran
  * `/bin/sh -c`, which exists on no Windows runner, and nine of them died there with `CreateProcess error=2` while passing everywhere else.
  */
class SubprocessRunnerTest extends AnyFunSuite with Matchers with BeforeAndAfterAll {
  import SubprocessRunnerTest.ProbeSource

  private val cwd: Path = Files.createTempDirectory("subprocess-runner")

  private def javaHomeTool(name: String): String = {
    val exe = if (System.getProperty("os.name").toLowerCase.contains("win")) name + ".exe" else name
    Path.of(System.getProperty("java.home")).resolve("bin").resolve(exe).toString
  }

  /** A JVM running the probe program with the given steps. */
  private def probe(steps: String*): List[String] =
    List(javaHomeTool("java"), "-cp", cwd.toString, "Probe") ++ steps

  override def beforeAll(): Unit = {
    val src = cwd.resolve("Probe.java")
    Files.writeString(src, ProbeSource)
    val compile = new ProcessBuilder(javaHomeTool("javac"), src.toString).directory(cwd.toFile).redirectErrorStream(true).start()
    val output = new String(compile.getInputStream.readAllBytes())
    if (compile.waitFor() != 0) throw new RuntimeException(s"could not compile the probe program:\n$output")
  }

  /** Run into a fresh log directory, collecting every heartbeat line the runner emitted. */
  private def runCollecting(cmd: List[String], timeoutSeconds: Int, maxStreamBytes: Long): (RunOutcome, List[String], Path) =
    runCollecting(cmd, sys.env.toList, timeoutSeconds, maxStreamBytes)

  private def runCollecting(cmd: List[String], env: List[(String, String)], timeoutSeconds: Int, maxStreamBytes: Long): (RunOutcome, List[String], Path) = {
    val logDir = Files.createTempDirectory(cwd, "run")
    val seen = new java.util.concurrent.ConcurrentLinkedQueue[String]()
    val outcome =
      SubprocessRunner.run(cmd, cwd, env, timeoutSeconds, logDir, maxStreamBytes, _ => IO.unit, line => IO(seen.add(line)).void).unsafeRunSync()
    (outcome, seen.toArray.toList.map(_.asInstanceOf[String]), logDir)
  }

  private def runCollecting(cmd: List[String], timeoutSeconds: Int): (RunOutcome, List[String]) = {
    val (outcome, heartbeats, _) = runCollecting(cmd, timeoutSeconds, SubprocessRunner.MaxStreamBytes)
    (outcome, heartbeats)
  }

  private def text(result: StreamRecorder.Result): List[String] = result.shape.inline.getOrElse(fail(s"expected verbatim text, got ${result.shape}"))

  /** Wait for the probe to record its pid, i.e. for it to actually be running.
    *
    * Polls for a readable pid rather than for the path: file creation and file content are two events, and waiting only on `Files.exists` read an empty marker
    * on a slow macOS runner and died in `toLong`. The probe writes atomically now, so this should never spin — but a test that can flake in CI is worse than a
    * test that is slightly patient.
    */
  private def awaitPid(marker: Path): Long = {
    val deadline = System.currentTimeMillis() + 30000
    var pid: Option[Long] = None
    while (System.currentTimeMillis() < deadline && pid.isEmpty) {
      pid = if (Files.exists(marker)) Files.readString(marker).trim.toLongOption else None
      if (pid.isEmpty) Thread.sleep(50)
    }
    // A process that never started would make every "it was killed" assertion below pass vacuously — which is exactly
    // how the `/bin/sh` version of these tests reported green on Windows while spawning nothing at all.
    pid.getOrElse(fail(s"the probe never started: $marker never carried a pid"))
  }

  /** Ask the OS whether the process is gone, rather than out-waiting the work it would have done. */
  private def awaitDead(pid: Long): Unit = {
    val deadline = System.currentTimeMillis() + 30000
    while (System.currentTimeMillis() < deadline && ProcessHandle.of(pid).map[Boolean](_.isAlive).orElse(false)) Thread.sleep(50)
    ProcessHandle.of(pid).map[Boolean](_.isAlive).orElse(false) shouldBe false
  }

  test("a program that exits reports its code, its streams and how long it took") {
    val (outcome, _) = runCollecting(probe("out", "to-stdout", "err", "to-stderr", "exit", "3"), timeoutSeconds = 60)

    outcome.exitCode shouldBe Some(3)
    outcome.timedOut shouldBe false
    text(outcome.stdout) shouldBe List("to-stdout")
    text(outcome.stderr) shouldBe List("to-stderr")
    outcome.pid should be > 0L
    outcome.durationMs should be >= 0L
  }

  test("a timed-out program comes back with its output instead of an exception") {
    // This is the case the old implementation threw away: it printed something useful, then hung.
    val (outcome, _) = runCollecting(probe("out", "starting-work", "out", "about-to-hang", "sleep", "300000"), timeoutSeconds = 5)

    outcome.timedOut shouldBe true
    outcome.exitCode shouldBe None
    text(outcome.stdout) shouldBe List("starting-work", "about-to-hang")
    withClue("the hang is visible as the silence after the last line: ") {
      outcome.timing.silentAtEndMs should be >= 4000L
    }
  }

  test("a timed-out program is actually killed, not merely abandoned") {
    val started = cwd.resolve("timeout-started.txt")
    Files.deleteIfExists(started)

    // Records its pid at once, then outlives the timeout many times over.
    val (outcome, _) = runCollecting(probe("touch", started.toString, "sleep", "600000"), timeoutSeconds = 5)

    outcome.timedOut shouldBe true
    val pid = awaitPid(started)
    pid shouldBe outcome.pid
    awaitDead(pid)
  }

  test("progress is reported while the program is still running, not at the end") {
    val (outcome, heartbeats) = runCollecting(probe("out", "first-line", "sleep", "5000"), timeoutSeconds = 60)

    outcome.exitCode shouldBe Some(0)
    heartbeats.size should be >= 2
    heartbeats.exists(_.contains("last: first-line")) shouldBe true
  }

  test("a program that says nothing still proves the runner is watching it") {
    val (_, heartbeats) = runCollecting(probe("sleep", "5000"), timeoutSeconds = 60)

    heartbeats should not be empty
    heartbeats.head should include("no output yet")
  }

  test("a program that never exits and never prints still times out cleanly") {
    val (outcome, heartbeats) = runCollecting(probe("sleep", "300000"), timeoutSeconds = 5)

    outcome.timedOut shouldBe true
    outcome.stdout.shape.lines shouldBe 0
    outcome.timing.anyOutput shouldBe false
    heartbeats should not be empty
  }

  test("a flood is recorded whole on disk and summarized, with every line in the index") {
    val lines = 5000
    val (outcome, _, logDir) = runCollecting(probe("lines", lines.toString), timeoutSeconds = 120, SubprocessRunner.MaxStreamBytes)

    outcome.exitCode shouldBe Some(0)
    outcome.stdout.shape.lines shouldBe lines
    outcome.stdout.shape.inline shouldBe None
    outcome.stdout.shape.patterns.map(_.count) shouldBe List(lines)
    Files.size(logDir.resolve("stdout.log")) shouldBe outcome.stdout.bytes
    Files.size(logDir.resolve("stdout.idx")) shouldBe lines.toLong * StreamRecorder.IndexEntryBytes

    val page = RunLogReader.read(logDir, "stdout", from = Some(-2), to = None, pattern = None)
    page.lines shouldBe List(4999 -> "line-4999", 5000 -> "line-5000")
  }

  test("a program that outproduces the pipe buffer is not deadlocked by it") {
    // The reason each stream needs its own reader thread: a process whose pipe fills up blocks forever if nobody drains it. 64KB is the usual buffer.
    val (outcome, _) = runCollecting(probe("wide", "4000"), timeoutSeconds = 120)

    outcome.timedOut shouldBe false
    outcome.exitCode shouldBe Some(0)
    outcome.stdout.shape.lines shouldBe 4000
  }

  test("colour codes are kept on disk and stripped from what the agent reads") {
    val (outcome, _, logDir) = runCollecting(probe("ansi"), timeoutSeconds = 60, SubprocessRunner.MaxStreamBytes)
    text(outcome.stdout) shouldBe List("red-text")
    Files.readString(logDir.resolve("stdout.log")) should include("\u001b[31m")
    RunLogReader.read(logDir, "stdout", from = None, to = None, pattern = None).lines shouldBe List(1 -> "red-text")
  }

  test("the program sees exactly the environment it is given, not the server's") {
    val env = List("PATH" -> sys.env("PATH"), "BLEEP_RUN_PROBE" -> "from-build")
    val (outcome, _, _) = runCollecting(probe("env", "BLEEP_RUN_PROBE", "env", "HOME"), env, timeoutSeconds = 60, SubprocessRunner.MaxStreamBytes)
    text(outcome.stdout) shouldBe List("BLEEP_RUN_PROBE=from-build", "HOME=null")
  }

  test("a last line without a newline is still a line") {
    val (outcome, _) = runCollecting(probe("out", "first", "print", "no-newline"), timeoutSeconds = 60)
    text(outcome.stdout) shouldBe List("first", "no-newline")
  }

  test("a runaway stream stops being recorded at the cap and counts what it dropped") {
    val (outcome, _, logDir) = runCollecting(probe("lines", "1000"), timeoutSeconds = 60, maxStreamBytes = 1000L)

    Files.size(logDir.resolve("stdout.log")) shouldBe 1000L
    val recorded = outcome.stdout.shape.lines
    (recorded + outcome.stdout.droppedLines) shouldBe 1000L
    outcome.stdout.droppedLines should be > 0L
    LogShape.streamJson(outcome.stdout.shape, outcome.stdout.droppedLines).hcursor.get[Long]("droppedLines") shouldBe Right(outcome.stdout.droppedLines)
  }

  test("cancelling the call kills the program rather than leaking it") {
    val started = cwd.resolve("cancel-started.txt")
    Files.deleteIfExists(started)

    val logDir = Files.createTempDirectory(cwd, "run")
    val program =
      SubprocessRunner.run(
        probe("touch", started.toString, "sleep", "600000"),
        cwd,
        sys.env.toList,
        300,
        logDir,
        SubprocessRunner.MaxStreamBytes,
        _ => IO.unit,
        _ => IO.unit
      )
    val fiber = program.start.unsafeRunSync()
    val pid = awaitPid(started)
    fiber.cancel.unsafeRunSync()

    awaitDead(pid)
  }

  // ------------------------------------------------------------------ reading recorded output

  test("line ranges: positive, negative, capped per call with nextFrom, and filtered by pattern") {
    val (outcome, _, logDir) = runCollecting(probe("mixed", "300"), timeoutSeconds = 60, SubprocessRunner.MaxStreamBytes)
    outcome.stdout.shape.lines shouldBe 600

    val middle = RunLogReader.read(logDir, "stdout", from = Some(10), to = Some(12), pattern = None)
    middle.lines.map(_._1) shouldBe List(10, 11, 12)

    val tail = RunLogReader.read(logDir, "stdout", from = None, to = None, pattern = None)
    tail.lines.map(_._1) shouldBe (581 to 600).toList

    val wide = RunLogReader.read(logDir, "stdout", from = Some(1), to = None, pattern = None)
    wide.lines should have size RunLogReader.MaxLinesPerCall
    wide.nextFrom shouldBe Some(RunLogReader.MaxLinesPerCall + 1)

    val tickPattern = outcome.stdout.shape.patterns.find(_.text.startsWith("tick")).get.id
    val ticks = RunLogReader.read(logDir, "stdout", from = None, to = Some(20), pattern = Some(tickPattern))
    ticks.lines.map(_._2.takeWhile(_ != ' ')).distinct shouldBe List("tick")
    ticks.lines.map(_._1) shouldBe List(1, 3, 5, 7, 9, 11, 13, 15, 17, 19)
  }

  // ------------------------------------------------------------------ heartbeat

  test("the heartbeat carries the program's newest line, which is the part that proves it is alive") {
    // Elapsed time alone cannot separate a program doing work from one deadlocked on a socket. The line it printed four seconds ago can.
    val line = SubprocessRunner.statusLine(elapsedSeconds = 47L, stdoutLines = 1, stderrLines = 0, lastLine = Some("processing batch 14/50"))
    line should include("47s")
    line should include("1 lines stdout")
    line should include("last: processing batch 14/50")
  }

  test("a silent program still produces an honest line rather than a bare elapsed count") {
    val line = SubprocessRunner.statusLine(elapsedSeconds = 12L, stdoutLines = 0, stderrLines = 0, lastLine = None)
    line should include("12s")
    line should include("no output yet")
  }

  test("an enormous last line is abbreviated so one heartbeat cannot flood the transcript") {
    val line = SubprocessRunner.statusLine(elapsedSeconds = 1L, stdoutLines = 1, stderrLines = 0, lastLine = Some("y" * 5000))
    line.length should be < (SubprocessRunner.MaxHeartbeatLineChars + 100)
  }
}

/** The program the tests fork: a tiny argument-driven stand-in for whatever a user's `bleep.run` target does.
  *
  * Kept as Java source compiled at suite start rather than a class on the test classpath, because the forked JVM is given `-cp <tempdir>` and must not depend
  * on how the test runner arranged its own classloaders.
  *
  * Every step flushes. Without that a program that prints and then sleeps would deliver nothing until exit, and the tests asserting that progress arrives
  * mid-run would be measuring the buffer rather than the runner.
  */
object SubprocessRunnerTest {
  private val Esc = "((char) 27)"

  val ProbeSource: String =
    s"""import java.nio.file.*;
       |
       |public class Probe {
       |  public static void main(String[] args) throws Exception {
       |    for (int i = 0; i < args.length; i++) {
       |      switch (args[i]) {
       |        case "out"   -> System.out.println(args[++i]);
       |        case "err"   -> System.err.println(args[++i]);
       |        case "print" -> System.out.print(args[++i]);
       |        case "env"   -> System.out.println(args[i + 1] + "=" + System.getenv(args[++i]));
       |        case "mixed" -> {
       |          int n = Integer.parseInt(args[++i]);
       |          for (int k = 1; k <= n; k++) { System.out.println("tick " + k); System.out.println("tock " + k); }
       |        }
       |        case "ansi"  -> System.out.println($Esc + "[31mred-text" + $Esc + "[0m");
       |        case "sleep" -> Thread.sleep(Long.parseLong(args[++i]));
       |        case "touch" -> {
      |          Path target = Path.of(args[++i]);
      |          Path tmp = Path.of(target + ".tmp");
      |          Files.writeString(tmp, String.valueOf(ProcessHandle.current().pid()));
      |          Files.move(tmp, target, StandardCopyOption.ATOMIC_MOVE);
      |        }
       |        case "exit"  -> { System.out.flush(); System.err.flush(); System.exit(Integer.parseInt(args[++i])); }
       |        case "lines" -> {
       |          int n = Integer.parseInt(args[++i]);
       |          for (int k = 1; k <= n; k++) System.out.println("line-" + k);
       |        }
       |        case "wide" -> {
       |          int n = Integer.parseInt(args[++i]);
       |          String wide = "a".repeat(40);
       |          for (int k = 0; k < n; k++) System.out.println(wide);
       |        }
       |        default -> throw new IllegalArgumentException("unknown step: " + args[i]);
       |      }
       |      System.out.flush();
       |      System.err.flush();
       |    }
       |  }
       |}
       |""".stripMargin
}
