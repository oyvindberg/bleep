package bleep.mcp

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

  /** Run, collecting every heartbeat line the runner emitted. */
  private def runCollecting(cmd: List[String], timeoutSeconds: Int): (RunOutcome, List[String]) = {
    val seen = new java.util.concurrent.ConcurrentLinkedQueue[String]()
    val outcome = SubprocessRunner.run(cmd, cwd, timeoutSeconds, line => IO(seen.add(line)).void).unsafeRunSync()
    (outcome, seen.toArray.toList.map(_.asInstanceOf[String]))
  }

  private def capture(lines: String*): StreamCapture = lines.foldLeft(StreamCapture.empty)(_.append(_))

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
    outcome.stdout.render shouldBe "to-stdout"
    outcome.stderr.render shouldBe "to-stderr"
    outcome.pid should be > 0L
    outcome.durationMs should be >= 0L
  }

  test("a timed-out program comes back with its output instead of an exception") {
    // This is the case the old implementation threw away: it printed something useful, then hung.
    val (outcome, _) = runCollecting(probe("out", "starting-work", "out", "about-to-hang", "sleep", "300000"), timeoutSeconds = 5)

    outcome.timedOut shouldBe true
    outcome.exitCode shouldBe None
    outcome.stdout.render should include("starting-work")
    outcome.stdout.render should include("about-to-hang")
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
    // The whole point of the change: the old code could emit nothing before exit, by construction.
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
    outcome.stdout.totalLines shouldBe 0
    heartbeats should not be empty
  }

  test("a flood of output is bounded, counted, and keeps both ends") {
    val lines = StreamCapture.HeadLines + StreamCapture.TailLines + 1000
    val (outcome, _) = runCollecting(probe("lines", lines.toString), timeoutSeconds = 120)

    outcome.exitCode shouldBe Some(0)
    outcome.stdout.totalLines shouldBe lines
    outcome.stdout.omittedLines shouldBe 1000
    outcome.stdout.render should include("line-1\n")
    outcome.stdout.render should include(s"line-$lines")
    outcome.stdout.countsJson("stdout").map(_._1) should contain("stdoutOmittedLines")
  }

  test("a program that outproduces the pipe buffer is not deadlocked by it") {
    // The reason each stream needs its own reader thread: a process whose pipe fills up blocks forever if nobody drains it. 64KB is the usual buffer.
    val (outcome, _) = runCollecting(probe("wide", "4000"), timeoutSeconds = 120)

    outcome.timedOut shouldBe false
    outcome.exitCode shouldBe Some(0)
    outcome.stdout.totalLines shouldBe 4000
  }

  test("colour codes are stripped, because they are noise to a reader that is not a terminal") {
    val (outcome, _) = runCollecting(probe("ansi"), timeoutSeconds = 60)
    outcome.stdout.render shouldBe "red-text"
  }

  test("cancelling the call kills the program rather than leaking it") {
    val started = cwd.resolve("cancel-started.txt")
    Files.deleteIfExists(started)

    val program = SubprocessRunner.run(probe("touch", started.toString, "sleep", "600000"), cwd, timeoutSeconds = 300, _ => IO.unit)
    val fiber = program.start.unsafeRunSync()
    val pid = awaitPid(started)
    fiber.cancel.unsafeRunSync()

    awaitDead(pid)
  }

  // ------------------------------------------------------------------ output capture

  test("short output survives whole, with nothing elided") {
    val out = capture("first", "second", "third")
    out.render shouldBe "first\nsecond\nthird"
    out.totalLines shouldBe 3
    out.omittedLines shouldBe 0
    out.countsJson("stdout").map(_._1) shouldBe List("stdoutLines")
  }

  test("long output keeps the beginning and the end, and says how much it dropped") {
    val total = StreamCapture.HeadLines + StreamCapture.TailLines + 500
    val out = capture((1 to total).map(i => s"line $i")*)

    out.totalLines shouldBe total
    out.omittedLines shouldBe 500

    val rendered = out.render
    // How it started, and where it got to — the two questions an agent reading a run actually has.
    rendered should include("line 1\n")
    rendered should include(s"line $total")
    rendered should include("[500 lines omitted]")
    // The point of bounding at all: one chatty program must not be able to push everything else out of the agent's context.
    rendered.linesIterator.size shouldBe (StreamCapture.HeadLines + StreamCapture.TailLines + 1)
  }

  test("a single enormous line is truncated rather than admitted whole") {
    // A program printing one megabyte without a newline would otherwise defeat the line bound entirely.
    val out = capture("x" * 100000)
    out.render.length shouldBe StreamCapture.MaxLineChars
    out.render should endWith("…")
  }

  test("lastLine follows the tail once the head is full, and the head while it is not") {
    capture("only").lastLine shouldBe Some("only")
    capture((1 to StreamCapture.HeadLines + 10).map(i => s"line $i")*).lastLine shouldBe Some(s"line ${StreamCapture.HeadLines + 10}")
  }

  // ------------------------------------------------------------------ heartbeat

  test("the heartbeat carries the program's newest line, which is the part that proves it is alive") {
    // Elapsed time alone cannot separate a program doing work from one deadlocked on a socket. The line it printed four seconds ago can.
    val line = SubprocessRunner.statusLine(elapsedSeconds = 47L, stdout = capture("processing batch 14/50"), stderr = StreamCapture.empty)
    line should include("47s")
    line should include("1 lines stdout")
    line should include("last: processing batch 14/50")
  }

  test("stderr wins the last-line slot, because that is where a program in trouble talks") {
    val line = SubprocessRunner.statusLine(elapsedSeconds = 3L, stdout = capture("working"), stderr = capture("connection refused"))
    line should include("last: connection refused")
  }

  test("a silent program still produces an honest line rather than a bare elapsed count") {
    val line = SubprocessRunner.statusLine(elapsedSeconds = 12L, stdout = StreamCapture.empty, stderr = StreamCapture.empty)
    line should include("12s")
    line should include("no output yet")
  }

  test("an enormous last line is abbreviated so one heartbeat cannot flood the transcript") {
    val line = SubprocessRunner.statusLine(elapsedSeconds = 1L, stdout = capture("y" * 5000), stderr = StreamCapture.empty)
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
