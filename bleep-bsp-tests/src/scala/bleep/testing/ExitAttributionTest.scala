package bleep.testing

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

/** Attribution of a forked JVM's death.
  *
  * Regression for a misdiagnosis that drove a long investigation in the wrong direction. `destroyForcibly` sends SIGKILL, so a fork bleep terminated reports
  * exit 137 identically to one the kernel terminated — and reporting every 137 as "the kernel reclaiming memory under pressure" was confidently wrong for every
  * kill bleep issued itself: start-timeout, pool eviction, contention, cancellation, shutdown. Checked afterwards against the OS log, which had recorded no
  * memory kills at all during a run where 35 forks supposedly died of memory pressure.
  */
class ExitAttributionTest extends AnyFunSuite with Matchers {

  /** Any already-exited process will do — these tests are about how a death is *described*, not about what died. `/bin/sh` does not exist on Windows, so
    * hardcoding it made the whole suite fail there with `CreateProcess error=2`.
    */
  private def deadProcess(): Process = {
    val cmd = if (scala.util.Properties.isWin) List("cmd", "/c", "exit 0") else List("/bin/sh", "-c", "exit 0")
    val p = new ProcessBuilder(cmd*).start()
    p.waitFor()
    p
  }

  test("a kill bleep issued is reported as bleep's, with the reason, and never blamed on the OS") {
    val d = JvmPool.describeExit(deadProcess(), killedByUs = Some("bleep: evicted from pool to free memory for a new fork"))
    d.summary should include("terminated by bleep")
    d.summary should include("evicted from pool")
    d.summary should not include "SIGKILL"
    d.detail.getOrElse("") should include("not the OS")
  }

  test("an unexplained death is described by its exit status, not by a guess at the cause") {
    val d = JvmPool.describeExit(deadProcess(), killedByUs = None)
    d.summary should include("exited 0")
    d.summary should not include "terminated by bleep"
    // The one case where blaming something external is sound must still invite verification rather
    // than assert a cause — asserting it is what cost us a day.
    d.summary should not include "kernel reclaiming"
  }
}

/** Capturing what a fork wrote before it died — the diagnostic that turns "N suites never reported a result" into a reason.
  *
  * The failure that motivated this: a JVM handed an option it rejects prints "Unrecognized VM option ..." to stderr and exits non-zero, but bleep read only the
  * bytes `available()` at the instant the process was seen dead — which is usually zero, because the flush lands a beat later — and dropped the message.
  * Draining an exited fork to EOF is what actually gets it. Tested against real processes (a bare `Process`, no pool, no BSP server), because the bug lived
  * entirely in how the streams of a just-exited process are read.
  */
class DescribeChildOutputTest extends AnyFunSuite with Matchers {

  private def run(cmd: List[String]): Process = {
    val p = new ProcessBuilder(cmd*).start()
    p.waitFor()
    p
  }

  test("an exited fork's stderr is drained to EOF, not just what was already available at exit") {
    // The exact failure this exists for, without depending on any particular JVM: a process that writes to stderr and exits. `available()` reports 0 the instant
    // the process is seen dead, so the old read missed this; draining to EOF gets it. (A real bad-JVM-option failure — "Unrecognized VM option ..." — is this same
    // shape; that end-to-end path, on the project's resolved JVM, is covered by an integration test rather than here.)
    val marker = "STARTUP-FAILURE-MARKER"
    val cmd =
      if (scala.util.Properties.isWin) List("cmd", "/c", s"echo $marker 1>&2 & exit 3")
      else List("/bin/sh", "-c", s"echo $marker 1>&2; exit 3")
    val described = JvmPool.describeChildOutput(run(cmd), exited = true)
    described should include("stderr")
    described should include(marker)
  }

  test("a fork that wrote nothing says so, rather than returning an empty diagnostic") {
    val cmd = if (scala.util.Properties.isWin) List("cmd", "/c", "exit 0") else List("/bin/sh", "-c", "exit 0")
    JvmPool.describeChildOutput(run(cmd), exited = true) should include("no output")
  }
}

/** The start-stagger scales with the run, rather than being a constant tuned on one machine. */
class MaxConcurrentStartsTest extends AnyFunSuite with Matchers {

  test("a quarter of parallelism, floored at two") {
    JvmPool.maxConcurrentStarts(64) shouldBe 16 // a big CI box staggers in wide batches
    JvmPool.maxConcurrentStarts(18) shouldBe 4
    JvmPool.maxConcurrentStarts(8) shouldBe 2
    JvmPool.maxConcurrentStarts(4) shouldBe 2 // a small laptop barely staggers
    JvmPool.maxConcurrentStarts(1) shouldBe 2
  }
}
