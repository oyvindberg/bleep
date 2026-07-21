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
