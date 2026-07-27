package bleep.model

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

/** The concurrency knob and what derives from it.
  *
  * There is one — `parallelism` — because the previous surface had a separate `maxConcurrentCompiles`, so "how much of this machine may bleep use" had two
  * answers that could disagree. It is machine-wide and read by the shared compile server at startup, which is why it lives in config and not in the
  * environment: a per-client override would let whichever client spawned the daemon configure it for everyone else.
  */
class ResourceKnobsTest extends AnyFunSuite with Matchers {

  private val cores = Runtime.getRuntime.availableProcessors

  test("parallelism defaults to one per core, and the compile ceiling to half of that") {
    val c = BspServerConfig.default
    c.effectiveParallelism shouldBe cores
    c.effectiveMaxConcurrentCompiles shouldBe math.max(1, cores / 2)
  }

  test("the compile ceiling follows parallelism, so there is one number to reason about") {
    BspServerConfig.default.copy(parallelism = Some(8)).effectiveMaxConcurrentCompiles shouldBe 4
    BspServerConfig.default.copy(parallelism = Some(2)).effectiveMaxConcurrentCompiles shouldBe 1
    // Never zero: a ceiling of zero would deadlock every build.
    BspServerConfig.default.copy(parallelism = Some(1)).effectiveMaxConcurrentCompiles shouldBe 1
  }

  test("parallelismRatio expresses the same thing as a fraction of cores") {
    BspServerConfig.default.copy(parallelismRatio = Some(0.5)).effectiveParallelism shouldBe math.max(1, cores / 2)
  }

  test("an explicit parallelism beats the ratio") {
    BspServerConfig.default.copy(parallelism = Some(3), parallelismRatio = Some(0.5)).effectiveParallelism shouldBe 3
  }

  test("no resource knob is settable from the environment") {
    // Regression guard for a design mistake worth remembering: env overrides were added for these,
    // and one `BLEEP_PARALLELISM=3 bleep compile` pinned the value into the shared daemon, which
    // then served every other client — and every fork it spawned — at that concurrency. Anything the
    // daemon reads has to be machine-wide, because the daemon is.
    val fields = classOf[BspServerConfig].getDeclaredFields.map(_.getName).toSet
    fields should contain("parallelism")
    BspServerConfig.getClass.getDeclaredMethods.map(_.getName) should not contain "envString"
    BspServerConfig.getClass.getDeclaredMethods.map(_.getName) should not contain "envInt"
  }
}
