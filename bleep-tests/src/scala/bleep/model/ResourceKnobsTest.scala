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

  test("parallelism defaults to one per core — cores are the default, not a second limit") {
    BspServerConfig.default.effectiveParallelism shouldBe cores
  }

  test("an explicit parallelism replaces the core count everywhere it matters") {
    // The governor's CPU axis is sized from this, not from availableProcessors. Reading cores there
    // instead meant `parallelism = 2` bounded each run and each JVM pool at 2 while the governor
    // still admitted one-per-core across every connected client — so a user who asked for two got two
    // per client and many in total.
    BspServerConfig.default.copy(parallelism = Some(2)).effectiveParallelism shouldBe 2
    BspServerConfig.default.copy(parallelism = Some(64)).effectiveParallelism shouldBe 64
  }

  test("compiles are not capped separately from everything else") {
    // A fixed compile ceiling used to sit inside the governor, holding capacity back for test forks
    // that may not exist — a static partition in a work-conserving system, so a compile-only run left
    // half the machine idle. Heap pressure is answered by HeapPressureGate against the live heap.
    BspServerConfig.getClass.getDeclaredMethods.map(_.getName) should not contain "effectiveMaxConcurrentCompiles"
    classOf[BspServerConfig].getDeclaredFields.map(_.getName) should not contain "maxConcurrentCompiles"
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
