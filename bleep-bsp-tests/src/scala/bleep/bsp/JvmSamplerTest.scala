package bleep.bsp

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

/** The sampler feeds two consumers that used to disagree by construction: `metrics.jsonl` and, from Phase 1, the `bleep/status` endpoint. These tests pin the
  * shape and — more importantly — the two fields that are allowed to say "unknown", so nobody later "fixes" them into zeros.
  */
class JvmSamplerTest extends AnyFunSuite with Matchers {

  test("samples the running JVM") {
    val stats = JvmSampler.sample()

    stats.heapUsedMb should be > 0L
    stats.heapCommittedMb should be > 0L
    stats.threads should be > 0
    stats.peakThreads should be >= stats.threads
    stats.loadedClasses should be > 0
    withClue("every JVM has at least one collector: ") {
      stats.gc should not be empty
    }
    all(stats.gc.map(_.count)) should be >= 0L
    all(stats.gc.map(_.timeMs)) should be >= 0L
  }

  /** `-1` and `0` mean different things here and both are real answers: `-1` is "this JVM does not report collection usage for its heap pools", while `0` is a
    * genuine measurement — no collection has happened yet, so nothing is known to be retained. A fresh test JVM hits the `0` case. Anything below `-1` would be
    * a bug.
    */
  test("heapLiveMb is either the -1 sentinel or a real post-collection figure") {
    val heapLiveMb = JvmSampler.sample().heapLiveMb
    withClue(s"heapLiveMb=$heapLiveMb: ") {
      heapLiveMb should be >= -1L
    }
  }

  test("openFileDescriptors is None where the platform has no such notion, never Some(0)") {
    JvmSampler.sample().openFileDescriptors.foreach(count => count should be > 0L)
  }

  test("sampling twice does not mutate JVM state — counters only move forward") {
    val first = JvmSampler.sample()
    val second = JvmSampler.sample()

    second.heapMaxMb shouldBe first.heapMaxMb
    second.gc.map(_.name) shouldBe first.gc.map(_.name)
    second.gc.map(_.count).sum should be >= first.gc.map(_.count).sum
    second.peakThreads should be >= first.peakThreads
  }
}
