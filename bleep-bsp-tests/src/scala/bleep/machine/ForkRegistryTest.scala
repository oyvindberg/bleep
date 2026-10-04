package bleep.machine

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

/** The daemon-wide register of live forks (design §10 step 10). */
class ForkRegistryTest extends AnyFunSuite with Matchers {
  private def fork(pid: Long, label: String, startedAt: Long): ForkRegistry.LiveFork =
    ForkRegistry.LiveFork(pid = pid, label = label, key = "k", heapBoundMb = Some(512L), startedAtEpochMs = startedAt, kill = _ => ())

  test("forks are listed oldest first while registered, and gone once unregistered") {
    val r = new ForkRegistry
    r.register(fork(2L, "b", 20L))
    r.register(fork(1L, "a", 10L))
    r.live.map(_.label) shouldBe List("a", "b")
    r.size shouldBe 2
    r.unregister(1L) shouldBe true
    r.live.map(_.label) shouldBe List("b")
  }

  test("registering a pid twice is a bug; unregistering an unknown pid is merely late") {
    val r = new ForkRegistry
    r.register(fork(7L, "first", 1L))
    val e = intercept[IllegalStateException](r.register(fork(7L, "second", 2L)))
    e.getMessage should include("first")
    r.unregister(7L) shouldBe true
    r.unregister(7L) shouldBe false
    r.size shouldBe 0
  }

  test("a registered fork can be killed through its entry") {
    val r = new ForkRegistry
    var killedFor: Option[String] = None
    r.register(ForkRegistry.LiveFork(3L, "x", "k", None, 0L, reason => killedFor = Some(reason)))
    r.live.head.kill("bleep: test")
    killedFor shouldBe Some("bleep: test")
  }
}
