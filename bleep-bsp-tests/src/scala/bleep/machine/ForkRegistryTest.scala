package bleep.machine

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

/** The daemon-wide register of live forks, by the scheduler's fork id (design §10 step 10). */
class ForkRegistryTest extends AnyFunSuite with Matchers {
  private def fork(id: Long, label: String, startedAt: Long): ForkRegistry.LiveFork =
    ForkRegistry.LiveFork(id = ForkId(id), pid = 1000L + id, label = label, key = ForkKey("k"), startedAtEpochMs = startedAt, kill = _ => ())

  test("forks are listed oldest first while registered, found by id, and gone once unregistered") {
    val r = new ForkRegistry
    r.register(fork(2L, "b", 20L))
    r.register(fork(1L, "a", 10L))
    r.live.map(_.label) shouldBe List("a", "b")
    r.get(ForkId(2L)).map(_.label) shouldBe Some("b")
    r.size shouldBe 2
    r.unregister(ForkId(1L)) shouldBe true
    r.live.map(_.label) shouldBe List("b")
    r.get(ForkId(1L)) shouldBe None
  }

  test("registering an id twice is a bug; unregistering an unknown id is merely late") {
    val r = new ForkRegistry
    r.register(fork(7L, "first", 1L))
    val e = intercept[IllegalStateException](r.register(fork(7L, "second", 2L)))
    e.getMessage should include("first")
    r.unregister(ForkId(7L)) shouldBe true
    r.unregister(ForkId(7L)) shouldBe false
    r.size shouldBe 0
  }

  test("a registered fork can be killed through its entry") {
    val r = new ForkRegistry
    var killedFor: Option[String] = None
    r.register(ForkRegistry.LiveFork(ForkId(3L), 3000L, "x", ForkKey("k"), 0L, reason => killedFor = Some(reason)))
    r.get(ForkId(3L)).get.kill("bleep: test")
    killedFor shouldBe Some("bleep: test")
  }
}
