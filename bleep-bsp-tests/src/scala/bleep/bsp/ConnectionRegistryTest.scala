package bleep.bsp

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

/** Watching a daemon must never change its life expectancy.
  *
  * Both obvious implementations get this wrong. Counting every connection as a client makes a daemon immortal for as long as `bleep server top` is open;
  * reconnecting per poll refreshes the idle clock on every tick, which is the same bug wearing a hat. So observers are recorded and rendered but excluded from
  * both the liveness count and the idle clock.
  */
class ConnectionRegistryTest extends AnyFunSuite with Matchers {

  test("a fresh client counts as use") {
    val registry = new ConnectionRegistry
    registry.register(1, System.currentTimeMillis())
    registry.nonObserverCount shouldBe 1
  }

  test("an observer does not hold the daemon open") {
    val registry = new ConnectionRegistry
    registry.register(1, System.currentTimeMillis())
    registry.markObserver(1)

    withClue("an observer-only connection must leave the daemon reapable: ") {
      registry.nonObserverCount shouldBe 0
    }
  }

  test("an observer never refreshes the idle clock — neither on connect nor on disconnect") {
    val registry = new ConnectionRegistry
    val start = System.currentTimeMillis()

    // Establish an idle baseline by asking about a moment far in the future.
    val farFuture = start + 600000
    val idleBefore = registry.idleForMs(farFuture)

    registry.register(1, start)
    registry.markObserver(1)
    registry.unregister(1)

    withClue("a poll cycle by an observer must not reset the idle window: ") {
      registry.idleForMs(farFuture) shouldBe idleBefore
    }
  }

  test("a real client does refresh the idle clock on disconnect, so a finished session gets the full window") {
    val registry = new ConnectionRegistry
    val farFuture = System.currentTimeMillis() + 600000
    val idleBefore = registry.idleForMs(farFuture)

    // The registry stamps wall-clock millis, so without this the whole lifecycle can land inside the same millisecond as construction and the clock looks
    // unmoved. Sleeping is the honest fix: the property under test is about time passing.
    Thread.sleep(5)

    registry.register(1, System.currentTimeMillis())
    registry.markClient(1, Some("Metals"), Some("1.0"), Some("/tmp/ws"))
    registry.unregister(1)

    registry.idleForMs(farFuture) should be < idleBefore
  }

  test("a connection that starts as an observer and then initializes becomes a real client") {
    val registry = new ConnectionRegistry
    registry.register(1, System.currentTimeMillis())
    registry.markObserver(1)
    registry.nonObserverCount shouldBe 0

    registry.markClient(1, Some("IntelliJ"), None, Some("/tmp/ws"))
    registry.nonObserverCount shouldBe 1
  }

  test("observers and clients are both rendered, and say which they are") {
    val registry = new ConnectionRegistry
    registry.register(1, 1000L)
    registry.markClient(1, Some("Metals"), Some("1.0"), Some("/tmp/ws"))
    registry.register(2, 2000L)
    registry.markObserver(2)

    val snapshot = registry.snapshot
    snapshot.map(_.connId) shouldBe List(1, 2)
    snapshot.head.observer shouldBe false
    snapshot.head.clientName shouldBe Some("Metals")
    snapshot(1).observer shouldBe true
  }

  test("unregistering a connection that was never registered is harmless") {
    val registry = new ConnectionRegistry
    registry.unregister(42)
    registry.nonObserverCount shouldBe 0
    registry.snapshot shouldBe empty
  }
}
