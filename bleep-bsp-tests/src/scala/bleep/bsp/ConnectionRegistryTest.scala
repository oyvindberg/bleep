package bleep.bsp

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

/** Watching a daemon must never change its life expectancy.
  *
  * Both obvious implementations get this wrong. Counting every connection as a client makes a daemon immortal for as long as `bleep server top` is open;
  * reconnecting per poll refreshes the idle clock on every tick, which is the same bug wearing a hat. So observers are recorded and rendered but excluded from
  * both the liveness count and the idle clock.
  *
  * The registry takes its clock as a parameter, so these tests state exactly when time passes instead of sleeping and hoping.
  */
class ConnectionRegistryTest extends AnyFunSuite with Matchers {

  /** Time only moves when a test says so. */
  private class FakeClock(start: Long) {
    private var current = start
    val read: () => Long = () => current
    def advance(ms: Long): Unit = current += ms
    def nowPlus(ms: Long): Long = current + ms
  }

  private def fixture(): (FakeClock, ConnectionRegistry) = {
    val clock = new FakeClock(1_000_000L)
    (clock, new ConnectionRegistry(clock.read))
  }

  test("a fresh client counts as use") {
    val (_, registry) = fixture()
    registry.register(1, 1_000_000L)
    registry.nonObserverCount shouldBe 1
  }

  test("an observer does not hold the daemon open") {
    val (_, registry) = fixture()
    registry.register(1, 1_000_000L)
    registry.markObserver(1)

    withClue("an observer-only connection must leave the daemon reapable: ") {
      registry.nonObserverCount shouldBe 0
    }
  }

  test("an observer never refreshes the idle clock — neither on connect nor on disconnect") {
    val (clock, registry) = fixture()
    val idleBefore = registry.idleForMs(clock.nowPlus(600_000L))

    clock.advance(30_000L)
    registry.register(1, clock.read())
    registry.markObserver(1)
    registry.unregister(1)

    withClue("a poll cycle by an observer must not reset the idle window: ") {
      // Measured from the same wall-clock instant as `idleBefore`, so any change is the registry's doing rather than time passing.
      registry.idleForMs(clock.nowPlus(570_000L)) shouldBe idleBefore
    }
  }

  test("a real client does refresh the idle clock on disconnect, so a finished session gets the full window") {
    val (clock, registry) = fixture()
    val idleBefore = registry.idleForMs(clock.nowPlus(600_000L))

    clock.advance(30_000L)
    registry.register(1, clock.read())
    registry.markClient(1, Some("Metals"), Some("1.0"), Some("/tmp/ws"))
    registry.unregister(1)

    registry.idleForMs(clock.nowPlus(570_000L)) shouldBe (idleBefore - 30_000L)
  }

  test("an idle daemon's clock keeps running while nothing connects") {
    val (clock, registry) = fixture()
    clock.advance(45_000L)
    registry.idleForMs(clock.read()) shouldBe 45_000L
  }

  test("a connection that starts as an observer and then initializes becomes a real client") {
    val (_, registry) = fixture()
    registry.register(1, 1_000_000L)
    registry.markObserver(1)
    registry.nonObserverCount shouldBe 0

    registry.markClient(1, Some("IntelliJ"), None, Some("/tmp/ws"))
    registry.nonObserverCount shouldBe 1
  }

  test("observers and clients are both rendered, and say which they are") {
    val (_, registry) = fixture()
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
    val (_, registry) = fixture()
    registry.unregister(42)
    registry.nonObserverCount shouldBe 0
    registry.snapshot shouldBe empty
  }
}
