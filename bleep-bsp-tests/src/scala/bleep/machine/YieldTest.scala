package bleep.machine

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

/** Design §5.1 as two pure decisions: whether an idle server should go for the lock, and, under it, whether it is the one to yield. */
class YieldTest extends AnyFunSuite with Matchers {
  private val now = 10_000_000L
  private val yieldAfter = 60_000L
  private val need = Some(MemoryNeed.UnderPressure(Pressure.Elevated))
  private def idleFor(ms: Long, connections: Int) = Yield.Idleness(nonObserverConnections = connections, lastActivityEpochMs = now - ms)

  test("idle long enough, nobody connected, nothing scheduled, memory needed: a candidate") {
    Yield.candidate(idleFor(yieldAfter, connections = 0), schedulerIdle = true, need, now, yieldAfter) shouldBe true
  }

  test("a connected IDE blocks yielding, however long since it last did anything — bleep bsp does not reconnect") {
    Yield.candidate(idleFor(yieldAfter * 10, connections = 1), schedulerIdle = true, need, now, yieldAfter) shouldBe false
  }

  test("between MCP calls nothing is connected, so an MCP-started server may yield like any other") {
    Yield.candidate(idleFor(yieldAfter, connections = 0), schedulerIdle = true, need, now, yieldAfter) shouldBe true
  }

  test("not idle long enough, or still scheduling something, or nobody needs the memory: not a candidate") {
    Yield.candidate(idleFor(yieldAfter - 1L, connections = 0), schedulerIdle = true, need, now, yieldAfter) shouldBe false
    Yield.candidate(idleFor(yieldAfter, connections = 0), schedulerIdle = false, need, now, yieldAfter) shouldBe false
    Yield.candidate(idleFor(yieldAfter, connections = 0), schedulerIdle = true, None, now, yieldAfter) shouldBe false
  }

  private def other(pid: Long, idleSince: Option[Long], shuttingDown: Boolean): StateJson =
    StateJson(1, pid, 0L, "x", 0L, requests = 0, cpuInUse = 0, wantsMore = false, shuttingDown = shuttingDown, forks = Nil, idleSinceEpochMs = idleSince)

  test("of two idle servers only the longest idle goes; the other stays for the next tick") {
    val mine = 1_000L
    Yield.goes(mine, myPid = 10L, others = List(other(20L, Some(2_000L), shuttingDown = false))) shouldBe true
    Yield.goes(2_000L, myPid = 20L, others = List(other(10L, Some(mine), shuttingDown = false))) shouldBe false
  }

  test("idle since the same instant, the lower pid goes") {
    Yield.goes(1_000L, myPid = 10L, others = List(other(20L, Some(1_000L), shuttingDown = false))) shouldBe true
    Yield.goes(1_000L, myPid = 20L, others = List(other(10L, Some(1_000L), shuttingDown = false))) shouldBe false
  }

  test("a server already shutting down means nobody else goes this tick") {
    Yield.goes(1_000L, myPid = 10L, others = List(other(20L, Some(5_000L), shuttingDown = true))) shouldBe false
  }

  test("busy servers — no idle time published — never stand in the way") {
    Yield.goes(1_000L, myPid = 10L, others = List(other(20L, None, shuttingDown = false), other(30L, None, shuttingDown = false))) shouldBe true
  }
}
