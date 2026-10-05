package bleep.machine

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

/** The one trigger behind shedding and yielding (design §5.1, §5.2): the OS reclaiming, or another server waiting. Neither alone is "memory is low". */
class MemoryNeedTest extends AnyFunSuite with Matchers {
  private def server(pid: Long, wantsMore: Boolean): StateJson =
    StateJson(1, pid, 0L, "x", 0L, requests = 1, cpuInUse = 1, wantsMore = wantsMore, shuttingDown = false, forks = Nil, idleSinceEpochMs = None)

  test("pressure at or above elevated is a need, whoever is waiting") {
    MemoryNeed.of(Pressure.Elevated, Nil) shouldBe Some(MemoryNeed.UnderPressure(Pressure.Elevated))
    MemoryNeed.of(Pressure.Critical, List(server(5L, wantsMore = true))) shouldBe Some(MemoryNeed.UnderPressure(Pressure.Critical))
  }

  test("another server's wantsMore is a need, named by pid in order") {
    MemoryNeed.of(Pressure.Normal, List(server(9L, wantsMore = true), server(4L, wantsMore = false), server(2L, wantsMore = true))) shouldBe
      Some(MemoryNeed.OthersWantMore(List(2L, 9L)))
  }

  test("normal pressure with nobody waiting is no need — and so is a missing pressure signal") {
    MemoryNeed.of(Pressure.Normal, List(server(4L, wantsMore = false))) shouldBe None
    MemoryNeed.of(Pressure.NoSignal("no sysctl"), Nil) shouldBe None
  }

  test("the need says what it is, for the log") {
    MemoryNeed.UnderPressure(Pressure.Elevated).describe shouldBe "memory pressure is elevated"
    MemoryNeed.OthersWantMore(List(7L)).describe shouldBe "server 7 wants more memory"
    MemoryNeed.OthersWantMore(List(7L, 8L)).describe shouldBe "servers 7, 8 want more memory"
  }
}
