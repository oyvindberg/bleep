package bleep.bsp

import bleep.machine.{LockHolder, LockState, Pressure, TickReport}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import scala.collection.mutable.ListBuffer

/** Tick reports into `metrics.jsonl` at a sane rate: one summed line per second, transitions the moment they happen. */
class SchedulerMetricsTest extends AnyFunSuite with Matchers {
  import SchedulerMetrics.Line

  private def report(nowMs: Long, claimed: Boolean, lock: LockState, hold: List[(String, Long)], spawns: Int, pressure: Pressure): TickReport =
    TickReport(
      nowMs = nowMs,
      claimed = claimed,
      lock = lock,
      holdBreakdownMs = hold,
      spawns = spawns,
      spawnsGuaranteed = math.min(spawns, 1),
      reuses = 0,
      reusesGuaranteed = 0,
      admittedInHeap = 1,
      admittedInHeapGuaranteed = 1,
      evictedNothingToReuse = 0,
      evictedRoomShortage = 0,
      evictedCriticalPressure = 0,
      heapDeferred = 0,
      pressure = Some(pressure),
      liveServers = 2,
      requests = 1,
      forks = spawns,
      cpuInUse = 1 + spawns,
      wantsMore = false
    )

  private def collecting(f: (SchedulerMetrics, ListBuffer[Line]) => Unit): List[Line] = {
    val lines = ListBuffer.empty[Line]
    f(new SchedulerMetrics(lines += _), lines)
    lines.toList
  }

  test("a second's ticks become one line when the next second begins: counts summed, the longest hold kept, steps summed") {
    val lines = collecting { (m, _) =>
      m.tick(report(10_000L, claimed = true, LockState.Held, List("probe" -> 1L, "read" -> 2L, "decide" -> 1L, "write" -> 3L), spawns = 1, Pressure.Normal))
      m.tick(report(10_500L, claimed = true, LockState.Held, List("probe" -> 1L, "read" -> 1L, "decide" -> 20L, "write" -> 1L), spawns = 1, Pressure.Normal))
      m.tick(report(10_900L, claimed = false, LockState.NotNeeded, Nil, spawns = 0, Pressure.Normal))
      m.tick(report(11_000L, claimed = false, LockState.NotNeeded, Nil, spawns = 0, Pressure.Normal))
    }
    lines should have size 1
    val second = lines.head.asInstanceOf[Line.SchedulerSecond]
    second.ticks shouldBe 3
    second.claims shouldBe 2
    second.lockHeld shouldBe 2
    second.spawns shouldBe 2
    second.spawnsGuaranteed shouldBe 2
    second.inHeap shouldBe 3
    second.holdMaxMs shouldBe 23L
    second.holdMs shouldBe Map("probe" -> 2L, "read" -> 3L, "decide" -> 21L, "write" -> 4L)
    second.json should include(""""type":"scheduler"""")
    second.json should include(""""hold_ms":{"decide":21,"probe":2,"read":3,"write":4}""")
  }

  test("a change of pressure level is written the moment it is seen, and only on change") {
    val lines = collecting { (m, _) =>
      m.tick(report(10_000L, claimed = false, LockState.NotNeeded, Nil, spawns = 0, Pressure.Normal))
      m.tick(report(10_100L, claimed = false, LockState.NotNeeded, Nil, spawns = 0, Pressure.Normal))
      m.tick(report(10_200L, claimed = false, LockState.NotNeeded, Nil, spawns = 0, Pressure.Elevated))
      m.tick(report(10_300L, claimed = false, LockState.NotNeeded, Nil, spawns = 0, Pressure.Elevated))
    }
    lines shouldBe List(Line.PressureChanged(10_200L, Pressure.Normal, Pressure.Elevated))
    lines.head.json shouldBe """{"type":"pressure","ts":10200,"from":"normal","to":"elevated"}"""
  }

  test("an unavailable lock is written once per second, and counted for the rest of it") {
    val holder = LockHolder.Announced(pid = 42L, startedAtEpochMs = 1L, heldForMs = 1300L)
    val lines = collecting { (m, _) =>
      m.tick(report(10_000L, claimed = true, LockState.Unavailable(holder), Nil, spawns = 0, Pressure.Normal))
      m.tick(report(10_010L, claimed = true, LockState.Unavailable(holder), Nil, spawns = 0, Pressure.Normal))
      m.quiet(11_000L)
    }
    lines.collect { case l: Line.LockUnavailable => l } shouldBe List(Line.LockUnavailable(10_000L, holder.describe))
    lines.collect { case l: Line.SchedulerSecond => l.lockUnavailable } shouldBe List(2)
  }

  test("going quiet flushes the second in hand, so a burst's last second is not lost; a quiet with nothing held writes nothing") {
    val lines = collecting { (m, _) =>
      m.tick(report(10_000L, claimed = false, LockState.NotNeeded, Nil, spawns = 0, Pressure.Normal))
      m.quiet(13_000L)
      m.quiet(16_000L)
    }
    lines.collect { case l: Line.SchedulerSecond => l.ticks } shouldBe List(1)
  }
}
