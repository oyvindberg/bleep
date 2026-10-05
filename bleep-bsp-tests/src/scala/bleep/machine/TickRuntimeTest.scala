package bleep.machine

import bleep.machine.SchedulerFakes.{eventually, withWorld, Effect}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import ryddig.TypedLogger

/** The scheduler's thread (design §7): a dedicated thread, woken by events, coalescing them, idle when there is nothing to schedule, and loud when it dies. */
class TickRuntimeTest extends AnyFunSuite with Matchers {
  private val r1 = RequestId("r1")
  private val k = ForkKey("k")

  private def demand(task: String): ForkDemand = ForkDemand(r1, TaskId(task), ForkKind.TestSuite, k, boundMb = 1000L, cpu = 1, shared = false)

  test("an event from another thread wakes the dedicated thread, which decides and reports through the effects") {
    withWorld { w =>
      val runtime = new TickRuntime(w.deps(lockWaitMs = 100L, tickIntervalPerServerMs = 10L), TypedLogger.DevNull, t => fail(s"scheduler died: $t"))
      runtime.start()
      try {
        runtime.registerRequest(r1, RequestKind.Test)
        runtime.submitReady(r1, List(demand("t1")), Map(k -> 1))
        eventually(5000L)(w.effects.all.nonEmpty) shouldBe true
        w.effects.all shouldBe List(Effect.Spawn(demand("t1"), ForkId(1), guaranteed = true))
        w.ownState.map(_.forks.map(_.id)) shouldBe Some(List(1L))
        val threads = Thread.getAllStackTraces.keySet().toArray(Array.empty[Thread]).map(_.getName).toList
        threads should contain(TickRuntime.ThreadName)
        threads.find(_ == TickRuntime.ThreadName).get should not include "io-compute"
      } finally runtime.close()
    }
  }

  test("with nothing to schedule the thread runs only the slow check — one probe, no lock, no decision; with a request it ticks at the cadence") {
    withWorld { w =>
      val runtime = new TickRuntime(w.deps(lockWaitMs = 100L, tickIntervalPerServerMs = 10L), TypedLogger.DevNull, t => fail(s"scheduler died: $t"))
      runtime.start()
      try {
        Thread.sleep(100L)
        // The fake clock stands still, so the slow check (design §5.1) is due exactly once however long the thread waits.
        w.machineProbe.samples.get() shouldBe 1
        w.lock.calls.get() shouldBe 0
        w.ownState.map(_.requests) shouldBe Some(0) // the idle record, not a decision
        w.machineProbe.samples.set(0)
        runtime.registerRequest(r1, RequestKind.Compile)
        runtime.submitReady(r1, List(InHeap(r1, TaskId("c1"), InHeapKind.Compile, 1)), Map.empty)
        Thread.sleep(200L)
        val ticks = w.machineProbe.samples.get()
        // ~10 ms cadence over 200 ms: several, and well below one per millisecond. The lower bound is loose on purpose — a loaded CI runner has been seen to
        // schedule only a handful of parks in that window — while the upper bound is what the test is for: an idle-wait that spins would tick thousands of times.
        ticks should be >= 2
        ticks should be < 200
        w.lock.calls.get() shouldBe 0 // a compile never takes the lock, however often it ticks
      } finally runtime.close()
    }
  }

  test("a burst of events while a tick is in progress coalesces into one following tick") {
    withWorld { w =>
      w.machineProbe.delayMs.set(80L)
      val runtime = new TickRuntime(w.deps(lockWaitMs = 100L, tickIntervalPerServerMs = 1000L), TypedLogger.DevNull, t => fail(s"scheduler died: $t"))
      runtime.start()
      try {
        runtime.registerRequest(r1, RequestKind.Compile)
        eventually(2000L)(w.machineProbe.samples.get() >= 1) shouldBe true // the first tick is now sleeping in the probe
        (1 to 50).foreach(i => runtime.submitReady(r1, List(InHeap(r1, TaskId(s"c$i"), InHeapKind.Discover, 1)), Map.empty))
        Thread.sleep(300L)
        // One tick was running, one more absorbed all fifty events; the cadence (1 s) has not come round.
        w.machineProbe.samples.get() should be <= 3
        w.effects.all.collect { case e: Effect.StartInHeap => e.demand.taskId.value } shouldBe List("c50")
      } finally runtime.close()
    }
  }

  test("a probe that fails stops the runtime, and every later call says so with the cause") {
    withWorld { w =>
      val death = new java.util.concurrent.atomic.AtomicReference[Throwable](null)
      val runtime = new TickRuntime(w.deps(lockWaitMs = 100L, tickIntervalPerServerMs = 10L), TypedLogger.DevNull, death.set)
      runtime.start()
      try {
        w.machineProbe.failWith.set(new IllegalStateException("host_statistics64 failed"))
        runtime.registerRequest(r1, RequestKind.Compile)
        eventually(5000L)(runtime.failed.isDefined) shouldBe true
        runtime.failed.get.getMessage shouldBe "host_statistics64 failed"
        // The server's signal: it must shut down on this.
        eventually(5000L)(death.get() != null) shouldBe true
        death.get().getMessage shouldBe "host_statistics64 failed"
        val refused = intercept[IllegalStateException](runtime.unregisterRequest(r1))
        refused.getCause.getMessage shouldBe "host_statistics64 failed"
      } finally runtime.close()
    }
  }

  test("an unconstrained runtime says so once at start, before anything is scheduled") {
    withWorld { w =>
      val runtime = new TickRuntime(
        w.depsUnconstrained("no probe library for this platform", tickIntervalPerServerMs = 10L),
        TypedLogger.DevNull,
        t => fail(s"scheduler died: $t")
      )
      runtime.start()
      try w.effects.all shouldBe List(Effect.SchedulingUnconstrained("no probe library for this platform"))
      finally runtime.close()
    }
  }

  test("close stops the thread; calls after it are refused") {
    withWorld { w =>
      val runtime = new TickRuntime(w.deps(lockWaitMs = 100L, tickIntervalPerServerMs = 10L), TypedLogger.DevNull, t => fail(s"scheduler died: $t"))
      runtime.start()
      runtime.close()
      runtime.close() // idempotent
      an[IllegalStateException] should be thrownBy runtime.registerRequest(r1, RequestKind.Test)
      runtime.isRunning shouldBe false
    }
  }
}
