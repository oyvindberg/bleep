package bleep.machine

import bleep.machine.SchedulerFakes.{withWorld, Effect, World}
import bleep.machine.Ticker.Event
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

/** The tick (design §7), driven one tick at a time with fakes: what a tick touches, in what order, and what it never touches. */
class TickerTest extends AnyFunSuite with Matchers {
  private val r1 = RequestId("r1")
  private val r2 = RequestId("r2")
  private val k = ForkKey("k")

  private def ticker(w: World): Ticker = new Ticker(w.deps(lockWaitMs = 1000L, tickIntervalPerServerMs = 10L))

  private def forkDemand(request: RequestId, task: String, boundMb: Long, shared: Boolean): ForkDemand =
    ForkDemand(request, TaskId(task), ForkKind.TestSuite, k, boundMb, cpu = 1, shared = shared)

  private def compile(request: RequestId, task: String): InHeap = InHeap(request, TaskId(task), InHeapKind.Compile, cpu = 1)

  test("with nothing registered a tick touches nothing: no probe, no lock, no file") {
    withWorld { w =>
      val t = ticker(w)
      t.tick()
      t.tick()
      w.machineProbe.samples.get() shouldBe 0
      w.lock.calls.get() shouldBe 0
      w.ownState shouldBe None
      w.effects.all shouldBe Nil
    }
  }

  test("a compile-only request is admitted without the lock and without reading anyone else's state") {
    withWorld { w =>
      w.otherServer("bbbb", forks = Nil)
      val t = ticker(w)
      t(Event.RegisterRequest(r1, RequestKind.Compile))
      t(Event.SubmitReady(r1, List(compile(r1, "c1")), Map.empty))
      t.tick()
      w.lock.calls.get() shouldBe 0
      w.machineProbe.samples.get() shouldBe 1
      w.effects.all shouldBe List(Effect.StartInHeap(compile(r1, "c1"), guaranteed = true))
      w.ownState.map(s => (s.requests, s.cpuInUse, s.forks)) shouldBe Some((1, 1, Nil))
      t.cadenceMs shouldBe 10L // nobody else was counted, because nobody else was read
    }
  }

  test("a fork demand nothing warm can absorb takes the lock, probes under it, reads every live server and counts their starting forks") {
    withWorld { w =>
      // Ceiling 16384 − 2048 = 14336; used 4096 → 10240 of room; the other server's Starting 9000 leaves 1240.
      w.otherServer("bbbb", forks = List(StateFork(3L, Some(77L), ForkKind.TestBatch, 9000L, StateForkState.Starting, 0L)))
      w.otherServer("cccc", forks = Nil)
      val t = ticker(w)
      t(Event.RegisterRequest(r1, RequestKind.Test))
      t(Event.SubmitReady(r1, List(forkDemand(r1, "t1", boundMb = 1000L, shared = false), forkDemand(r1, "t2", boundMb = 2000L, shared = false)), Map(k -> 2)))
      t.tick()
      w.lock.calls.get() shouldBe 1
      // t1 is the guarantee; t2 (2000) does not fit in the 1240 the other server left.
      w.effects.all shouldBe List(Effect.Spawn(forkDemand(r1, "t1", 1000L, shared = false), ForkId(1), guaranteed = true))
      w.ownState.get.forks shouldBe List(StateFork(1L, None, ForkKind.TestSuite, 1000L, StateForkState.Starting, w.clock.get()))
      w.ownState.get.wantsMore shouldBe true
      t.cadenceMs shouldBe 30L // three live servers
    }
  }

  test("a tick that can reuse a warm fork takes no lock") {
    withWorld { w =>
      val t = ticker(w)
      t(Event.RegisterRequest(r1, RequestKind.Test))
      t(Event.SubmitReady(r1, List(forkDemand(r1, "t1", 1000L, shared = false)), Map(k -> 2)))
      t.tick()
      w.lock.calls.get() shouldBe 1
      t(Event.ForkSpawned(ForkId(1), pid = 500L))
      t(Event.ForkWorkFinished(ForkId(1), cpu = 1))
      t(Event.SubmitReady(r1, List(forkDemand(r1, "t2", 1000L, shared = false)), Map(k -> 1)))
      w.effects.clear()
      t.tick()
      w.lock.calls.get() shouldBe 1
      w.effects.all shouldBe List(Effect.Reuse(forkDemand(r1, "t2", 1000L, shared = false), ForkId(1), guaranteed = true))
      // And a suite joining its busy shared fork takes none either.
      t(Event.SubmitReady(r1, List(forkDemand(r1, "t3", 1000L, shared = true)), Map(k -> 1)))
      t(Event.ForkWorkFinished(ForkId(1), cpu = 1))
      w.effects.clear()
      t.tick()
      w.lock.calls.get() shouldBe 1
      w.effects.all.collect { case e: Effect.Reuse => e.demand.taskId.value } shouldBe List("t3")
    }
  }

  test("a fork is charged its bound until measured one second after start, then remeasured at most once a second") {
    withWorld { w =>
      val t = ticker(w)
      t(Event.RegisterRequest(r1, RequestKind.Test))
      t(Event.SubmitReady(r1, List(forkDemand(r1, "t1", 1000L, shared = false)), Map(k -> 1)))
      val started = w.clock.get()
      t.tick()
      t(Event.ForkSpawned(ForkId(1), pid = 500L))
      w.forkProbe.footprints.set(Map(500L -> 640L))
      w.clock.set(started + 999L)
      t.tick()
      w.forkProbe.calls.get() shouldBe 0
      t.current.forks.head.state shouldBe ForkState.Starting
      w.ownState.get.forks.head.state shouldBe StateForkState.Starting
      w.clock.set(started + 1000L)
      t.tick()
      w.forkProbe.calls.get() shouldBe 1
      t.current.forks.head.state shouldBe ForkState.Measured(640L, started + 1000L)
      w.ownState.get.forks.head.state shouldBe StateForkState.Measured(640L)
      w.clock.set(started + 1500L)
      t.tick()
      w.forkProbe.calls.get() shouldBe 1
      w.clock.set(started + 2000L)
      w.forkProbe.footprints.set(Map(500L -> 700L))
      t.tick()
      w.forkProbe.calls.get() shouldBe 2
      t.current.forks.head.state shouldBe ForkState.Measured(700L, started + 2000L)
      // A fork that exited under the probe keeps its last state; its exit event follows.
      w.forkProbe.footprints.set(Map.empty)
      w.clock.set(started + 3000L)
      t.tick()
      t.current.forks.head.state shouldBe ForkState.Measured(700L, started + 2000L)
    }
  }

  test("an unavailable lock still spawns the guarantee, reads nobody, and is reported") {
    withWorld { w =>
      w.otherServer("bbbb", forks = Nil)
      w.lock.answer.set(LockState.Unavailable("pid 77", 1234L))
      val t = ticker(w)
      t(Event.RegisterRequest(r1, RequestKind.Test))
      t(Event.SubmitReady(r1, List(forkDemand(r1, "t1", 1000L, shared = false), forkDemand(r1, "t2", 1000L, shared = false)), Map(k -> 2)))
      t.tick()
      w.effects.all shouldBe List(
        Effect.Spawn(forkDemand(r1, "t1", 1000L, shared = false), ForkId(1), guaranteed = true),
        Effect.LockUnavailable("pid 77", 1234L)
      )
      t.cadenceMs shouldBe 10L // nobody was read
      w.ownState.get.forks.map(_.id) shouldBe List(1L) // published without the lock: more for the next holder to count, never less
    }
  }

  test("critical pressure evicts idle forks, after the lock is released, through the effects") {
    withWorld { w =>
      val t = ticker(w)
      t(Event.RegisterRequest(r1, RequestKind.Test))
      t(Event.SubmitReady(r1, List(forkDemand(r1, "t1", 1000L, shared = false)), Map(k -> 5)))
      t.tick()
      t(Event.ForkSpawned(ForkId(1), 500L))
      t(Event.ForkWorkFinished(ForkId(1), 1))
      t(Event.SubmitReady(r1, Nil, Map(k -> 4)))
      w.effects.clear()
      t.tick()
      w.effects.all shouldBe Nil // warm: four suites still to come
      w.machineProbe.current.set(MachineSample(16_384L, 15_000L, RawPressure.MacOs(4)))
      t.tick()
      w.effects.all shouldBe List(Effect.Evict(ForkId(1), Decision.EvictReason.CriticalPressure))
      // Still registered, flagged, until the process is gone; not evicted twice meanwhile.
      t.current.forks.map(_.evicting) shouldBe List(true)
      w.effects.clear()
      t.tick()
      w.effects.all shouldBe Nil
      t(Event.ForkExited(ForkId(1)))
      t.current.forks shouldBe Nil
    }
  }

  test("state.json is rewritten only when this server's entry changes") {
    withWorld { w =>
      val t = ticker(w)
      t(Event.RegisterRequest(r1, RequestKind.Compile))
      t(Event.SubmitReady(r1, List(compile(r1, "c1")), Map.empty))
      t.tick()
      val first = w.ownState.get
      w.clock.addAndGet(100L): Unit
      t.tick()
      w.ownState.get.updatedAtEpochMs shouldBe first.updatedAtEpochMs // unchanged: not rewritten
      t(Event.InHeapFinished(r1, TaskId("c1")))
      w.clock.addAndGet(100L): Unit
      t.tick()
      w.ownState.get.updatedAtEpochMs shouldBe first.updatedAtEpochMs + 200L
      w.ownState.get.cpuInUse shouldBe 0
    }
  }

  test("unregistering a request drops its ready set; its idle fork is evicted on the next tick") {
    withWorld { w =>
      val t = ticker(w)
      t(Event.RegisterRequest(r1, RequestKind.Test))
      t(Event.SubmitReady(r1, List(forkDemand(r1, "t1", 1000L, shared = false), forkDemand(r1, "t2", 1000L, shared = false)), Map(k -> 2)))
      t.tick()
      t(Event.ForkSpawned(ForkId(1), 500L))
      t(Event.ForkWorkFinished(ForkId(1), 1))
      t(Event.UnregisterRequest(r1))
      t.current.ready shouldBe Nil
      t.current.unstartedSuitesByKey shouldBe Map.empty
      w.effects.clear()
      t.tick()
      w.effects.all shouldBe List(Effect.Evict(ForkId(1), Decision.EvictReason.NothingToReuseIt))
    }
  }

  test("unstarted suites are summed across requests, so one request's warm fork survives for another's suites") {
    withWorld { w =>
      val t = ticker(w)
      t(Event.RegisterRequest(r1, RequestKind.Test))
      t(Event.RegisterRequest(r2, RequestKind.Test))
      t(Event.SubmitReady(r1, List(forkDemand(r1, "t1", 1000L, shared = false)), Map(k -> 1)))
      t(Event.SubmitReady(r2, Nil, Map(k -> 3)))
      t.tick()
      t(Event.ForkSpawned(ForkId(1), 500L))
      t(Event.ForkWorkFinished(ForkId(1), 1))
      t(Event.SubmitReady(r1, Nil, Map.empty))
      w.effects.clear()
      t.tick()
      w.effects.all shouldBe Nil
      t.current.unstartedSuitesByKey shouldBe Map(k -> 3)
    }
  }

  test("a parallelism change applies to the next tick, and a fork spawned under the old value is not killed") {
    withWorld { w =>
      val t = ticker(w)
      t(Event.RegisterRequest(r1, RequestKind.Compile))
      t(Event.SubmitReady(r1, List(compile(r1, "c1"), compile(r1, "c2"), compile(r1, "c3")), Map.empty))
      t.tick()
      w.effects.all.map(_.asInstanceOf[Effect.StartInHeap].demand.taskId.value) shouldBe List("c1", "c2", "c3")
      w.params.set(w.params.get().copy(parallelism = 1))
      t(Event.SubmitReady(r1, List(compile(r1, "c4")), Map.empty))
      w.effects.clear()
      t.tick()
      w.effects.all shouldBe Nil
      t.current.inHeap.size shouldBe 3
    }
  }

  test("lifecycle events for things the scheduler does not know are bugs") {
    withWorld { w =>
      val t = ticker(w)
      an[IllegalArgumentException] should be thrownBy t(Event.ForkSpawned(ForkId(9), 1L))
      an[IllegalArgumentException] should be thrownBy t(Event.SubmitReady(r1, Nil, Map.empty))
      an[IllegalArgumentException] should be thrownBy t(Event.UnregisterRequest(r1))
      t(Event.RegisterRequest(r1, RequestKind.Test))
      an[IllegalArgumentException] should be thrownBy t(Event.RegisterRequest(r1, RequestKind.Test))
      an[IllegalArgumentException] should be thrownBy t(Event.SubmitReady(r1, List(compile(r2, "x")), Map.empty))
      an[IllegalArgumentException] should be thrownBy t(Event.InHeapFinished(r1, TaskId("never")))
    }
  }

  test("a claiming tick with fakes costs microseconds") {
    withWorld { w =>
      w.otherServer("bbbb", forks = List(StateFork(3L, Some(77L), ForkKind.TestBatch, 100L, StateForkState.Starting, 0L)))
      w.otherServer("cccc", forks = Nil)
      val t = ticker(w)
      t(Event.RegisterRequest(r1, RequestKind.Test))
      // A demand that never fits beyond the guarantee keeps every tick on the claiming path: lock, probe, read two files, decide, (no) write.
      t(
        Event.SubmitReady(
          r1,
          List(forkDemand(r1, "big", boundMb = 1000L, shared = false), forkDemand(r1, "huge", boundMb = 100_000L, shared = false)),
          Map(k -> 2)
        )
      )
      t.tick()
      val ticks = 2000
      val start = System.nanoTime()
      (1 to ticks).foreach(_ => t.tick())
      val perTickMicros = (System.nanoTime() - start) / 1000L / ticks
      info(s"claiming tick with fakes and two other servers' files: ${perTickMicros}µs")
      w.lock.calls.get() shouldBe ticks + 1
      perTickMicros should be < 5000L
    }
  }
}
