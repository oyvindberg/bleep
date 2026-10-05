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

  test("with nothing registered a tick decides nothing and takes no lock; the slow check runs once per interval and publishes an idle record") {
    withWorld { w =>
      val t = ticker(w)
      t.tick()
      t.tick()
      w.machineProbe.samples.get() shouldBe 1 // the slow check (design §5.1), once per interval however many ticks
      w.lock.calls.get() shouldBe 0
      // Nothing scheduled, and no idle-since: the fake registry has a client connected, so this server is in use even though it schedules nothing.
      w.ownState.map(s => (s.requests, s.forks, s.idleSinceEpochMs, s.shuttingDown)) shouldBe Some((0, Nil, None, false))
      w.effects.all shouldBe Nil
      w.clock.addAndGet(w.slowCheckIntervalMs): Unit
      t.tick()
      w.machineProbe.samples.get() shouldBe 2
      w.lock.calls.get() shouldBe 0
    }
  }

  test("under elevated pressure an idle server sheds its idle workspaces' caches, once per slow-check interval while it lasts") {
    withWorld { w =>
      w.machineProbe.current.set(
        MachineSample(
          physicalMb = 16_384L,
          usedMb = 14_000L,
          availableMb = 2_384L,
          roomBasis = RoomBasis.StartingForksCap,
          pressure = RawPressure.MacOs(2, 0L, 0L, 0L, 0L)
        )
      )
      val t = ticker(w)
      t.tick()
      t.tick()
      w.effects.all shouldBe List(Effect.ShedIdleCaches(MemoryNeed.UnderPressure(Pressure.Elevated)))
      w.clock.addAndGet(w.slowCheckIntervalMs): Unit
      t.tick()
      w.effects.all.size shouldBe 2
      // Pressure gone, nobody waiting: nothing more is shed.
      w.machineProbe.current.set(
        MachineSample(
          physicalMb = 16_384L,
          usedMb = 4_096L,
          availableMb = 12_288L,
          roomBasis = RoomBasis.StartingForksCap,
          pressure = RawPressure.MacOs(1, 0L, 0L, 0L, 0L)
        )
      )
      w.clock.addAndGet(w.slowCheckIntervalMs): Unit
      t.tick()
      w.effects.all.size shouldBe 2
    }
  }

  test("another server's wantsMore, read without the lock, makes a server shed; low memory with nobody waiting does not") {
    withWorld { w =>
      w.otherServer("bbbb", forks = Nil, wantsMore = false)
      val t = ticker(w)
      t.tick()
      w.effects.all shouldBe Nil
      w.lock.calls.get() shouldBe 0
      w.otherServer("bbbb", forks = Nil, wantsMore = true)
      w.clock.addAndGet(w.slowCheckIntervalMs): Unit
      t.tick()
      w.lock.calls.get() shouldBe 0 // read without the lock: shedding releases memory, it never claims it
      w.effects.all shouldBe List(Effect.ShedIdleCaches(MemoryNeed.OthersWantMore(List(StateFile.selfIdentity("other").pid))))
    }
  }

  test("a busy server sheds on the same trigger, from the others it read under the lock") {
    withWorld { w =>
      w.otherServer("bbbb", forks = Nil, wantsMore = true)
      val t = ticker(w)
      t(Event.RegisterRequest(r1, RequestKind.Test))
      t(Event.SubmitReady(r1, List(forkDemand(r1, "t1", boundMb = 1000L, shared = false)), Map(k -> 1)))
      t.tick()
      w.lock.calls.get() shouldBe 1
      w.effects.all should contain(Effect.ShedIdleCaches(MemoryNeed.OthersWantMore(List(StateFile.selfIdentity("other").pid))))
    }
  }

  test("an idle server publishes since when it is idle, and yields under the lock when idle long enough and someone wants more") {
    withWorld { w =>
      w.otherServer("bbbb", forks = Nil, wantsMore = true)
      val lastActivity = w.clock.get()
      w.idleness.set(Yield.Idleness(nonObserverConnections = 0, lastActivityEpochMs = lastActivity))
      val t = ticker(w)
      t.tick()
      // Not idle for long enough yet: published as idle, lock untouched.
      w.ownState.map(s => (s.idleSinceEpochMs, s.shuttingDown)) shouldBe Some((Some(lastActivity), false))
      w.lock.calls.get() shouldBe 0
      w.effects.all.collect { case e: Effect.YieldServer => e } shouldBe Nil

      w.clock.addAndGet(w.idleYieldAfterMs): Unit
      t.tick()
      w.lock.calls.get() shouldBe 1
      w.ownState.map(s => (s.idleSinceEpochMs, s.shuttingDown)) shouldBe Some((Some(lastActivity), true))
      val need = MemoryNeed.OthersWantMore(List(StateFile.selfIdentity("other").pid))
      w.effects.all.collect { case e: Effect.YieldServer => e } shouldBe List(Effect.YieldServer(need, idleForMs = w.idleYieldAfterMs))

      // Once is enough: no second yield, no second lock.
      w.clock.addAndGet(w.slowCheckIntervalMs): Unit
      t.tick()
      w.lock.calls.get() shouldBe 1
      w.effects.all.collect { case e: Effect.YieldServer => e }.size shouldBe 1
    }
  }

  test("a connected IDE keeps an idle server from yielding, and so does another server that has been idle longer or is already going") {
    withWorld { w =>
      val lastActivity = w.clock.get() - w.idleYieldAfterMs
      w.otherServer("bbbb", forks = Nil, wantsMore = true)
      w.idleness.set(Yield.Idleness(nonObserverConnections = 1, lastActivityEpochMs = lastActivity))
      val t = ticker(w)
      t.tick()
      w.lock.calls.get() shouldBe 0
      w.ownState.get.idleSinceEpochMs shouldBe None // a client is connected: not idle, whatever the clock says

      // Nobody connected now, but another server has been idle longer: it goes first, this one takes the lock, looks, and stays.
      w.idleness.set(Yield.Idleness(nonObserverConnections = 0, lastActivityEpochMs = lastActivity))
      w.otherServer("cccc", forks = Nil, wantsMore = false, idleSinceEpochMs = Some(lastActivity - 1L), shuttingDown = false)
      w.clock.addAndGet(w.slowCheckIntervalMs): Unit
      t.tick()
      w.lock.calls.get() shouldBe 1
      w.ownState.get.shuttingDown shouldBe false
      w.effects.all.collect { case e: Effect.YieldServer => e } shouldBe Nil

      // The longer-idle one marked itself shutting down: still nobody else goes this tick.
      w.otherServer("cccc", forks = Nil, wantsMore = false, idleSinceEpochMs = Some(lastActivity - 1L), shuttingDown = true)
      w.clock.addAndGet(w.slowCheckIntervalMs): Unit
      t.tick()
      w.lock.calls.get() shouldBe 2
      w.ownState.get.shuttingDown shouldBe false
      w.effects.all.collect { case e: Effect.YieldServer => e } shouldBe Nil
    }
  }

  test("every deciding tick is reported to the observer: what it decided, whether it claimed, and how long each step held the lock") {
    withWorld { w =>
      val t = ticker(w)
      t(Event.RegisterRequest(r1, RequestKind.Test))
      t(Event.SubmitReady(r1, List(forkDemand(r1, "t1", boundMb = 1000L, shared = false)), Map(k -> 1)))
      t.tick()
      val reports = w.reports.toArray(Array.empty[TickReport]).toList
      reports should have size 1
      val r = reports.head
      r.claimed shouldBe true
      r.lock shouldBe LockState.Held
      r.holdBreakdownMs.map(_._1) shouldBe List("probe", "read", "decide", "write")
      r.spawns shouldBe 1
      r.spawnsGuaranteed shouldBe 1
      r.pressure shouldBe Some(Pressure.Normal)
      r.liveServers shouldBe 1
      r.forks shouldBe 1
      w.quiets.get() shouldBe 0 // not idle: nothing to flush for
    }
  }

  test("an idle slow check tells the observer it is quiet, and reports no tick") {
    withWorld { w =>
      val t = ticker(w)
      t.tick()
      w.reports.isEmpty shouldBe true
      w.quiets.get() shouldBe 1
    }
  }

  test("a grant with several processes is charged their trees summed; a gone process leaves the set; an empty set is charged the bound again") {
    withWorld { w =>
      val t = ticker(w)
      t(Event.RegisterRequest(r1, RequestKind.Link))
      t(Event.SubmitReady(r1, List(ForkDemand(r1, TaskId("link:n"), ForkKind.Link, ForkKey("link:n"), 2000L, cpu = 1, shared = false)), Map.empty))
      t.tick()
      t(Event.ForkSpawned(ForkId(1), pid = 500L))
      t(Event.ForkSpawned(ForkId(1), pid = 501L))
      w.forkProbe.footprints.set(Map(500L -> 300L, 501L -> 120L))
      w.clock.addAndGet(Ticker.MeasureAfterMs): Unit
      t.tick()
      t.current.forks.head.state shouldBe ForkState.Measured(420L, w.clock.get())
      t.current.forks.head.pids shouldBe Set(500L, 501L)

      // A new process joining resets to the bound until the whole set is measured again.
      t(Event.ForkSpawned(ForkId(1), pid = 502L))
      t.current.forks.head.state shouldBe ForkState.Starting
      w.forkProbe.footprints.set(Map(500L -> 300L, 502L -> 50L)) // 501 is gone
      w.clock.addAndGet(Ticker.MeasureAfterMs): Unit
      t.tick()
      t.current.forks.head.state shouldBe ForkState.Measured(350L, w.clock.get())
      t.current.forks.head.pids shouldBe Set(500L, 502L)
      w.ownState.get.forks.head.pids shouldBe List(500L, 502L)

      // Everything gone while the grant still lives: back to the bound, never undercounted.
      w.forkProbe.footprints.set(Map.empty)
      w.clock.addAndGet(Ticker.MeasureAfterMs): Unit
      t.tick()
      t.current.forks.head.state shouldBe ForkState.Starting
      t.current.forks.head.pids shouldBe Set.empty
      t.current.forks.head.reclaimableMb shouldBe 2000L
    }
  }

  /** The leak seen live: a batch's fork demand was granted a Spawn; a ready set built by the request's thread while that tick ran still listed the demand and
    * landed after it; when the fork went idle the stale demand was granted a Reuse of it, which nothing ever released — the fork stayed busy, with a live
    * process, after its request had ended.
    */
  test("a demand already granted is not granted again when a ready set built before the grant arrives late") {
    withWorld { w =>
      val t = ticker(w)
      val batch = forkDemand(r1, "bleep-bsp-tests (batch of 4)#7", boundMb = 1000L, shared = false)
      t(Event.RegisterRequest(r1, RequestKind.Test))
      t(Event.SubmitReady(r1, List(batch), Map(k -> 1)))
      t.tick()
      w.effects.all shouldBe List(Effect.Spawn(batch, ForkId(1), guaranteed = true))
      t(Event.ForkSpawned(ForkId(1), pid = 500L))
      // The stale ready set: built while the tick above was granting, delivered after it.
      t(Event.SubmitReady(r1, List(batch), Map(k -> 1)))
      w.effects.clear()
      t.tick()
      w.effects.all shouldBe Nil
      t.current.ready shouldBe Nil
      // The batch finishes and its fork goes idle: still nothing is granted to the stale demand, and the fork is evicted as nothing left wants it.
      t(Event.ForkWorkFinished(ForkId(1), cpu = 1))
      t(Event.SubmitReady(r1, Nil, Map.empty))
      t.tick()
      w.effects.all shouldBe List(Effect.Evict(ForkId(1), Decision.EvictReason.NothingToReuseIt))
      t(Event.ForkExited(ForkId(1)))
      t(Event.UnregisterRequest(r1))
      t.current.forks shouldBe Nil
      t.current.cpuInUse shouldBe 0
    }
  }

  /** With no request nothing may hold a fork or a cpu slot: a fork a request left busy — its work never reported finished — is evicted the next tick, and the
    * ticks keep coming until it is gone.
    */
  test("no requests means no forks and no cpu in use: a fork left busy by an ended request is evicted") {
    withWorld { w =>
      val t = ticker(w)
      t(Event.RegisterRequest(r1, RequestKind.Test))
      t(Event.SubmitReady(r1, List(forkDemand(r1, "t1", 1000L, shared = false)), Map(k -> 1)))
      t.tick()
      t(Event.ForkSpawned(ForkId(1), pid = 500L))
      t(Event.UnregisterRequest(r1)) // no ForkWorkFinished: the request ended with its fork marked busy
      t.idle shouldBe false // a fork remains, so the runtime keeps ticking
      w.effects.clear()
      t.tick()
      w.effects.all shouldBe List(Effect.Evict(ForkId(1), Decision.EvictReason.OwnerGone))
      t.current.cpuInUse shouldBe 0
      w.ownState.get.cpuInUse shouldBe 0
      t(Event.ForkExited(ForkId(1)))
      t.current.forks shouldBe Nil
      t.idle shouldBe true
    }
  }

  test("an unconstrained server never probes, never reads anyone and never sheds") {
    withWorld { w =>
      w.machineProbe.current.set(
        MachineSample(
          physicalMb = 16_384L,
          usedMb = 14_000L,
          availableMb = 2_384L,
          roomBasis = RoomBasis.StartingForksCap,
          pressure = RawPressure.MacOs(4, 0L, 0L, 0L, 0L)
        )
      )
      w.otherServer("bbbb", forks = Nil, wantsMore = true)
      val t = new Ticker(w.depsUnconstrained("test", tickIntervalPerServerMs = 10L))
      t.tick()
      w.clock.addAndGet(w.slowCheckIntervalMs): Unit
      t.tick()
      w.machineProbe.samples.get() shouldBe 0
      w.effects.all shouldBe Nil
      t.idleParkMs shouldBe None
      ticker(w).idleParkMs shouldBe Some(w.slowCheckIntervalMs)
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
      // Available 12288 − reserve 2048 = 10240 of room; the other server's Starting 9000 leaves 1240.
      w.otherServer("bbbb", forks = List(StateFork(3L, List(77L), ForkKind.TestBatch, 9000L, StateForkState.Starting, 0L)))
      w.otherServer("cccc", forks = Nil)
      val t = ticker(w)
      t(Event.RegisterRequest(r1, RequestKind.Test))
      t(Event.SubmitReady(r1, List(forkDemand(r1, "t1", boundMb = 1000L, shared = false), forkDemand(r1, "t2", boundMb = 2000L, shared = false)), Map(k -> 2)))
      t.tick()
      w.lock.calls.get() shouldBe 1
      // t1 is the guarantee; t2 (2000) does not fit in the 1240 the other server left.
      w.effects.all shouldBe List(Effect.Spawn(forkDemand(r1, "t1", 1000L, shared = false), ForkId(1), guaranteed = true))
      w.ownState.get.forks shouldBe List(StateFork(1L, Nil, ForkKind.TestSuite, 1000L, StateForkState.Starting, w.clock.get()))
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
      // A process that exited under the probe leaves the set; with nothing alive the grant is charged its bound again until its exit event follows.
      w.forkProbe.footprints.set(Map.empty)
      w.clock.set(started + 3000L)
      t.tick()
      t.current.forks.head.state shouldBe ForkState.Starting
      t.current.forks.head.pids shouldBe Set.empty
      t.current.forks.head.reclaimableMb shouldBe 1000L
    }
  }

  test("a successor process under the same grant is charged at the bound again and measured a second after it started") {
    withWorld { w =>
      val t = ticker(w)
      t(Event.RegisterRequest(r1, RequestKind.Test))
      t(Event.SubmitReady(r1, List(forkDemand(r1, "t1", 1000L, shared = false)), Map(k -> 1)))
      val started = w.clock.get()
      t.tick()
      t(Event.ForkSpawned(ForkId(1), pid = 500L))
      w.forkProbe.footprints.set(Map(500L -> 640L, 501L -> 300L))
      w.clock.set(started + 1000L)
      t.tick()
      t.current.forks.head.state shouldBe ForkState.Measured(640L, started + 1000L)
      // The first script's JVM ends and the next one starts under the same fork: back to Starting, measured a second after *its* start, not the fork's. The
      // dead predecessor is dropped from the set at that measurement, so the charge is the successor's alone.
      w.clock.set(started + 1500L)
      t(Event.ForkSpawned(ForkId(1), pid = 501L))
      w.forkProbe.footprints.set(Map(501L -> 300L))
      t.current.forks.head.pids shouldBe Set(500L, 501L)
      t.current.forks.head.state shouldBe ForkState.Starting
      t.current.forks.head.startedAtMs shouldBe started
      w.clock.set(started + 2400L)
      t.tick()
      t.current.forks.head.state shouldBe ForkState.Starting
      w.clock.set(started + 2500L)
      t.tick()
      t.current.forks.head.state shouldBe ForkState.Measured(300L, started + 2500L)
      t.current.forks.head.pids shouldBe Set(501L)
      w.ownState.get.forks.head.pids shouldBe List(501L)
    }
  }

  test("an unavailable lock still spawns the guarantee, reads nobody, and is reported") {
    withWorld { w =>
      w.otherServer("bbbb", forks = Nil)
      w.lock.answer.set(LockState.Unavailable(LockHolder.Announced(77L, 1L, 1234L)))
      val t = ticker(w)
      t(Event.RegisterRequest(r1, RequestKind.Test))
      t(Event.SubmitReady(r1, List(forkDemand(r1, "t1", 1000L, shared = false), forkDemand(r1, "t2", 1000L, shared = false)), Map(k -> 2)))
      t.tick()
      w.effects.all shouldBe List(
        Effect.Spawn(forkDemand(r1, "t1", 1000L, shared = false), ForkId(1), guaranteed = true),
        Effect.LockUnavailable(LockHolder.Announced(77L, 1L, 1234L))
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
      w.machineProbe.current.set(MachineSample(16_384L, 15_000L, 1_384L, RoomBasis.StartingForksCap, RawPressure.MacOs(4, 0L, 0L, 0L, 0L)))
      t.tick()
      // Eviction is the decision's; the shed is §5.2's answer to the same pressure, for whatever this server caches for nobody.
      w.effects.all shouldBe List(
        Effect.Evict(ForkId(1), Decision.EvictReason.CriticalPressure),
        Effect.ShedIdleCaches(MemoryNeed.UnderPressure(Pressure.Critical))
      )
      // Still registered, flagged, until the process is gone; not evicted twice meanwhile.
      t.current.forks.map(_.evicting) shouldBe List(true)
      w.effects.clear()
      t.tick()
      w.effects.all shouldBe Nil
      t(Event.ForkExited(ForkId(1)))
      t.current.forks shouldBe Nil
    }
  }

  test("a platform without a pressure source is reported once, and scheduling goes on without the brake") {
    withWorld { w =>
      w.machineProbe.current.set(MachineSample(16_384L, 15_000L, 1_384L, RoomBasis.AvailableMemory, RawPressure.Unavailable("kernel without PSI")))
      val t = ticker(w)
      t(Event.RegisterRequest(r1, RequestKind.Test))
      t(Event.SubmitReady(r1, List(forkDemand(r1, "t1", 1000L, shared = false)), Map(k -> 1)))
      t.tick()
      t.tick()
      t.tick()
      w.effects.all.collect { case e: Effect.PressureSignalMissing => e } shouldBe List(Effect.PressureSignalMissing("kernel without PSI"))
      w.effects.all.collect { case e: Effect.Spawn => e.demand.taskId.value } shouldBe List("t1")
    }
  }

  test("unconstrained: no probe, no lock, no file, ever — and the guarantee, one spawn per tick, warm reuse and idle eviction still run") {
    withWorld { w =>
      val t = new Ticker(w.depsUnconstrained("test", tickIntervalPerServerMs = 10L))
      t(Event.RegisterRequest(r1, RequestKind.Test))
      t(Event.RegisterRequest(r2, RequestKind.Test))
      t(Event.SubmitReady(r1, List(forkDemand(r1, "t1", 1000L, shared = false)), Map(k -> 2)))
      t(Event.SubmitReady(r2, List(forkDemand(r2, "t2", 1000L, shared = false)), Map(k -> 1)))
      t.tick()
      w.effects.all.collect { case e: Effect.Spawn => e.demand.taskId.value } shouldBe List("t1") // one per tick, oldest first
      t.tick()
      w.effects.all.collect { case e: Effect.Spawn => e.demand.taskId.value } shouldBe List("t1", "t2")
      t(Event.ForkSpawned(ForkId(1), 500L))
      t(Event.ForkWorkFinished(ForkId(1), 1))
      t(Event.SubmitReady(r1, List(forkDemand(r1, "t3", 1000L, shared = false)), Map(k -> 1)))
      w.effects.clear()
      t.tick()
      w.effects.all.collect { case e: Effect.Reuse => (e.demand.taskId.value, e.fork.value) } shouldBe List(("t3", 1L))
      t(Event.ForkWorkFinished(ForkId(1), 1))
      t(Event.SubmitReady(r1, Nil, Map.empty))
      w.effects.clear()
      t.tick()
      w.effects.all shouldBe Nil // r2 still has a suite to start on this key, so the fork stays warm
      t(Event.SubmitReady(r2, Nil, Map.empty))
      t.tick()
      w.effects.all shouldBe List(Effect.Evict(ForkId(1), Decision.EvictReason.NothingToReuseIt))
      // Nothing machine-wide was touched, by construction and in fact.
      w.machineProbe.samples.get() shouldBe 0
      w.forkProbe.calls.get() shouldBe 0
      w.lock.calls.get() shouldBe 0
      w.ownState shouldBe None
      t.cadenceMs shouldBe 10L
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
      w.effects.all shouldBe List(
        Effect.Evict(ForkId(1), Decision.EvictReason.OwnerGone)
      ) // its request has ended: gone whatever it was doing, before any warm-fork reasoning
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
      w.otherServer("bbbb", forks = List(StateFork(3L, List(77L), ForkKind.TestBatch, 100L, StateForkState.Starting, 0L)))
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
      // A sanity bound, not a benchmark: ~270µs unloaded on an M-series laptop, several ms when the machine is busy running other suites.
      perTickMicros should be < 50_000L
    }
  }
}
