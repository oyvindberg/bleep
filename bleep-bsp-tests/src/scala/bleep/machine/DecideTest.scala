package bleep.machine

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

/** Deterministic scenarios for [[Decide.decide]], one per rule where a property is not the clearest statement. */
class DecideTest extends AnyFunSuite with Matchers {
  private val now = 50_000L
  private val identity = ServerIdentity(pid = 1L, startedAtEpochMs = 1L, bleepVersion = "test")
  private val params = Params(reserveMb = 1000L, maxStartingForks = 1, parallelism = 4, maxNewForksPerTick = 1)
  private val k = ForkKey("k")
  private val r1 = Request(RequestId("r1"), RequestKind.Test, startedAtMs = 1L)
  private val r2 = Request(RequestId("r2"), RequestKind.Test, startedAtMs = 2L)

  private val blank = MyState.empty.copy(nextForkId = 101L)

  /** A 10 GB machine with `availableMb` free for a new process. Where available measures room (the default, Linux's rule) room = available − the 1000 MB
    * reserve; where the starting forks are capped (macOS) available is display only. Used is display only and set for scale.
    */
  private def view(
      availableMb: Long,
      pressure: Pressure = Pressure.Normal,
      basis: RoomBasis = RoomBasis.AvailableMemory,
      churn: Option[Long] = None
  ): MachineView =
    MachineView(
      physicalMb = 10_000L,
      usedMb = 10_000L - availableMb,
      availableMb = availableMb,
      roomBasis = basis,
      pressure = pressure,
      nowMs = now,
      churnPagesPerSecond = churn,
      pressureLevel = churn.map(_ => 1)
    )

  private def fork(
      id: Long,
      owner: Request,
      key: ForkKey = k,
      busyCpu: Int = 0,
      state: ForkState = ForkState.Measured(500L, now - 2000L),
      startedAtMs: Long = 0L,
      shared: Boolean = false
  ) =
    RunningFork(
      ForkId(id),
      Set(100L + id),
      owner.id,
      key,
      ForkKind.TestSuite,
      boundMb = 1000L,
      shared = shared,
      state = state,
      busyCpu = busyCpu,
      startedAtMs = startedAtMs,
      evicting = false,
      pidSinceMs = startedAtMs
    )

  private def demand(owner: Request, task: String, key: ForkKey = k, boundMb: Long = 1000L, cpu: Int = 1, shared: Boolean = false) =
    ForkDemand(owner.id, TaskId(task), ForkKind.TestSuite, key, boundMb, cpu, shared)

  private def decide(
      me: MyState,
      v: MachineView = view(availableMb = 8000L),
      others: List[StateJson] = Nil,
      lock: LockState = LockState.Held,
      gate: HeapGate = HeapGate.alwaysAdmit,
      p: Params = params
  ) =
    Decide.decide(Machine.Cooperative(v, others, lock), me, p, gate, identity)

  private def other(forks: StateFork*): StateJson =
    StateJson(
      version = 1,
      pid = 2L,
      startedAtEpochMs = 1L,
      bleepVersion = "other",
      updatedAtEpochMs = now,
      requests = 1,
      cpuInUse = 1,
      wantsMore = false,
      shuttingDown = false,
      forks = forks.toList,
      idleSinceEpochMs = None
    )

  test("a fork is charged its bound until measured; measured forks are in usedMb and charge nothing") {
    // available 4000 − reserve 1000 → room 3000. Another server's Starting fork of 2000 leaves 1000: a second (non-guaranteed) 1500 demand does not fit.
    val me = blank.copy(
      requests = List(r1),
      forks = List(fork(1, r1, busyCpu = 1)), // r1 already has a fork, so nothing here is guaranteed
      ready = List(demand(r1, "t1", boundMb = 1500L)),
      unstartedSuitesByKey = Map(k -> 1)
    )
    val starting = other(StateFork(7L, List(900L), ForkKind.TestSuite, boundMb = 2000L, StateForkState.Starting, now))
    decide(me, view(availableMb = 4000L), others = List(starting)).spawn shouldBe empty
    // A server that is shutting down (§5.1) is still alive, so its starting fork is still charged.
    decide(me, view(availableMb = 4000L), others = List(starting.copy(shuttingDown = true))).spawn shouldBe empty
    val measured = other(StateFork(7L, List(900L), ForkKind.TestSuite, boundMb = 2000L, StateForkState.Measured(1800L), now))
    decide(me, view(availableMb = 4000L), others = List(measured)).spawn.map(_.demand.taskId.value) shouldBe List("t1")
    // The same for this server's own forks: a Starting one of 1000 leaves 2000 of the 3000, so a 2500 demand waits; measured, it fits.
    val mine = me.copy(forks = me.forks :+ fork(2, r1, busyCpu = 1, state = ForkState.Starting), ready = List(demand(r1, "t1", boundMb = 2500L)))
    decide(mine, view(availableMb = 4000L)).spawn shouldBe empty
    decide(mine.copy(forks = me.forks :+ fork(2, r1, busyCpu = 1)), view(availableMb = 4000L)).spawn.map(_.demand.taskId.value) shouldBe List("t1")
  }

  test("under room shortage idle forks are evicted oldest first, only as many as make the admission fit") {
    // available 1500 − reserve 1000 → room 500. Three idle warm forks of another key, 500MB each (measured), oldest at t=10.
    val warm = ForkKey("warm")
    val me = blank.copy(
      requests = List(r1),
      forks = List(
        fork(1, r1, key = warm, startedAtMs = 30L),
        fork(2, r1, key = warm, startedAtMs = 10L),
        fork(3, r1, key = warm, startedAtMs = 20L),
        fork(4, r1, busyCpu = 1)
      ),
      ready = List(demand(r1, "t1", boundMb = 1400L)),
      unstartedSuitesByKey = Map(warm -> 5, k -> 1)
    )
    val d = decide(me, view(availableMb = 1500L))
    d.evict.map(e => (e.fork.value, e.reason)) shouldBe List((2L, Decision.EvictReason.RoomShortage), (3L, Decision.EvictReason.RoomShortage))
    d.spawn.map(_.demand.taskId.value) shouldBe List("t1")
    d.next.forks.filter(_.evicting).map(_.id.value) shouldBe List(2L, 3L)
    d.next.forks.filterNot(_.evicting).map(_.id.value).sorted shouldBe List(1L, 4L, 101L)
  }

  test("when even every idle fork would not make room, none is evicted and the demand waits") {
    val warm = ForkKey("warm")
    val me = blank.copy(
      requests = List(r1),
      forks = List(fork(1, r1, key = warm), fork(4, r1, busyCpu = 1)),
      ready = List(demand(r1, "t1", boundMb = 5000L)),
      unstartedSuitesByKey = Map(warm -> 5, k -> 1)
    )
    val d = decide(me, view(availableMb = 1500L))
    d.evict shouldBe empty
    d.spawn shouldBe empty
    d.publish.wantsMore shouldBe true
  }

  test("a guaranteed fork reuses an idle warm fork before anything is evicted, even under Critical pressure") {
    val me = blank.copy(requests = List(r1), forks = List(fork(1, r1)), ready = List(demand(r1, "t1")), unstartedSuitesByKey = Map(k -> 1))
    val d = decide(me, view(availableMb = 500L, pressure = Pressure.Critical), lock = LockState.Unavailable(LockHolder.Announced(5L, 0L, 3000L)))
    d.reuse.map(x => (x.fork.value, x.guaranteed)) shouldBe List((1L, true))
    d.spawn shouldBe empty
    d.evict shouldBe empty
  }

  test("two guarantees needing new forks share one spawn slot: the older request spawns this tick, the younger next tick") {
    val me = blank.copy(requests = List(r2, r1), ready = List(demand(r2, "t2"), demand(r1, "t1")), unstartedSuitesByKey = Map(k -> 2))
    val first = decide(me, view(availableMb = 500L, pressure = Pressure.Elevated), lock = LockState.Unavailable(LockHolder.Unannounced))
    first.spawn.map(s => (s.demand.taskId.value, s.guaranteed)) shouldBe List(("t1", true))
    first.publish.wantsMore shouldBe true
    val second = decide(first.next, view(availableMb = 500L, pressure = Pressure.Elevated), lock = LockState.Unavailable(LockHolder.Unannounced))
    second.spawn.map(s => (s.demand.taskId.value, s.guaranteed)) shouldBe List(("t2", true))
    // A warm fork of the key, though, is reused for a guarantee without waiting for the slot — by the oldest request, whoever started the fork — and the
    // spawn slot then goes to the other.
    val warm = me.copy(forks = List(fork(1, r2)))
    val d = decide(warm, view(availableMb = 500L, pressure = Pressure.Elevated), lock = LockState.Unavailable(LockHolder.Unannounced))
    d.reuse.map(x => (x.demand.taskId.value, x.guaranteed)) shouldBe List(("t1", true))
    d.spawn.map(s => (s.demand.taskId.value, s.guaranteed)) shouldBe List(("t2", true))
  }

  test("an idle fork of another request is reused, and changes owner") {
    val me = blank.copy(requests = List(r1, r2), forks = List(fork(1, r1)), ready = List(demand(r2, "t1")), unstartedSuitesByKey = Map(k -> 1))
    val d = decide(me)
    d.reuse.map(_.fork.value) shouldBe List(1L)
    d.next.forks.map(f => (f.id.value, f.owner.value, f.busyCpu)) shouldBe List((1L, "r2", 1))
  }

  test("a request's own idle fork is preferred over another request's") {
    val me = blank.copy(
      requests = List(r1, r2),
      forks = List(fork(1, r1, startedAtMs = 1L), fork(2, r2, startedAtMs = 2L)),
      ready = List(demand(r2, "t1")),
      unstartedSuitesByKey = Map(k -> 1)
    )
    decide(me).reuse.map(_.fork.value) shouldBe List(2L)
  }

  test("a suite joins its request's busy shared fork of the same key instead of spawning") {
    val me = blank.copy(
      requests = List(r1),
      forks = List(fork(1, r1, busyCpu = 1, shared = true)),
      ready = List(demand(r1, "t2", shared = true), demand(r1, "t3", shared = true)),
      unstartedSuitesByKey = Map(k -> 2)
    )
    val d = decide(me)
    d.reuse.map(x => (x.demand.taskId.value, x.fork.value, x.guaranteed)) shouldBe List(("t2", 1L, false), ("t3", 1L, false))
    d.spawn shouldBe empty
    d.next.forks.head.busyCpu shouldBe 3
  }

  test("an exclusive busy fork of the same key does not stop a second fork being spawned") {
    val me = blank.copy(requests = List(r1), forks = List(fork(1, r1, busyCpu = 1)), ready = List(demand(r1, "t2")), unstartedSuitesByKey = Map(k -> 1))
    decide(me).spawn.map(_.demand.taskId.value) shouldBe List("t2")
  }

  test("another request's busy shared fork is not joined") {
    val me = blank.copy(
      requests = List(r1, r2),
      forks = List(fork(1, r1, busyCpu = 1, shared = true)),
      ready = List(demand(r2, "t2", shared = true)),
      unstartedSuitesByKey = Map(k -> 1)
    )
    val d = decide(me)
    d.reuse shouldBe empty
    d.spawn.map(x => (x.demand.taskId.value, x.guaranteed)) shouldBe List(("t2", true))
  }

  test("a request that is only compiling gets one in-heap slot regardless of cpu and the heap gate") {
    val neverAdmit: HeapGate = (_, _, _, _) => HeapVerdict.Defer(500L)
    val c1 = InHeap(r1.id, TaskId("c1"), InHeapKind.Compile, cpu = 1)
    val c2 = InHeap(r1.id, TaskId("c2"), InHeapKind.Compile, cpu = 1)
    val me = blank.copy(requests = List(r1), inHeap = List(InHeapRunning(r2.id, TaskId("x"), InHeapKind.Compile, 4)), ready = List(c1, c2))
    val d = decide(me, gate = neverAdmit)
    d.admitInHeap shouldBe List(Decision.AdmitInHeap(c1, guaranteed = true))
    d.heapDeferred.map(_.demand) shouldBe Nil // c2 never reached the gate: the cpu budget (4 of 4 used, +1 guaranteed) is spent
    d.next.cpuInUse shouldBe 5
  }

  test("the heap gate's deferral is remembered from the first refusal until the compile is admitted") {
    val c1 = InHeap(r1.id, TaskId("c1"), InHeapKind.Compile, cpu = 1)
    val gate: HeapGate = (_, _, first, _) => if (first.isDefined) HeapVerdict.Admit else HeapVerdict.Defer(300L)
    val me = blank.copy(requests = List(r1), inHeap = List(InHeapRunning(r1.id, TaskId("x"), InHeapKind.Compile, 1)), ready = List(c1))
    val first = decide(me, gate = gate)
    first.admitInHeap shouldBe empty
    first.heapDeferred shouldBe List(Decision.HeapDeferred(c1, delayMs = 300L, firstDeferredAtMs = now))
    first.next.heapDeferredSince shouldBe Map(TaskId("c1") -> now)
    val second = decide(first.next, gate = gate)
    second.admitInHeap shouldBe List(Decision.AdmitInHeap(c1, guaranteed = false))
    second.next.heapDeferredSince shouldBe empty
  }

  test("an in-heap link answers to the heap gate like a compile, and counts as a compile for others") {
    val neverAdmit: HeapGate = (_, _, _, _) => HeapVerdict.Defer(500L)
    val link = InHeap(r1.id, TaskId("link:app-js"), InHeapKind.Link, cpu = 1)
    val busy = blank.copy(requests = List(r1), inHeap = List(InHeapRunning(r1.id, TaskId("x"), InHeapKind.Compile, 1)), ready = List(link))
    withClue("a second heap-heavy task is held like a second compile would be: ") {
      decide(busy, gate = neverAdmit).heapDeferred.map(_.demand) shouldBe List(link)
    }
    val seen = new java.util.concurrent.atomic.AtomicReference[Option[Boolean]](None)
    val recording: HeapGate = (_, others, _, _) => { seen.set(Some(others)); HeapVerdict.Admit }
    val compile = InHeap(r1.id, TaskId("c1"), InHeapKind.Compile, cpu = 1)
    val linking = blank.copy(requests = List(r1), inHeap = List(InHeapRunning(r1.id, TaskId("l"), InHeapKind.Link, 1)), ready = List(compile))
    decide(linking, gate = recording).admitInHeap shouldBe List(Decision.AdmitInHeap(compile, guaranteed = false))
    withClue("a running in-heap link is 'others compiling' to the gate: ") {
      seen.get() shouldBe Some(true)
    }
  }

  test("a fork whose request has ended is evicted busy or idle, and its cpu goes back") {
    val busyOrphan = fork(1L, owner = r2, busyCpu = 1)
    val idleOwned = fork(2L, owner = r1, busyCpu = 0)
    val me = blank.copy(requests = List(r1), forks = List(busyOrphan, idleOwned), unstartedSuitesByKey = Map(k -> 1))
    val d = decide(me)
    d.evict shouldBe List(Decision.Evict(ForkId(1L), Decision.EvictReason.OwnerGone))
    d.next.cpuInUse shouldBe 0
    d.next.forks.find(_.id == ForkId(1L)).map(_.evicting) shouldBe Some(true)
    d.next.forks.find(_.id == ForkId(2L)).map(_.evicting) shouldBe Some(false) // its owner lives and a suite still wants it
  }

  /** The owner's one-hour passive log of a 48 GB Mac in ordinary use (design §9), by kernel_task band — churn p50, available (free + speculative + purgeable)
    * p50: calm (< 50 %) 3k pages/s, 994 MB; 50–90 % 125k, 223 MB; > 90 % 249k, 134 MB. A 2560 MB fork would have been refused by available memory in 68–74 % of
    * the calm samples whatever the reserve: on macOS available is no measure of room, the unmeasured forks are capped instead, and churn decides.
    */
  private val macCalm = view(availableMb = 994L, basis = RoomBasis.StartingForksCap, churn = Some(3_000L))
  private val macBusy = view(availableMb = 223L, pressure = Pressure.Elevated, basis = RoomBasis.StartingForksCap, churn = Some(125_000L))
  private val macOverloaded = view(availableMb = 134L, pressure = Pressure.Critical, basis = RoomBasis.StartingForksCap, churn = Some(249_000L))
  private def wantingTwo = blank.copy(
    requests = List(r1),
    ready = List(demand(r1, "t1", boundMb = 2560L), demand(r1, "t2", boundMb = 2560L)),
    unstartedSuitesByKey = Map(k -> 2)
  )

  test("macOS, calm: a fork beyond the guarantee starts with under 1 GB available, since available is no measure of room there") {
    val working = wantingTwo.copy(forks = List(fork(1L, r1, busyCpu = 1)), ready = List(demand(r1, "t2", boundMb = 2560L)))
    decide(working, v = macCalm).spawn.map(_.demand.taskId.value) shouldBe List("t2")
    // The same machine read by a platform where available does measure room refuses it: 994 − 1000 < 2560.
    decide(working, v = view(availableMb = 994L)).spawn shouldBe Nil
  }

  test("macOS, calm: the second fork beyond the guarantee waits until the first has been measured, not just started") {
    val first = decide(wantingTwo, v = macCalm)
    first.spawn.map(_.demand.taskId.value) shouldBe List("t1") // the guarantee; t2 waits on the one-spawn-per-tick slot
    val started = first.next.copy(ready = List(demand(r1, "t2", boundMb = 2560L)))
    started.forks.map(_.state) shouldBe List(ForkState.Starting)
    decide(started, v = macCalm).spawn shouldBe Nil // one unmeasured fork on the machine already: the cap is 1
    val measured = started.copy(forks = started.forks.map(_.copy(state = ForkState.Measured(900L, now))))
    decide(measured, v = macCalm).spawn.map(_.demand.taskId.value) shouldBe List("t2")
    // It is the count of unmeasured forks, not room: a cap of two admits it at once.
    decide(started, v = macCalm, p = params.copy(maxStartingForks = 2)).spawn.map(_.demand.taskId.value) shouldBe List("t2")
  }

  test("macOS: the cap is machine-wide — another server's starting fork holds this one's second fork back, its measured fork does not") {
    val working = wantingTwo.copy(forks = List(fork(1L, r1, busyCpu = 1)), ready = List(demand(r1, "t2", boundMb = 2560L)))
    val starting = other(StateFork(9L, List(9009L), ForkKind.TestSuite, 2560L, StateForkState.Starting, now - 500L))
    val measured = other(StateFork(9L, List(9009L), ForkKind.TestSuite, 2560L, StateForkState.Measured(700L), now - 5000L))
    decide(working, v = macCalm, others = List(starting)).spawn shouldBe Nil
    decide(working, v = macCalm, others = List(measured)).spawn.map(_.demand.taskId.value) shouldBe List("t2")
  }

  test("macOS: Elevated churn withholds forks beyond the guarantee with nothing starting anywhere; Critical also evicts the warm ones; the guarantee stands") {
    val working = wantingTwo.copy(forks = List(fork(1L, r1, busyCpu = 1)), ready = List(demand(r1, "t2", boundMb = 2560L)))
    decide(working, v = macBusy).spawn shouldBe Nil
    decide(working, v = macOverloaded).spawn shouldBe Nil
    val warm = working.copy(forks = working.forks :+ fork(2L, r1, busyCpu = 0))
    decide(warm, v = macOverloaded).evict shouldBe List(Decision.Evict(ForkId(2L), Decision.EvictReason.CriticalPressure))
    decide(wantingTwo, v = macOverloaded).spawn.map(_.demand.taskId.value) shouldBe List("t1")
  }

  test("macOS: no fork is evicted for room — an idle fork is measured, so evicting it frees nothing the cap counts") {
    val warm = wantingTwo.copy(
      forks = List(fork(1L, r1, busyCpu = 1), fork(2L, r1, key = ForkKey("other"), busyCpu = 0)),
      ready = List(demand(r1, "t2", boundMb = 2560L))
    )
    val d = decide(
      warm.copy(unstartedSuitesByKey = Map(k -> 1, ForkKey("other") -> 1)),
      v = macCalm,
      others = List(other(StateFork(9L, Nil, ForkKind.TestSuite, 2560L, StateForkState.Starting, now)))
    )
    d.spawn shouldBe Nil
    d.evict shouldBe Nil
  }

  test("Linux is unchanged by the cap: with room, a second fork starts while the first is still unmeasured") {
    val first = decide(wantingTwo, v = view(availableMb = 8000L))
    val started = first.next.copy(ready = List(demand(r1, "t2", boundMb = 2560L)))
    decide(started, v = view(availableMb = 8000L)).spawn.map(_.demand.taskId.value) shouldBe List("t2") // 8000 − 1000 − 2560 ≥ 2560
  }

  test("discovery and processor resolution bypass the heap gate") {
    val neverAdmit: HeapGate = (_, _, _, _) => HeapVerdict.Defer(500L)
    val disc = InHeap(r1.id, TaskId("d"), InHeapKind.Discover, cpu = 1)
    val me = blank.copy(requests = List(r1), inHeap = List(InHeapRunning(r1.id, TaskId("x"), InHeapKind.Compile, 1)), ready = List(disc))
    decide(me, gate = neverAdmit).admitInHeap shouldBe List(Decision.AdmitInHeap(disc, guaranteed = false))
  }

  test("priority beyond guarantees interleaves requests by rank, oldest first") {
    val a = InHeap(r1.id, TaskId("a"), InHeapKind.Discover, 1)
    val b = InHeap(r1.id, TaskId("b"), InHeapKind.Discover, 1)
    val c = InHeap(r2.id, TaskId("c"), InHeapKind.Discover, 1)
    val me = blank.copy(
      requests = List(r2, r1),
      inHeap = List(InHeapRunning(r1.id, TaskId("x"), InHeapKind.Discover, 1), InHeapRunning(r2.id, TaskId("y"), InHeapKind.Discover, 1)),
      ready = List(a, b, c)
    )
    decide(me, p = params.copy(parallelism = 4)).admitInHeap.map(_.demand.taskId.value) shouldBe List("a", "c")
    Decide.interleave(List(List(1, 2, 3), List(4), List(5, 6))) shouldBe List(1, 4, 5, 2, 6, 3)
  }

  test("a ready demand for an unregistered request is a bug, not a silent drop") {
    val me = blank.copy(requests = Nil, ready = List(demand(r1, "t1")))
    an[IllegalArgumentException] should be thrownBy decide(me)
  }

  test("new forks are published Starting at their bound; nothing is published that is not in the next state") {
    val me = blank.copy(requests = List(r1), ready = List(demand(r1, "t1", boundMb = 1234L)))
    val d = decide(me)
    d.publish.forks shouldBe List(StateFork(101L, Nil, ForkKind.TestSuite, 1234L, StateForkState.Starting, now))
    d.publish.startingBoundMb shouldBe 1234L
    d.next.nextForkId shouldBe 102L
  }
}
