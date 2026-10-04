package bleep.machine

import java.nio.file.Path

/** One tick at a time, on one thread (design §7). [[TickRuntime]] owns that thread and the event queue; this is what the thread runs, kept free of threads and
  * clocks of its own so a test can drive it tick by tick with fake probes, a fake clock and an in-memory [[SchedulerEffects]].
  *
  * A tick:
  *   1. applies measurements: a Starting fork that has run a full second is measured through the [[ForkProbe]]; a Measured one is remeasured once a second
  *   1. decides whether it may *add* a memory claim — a ready fork demand that no idle fork of its key, and no busy shared fork of its request, could absorb
  *   1. if so: lock → probe → read every other live server's state → decide → write own state → unlock, in that order and nothing else under the lock
  *   1. otherwise: probe → decide with `LockState.NotNeeded` and nobody else's state → write own state if it changed
  *   1. carries the decision out through [[SchedulerEffects]], after the lock is released
  *
  * With no request, no fork and no in-heap task registered a tick decides nothing. What remains is the slow check (design §5.1, §5.2): every
  * `slowCheckIntervalMs`, one probe call and every other live server's `state.json` read without the lock, so a server with nothing to claim still learns that
  * memory is needed elsewhere — and sheds the caches of its idle workspaces for it. A busy server whose ticks take no lock does the same on the same cadence; a
  * claiming tick has read the others under the lock already. An idle server also publishes how long it has been idle, and — idle for `idleYieldAfterMs` with a
  * need — takes the lock to decide, with the others' state fresh, whether it is the one to yield (design §5.1).
  */
final class Ticker(deps: Ticker.Deps) {
  import Ticker._

  private var state: MyState = MyState.empty
  private var unstartedByRequest: Map[RequestId, Map[ForkKey, Int]] = Map.empty

  /** Every (request, task) ever granted — a slot, a spawn or a reuse — until the request ends. A ready set is built by the request's thread from what it still
    * waits for, and reaches this thread as an event; one built while a tick was granting one of its demands still lists that demand, and lands after the tick.
    * Without this a demand already running would be granted a second time: a second fork for a suite that has one, and nothing to ever release it. Task ids are
    * unique per request (a DAG task runs once; a test demand's id carries a counter), so "granted once" is exact, not a heuristic.
    */
  private var grantedTasks: Set[(RequestId, TaskId)] = Set.empty
  private var lastPublished: Option[StateJson] = None
  private var liveServersSeen: Int = 1
  private var pressureSignalWarned: Boolean = false
  private var lastMachine: Option[MachineView] = None
  private var lastLock: LockState = LockState.NotNeeded
  private var lastOthers: List[StateJson] = Nil
  private var lastSlowCheckMs: Option[Long] = None
  private var lastShedMs: Option[Long] = None

  /** This server's view of itself, for tests and `top`. */
  def current: MyState = state

  /** The cadence the runtime ticks at while requests exist: 10 ms × the servers seen on the last claiming tick, so the machine as a whole ticks about every 10
    * ms (design §7).
    */
  def cadenceMs: Long = deps.tickIntervalPerServerMs * liveServersSeen.toLong

  def idle: Boolean = state.requests.isEmpty && state.forks.isEmpty && state.inHeap.isEmpty

  /** How long the runtime may sleep with nothing to schedule: until the next slow check in cooperative mode; indefinitely in unconstrained, which has nothing
    * to check (no probe, no other servers).
    */
  def idleParkMs: Option[Long] = deps.mode match {
    case _: SchedulingMode.Cooperative   => Some(deps.slowCheckIntervalMs)
    case SchedulingMode.Unconstrained(_) => None
  }

  /** The state after the last tick, as plain data. Only the tick thread calls this; [[TickRuntime]] publishes the result for everyone else. */
  def snapshot(params: Params): SchedulerSnapshot =
    SchedulerSnapshot(
      mode = deps.mode match {
        case SchedulingMode.Unconstrained(_) => "unconstrained"
        case _: SchedulingMode.Cooperative   => "cooperative"
      },
      state = state,
      params = params,
      machine = lastMachine,
      lock = lastLock,
      liveServers = liveServersSeen,
      ready = state.ready
    )

  def apply(event: Event): Unit = event match {
    case Event.RegisterRequest(id, kind) =>
      require(!state.requests.exists(_.id == id), s"request ${id.value} is already registered")
      state = state.copy(requests = state.requests :+ Request(id, kind, deps.clock()))

    case Event.UnregisterRequest(id) =>
      require(state.requests.exists(_.id == id), s"request ${id.value} is not registered")
      state = state.copy(
        requests = state.requests.filterNot(_.id == id),
        ready = state.ready.filterNot(_.request == id),
        inHeap = state.inHeap.filterNot(_.request == id),
        heapDeferredSince = state.heapDeferredSince -- state.ready.filter(_.request == id).map(_.taskId)
      )
      unstartedByRequest -= id
      grantedTasks = grantedTasks.filterNot(_._1 == id)
      state = state.copy(unstartedSuitesByKey = aggregateUnstarted())

    case Event.SubmitReady(request, ready, unstarted) =>
      require(state.requests.exists(_.id == request), s"ready set submitted for unregistered request ${request.value}")
      ready.foreach(d => require(d.request == request, s"demand ${d.taskId.value} of request ${d.request.value} submitted under ${request.value}"))
      unstartedByRequest += (request -> unstarted)
      // A demand this scheduler has already granted is not ready, whatever a ready set built before that grant says.
      val fresh = ready.filterNot(d => grantedTasks.contains((d.request, d.taskId)))
      state = state.copy(ready = state.ready.filterNot(_.request == request) ++ fresh, unstartedSuitesByKey = aggregateUnstarted())

    case Event.InHeapFinished(request, taskId) =>
      val i = state.inHeap.indexWhere(t => t.request == request && t.taskId == taskId)
      require(i >= 0, s"in-heap task ${taskId.value} of ${request.value} is not running")
      state = state.copy(inHeap = state.inHeap.patch(i, Nil, 1))

    case Event.ForkSpawned(fork, pid) =>
      // A process joins the grant: the first, a successor, or one more alongside the others (a Scala Native link's clangs). One fork, one charge, over whatever
      // is alive under it. A process just started has no measurement, and its share of the fork is unknown, so the fork is charged its bound again until it
      // has run a full second and the whole set has been measured — the over-charge is at most a second per new process, and never an undercount. A grant
      // whose toolchain spawns a new process every few hundred milliseconds stays at its bound for that phase, which is the safe direction.
      val f = forkOrThrow(fork)
      update(f.copy(pids = f.pids + pid, state = ForkState.Starting, pidSinceMs = deps.clock()))

    case Event.ForkWorkFinished(fork, cpu) =>
      val f = forkOrThrow(fork)
      require(f.busyCpu >= cpu, s"fork ${fork.value} holds ${f.busyCpu} cpu, cannot give back $cpu")
      update(f.copy(busyCpu = f.busyCpu - cpu))

    case Event.ForkExited(fork) =>
      forkOrThrow(fork): Unit
      state = state.copy(forks = state.forks.filterNot(_.id == fork))
  }

  def tick(): Unit = {
    val now = deps.clock()
    if (!idle) decideTick(now)
    deps.mode match {
      case coop: SchedulingMode.Cooperative =>
        // A claiming tick read the others under the lock just now; otherwise the slow check keeps the picture fresh enough for relief.
        if (lastSlowCheckMs.forall(last => now - last >= deps.slowCheckIntervalMs)) slowCheck(coop, now)
        relief(now)
      case SchedulingMode.Unconstrained(_) => ()
    }
  }

  /** The slow check (design §5.1): one probe call and the other servers' `state.json` without the lock. The probe is skipped when this tick already took one.
    * An idle server publishes its idleness here, and yields through [[yieldIfLongestIdle]] when every condition holds.
    */
  private def slowCheck(coop: SchedulingMode.Cooperative, now: Long): Unit = {
    lastSlowCheckMs = Some(now)
    if (!lastMachine.exists(_.nowMs == now)) lastMachine = Some(machineView(coop, coop.machineProbe.sample(), now))
    lastOthers = coop.discovery.others()
    if (idle) deps.observer.quiet(now)
    if (idle && !state.shuttingDown) {
      val idleness = deps.idleness()
      val idleSince = Option.when(idleness.nonObserverConnections == 0)(idleness.lastActivityEpochMs)
      publishIfChanged(coop.ownSocketDir, idleStateJson(now, idleSince, shuttingDown = false))
      val need = lastMachine.flatMap(view => MemoryNeed.of(view.pressure, lastOthers))
      if (Yield.candidate(idleness, schedulerIdle = idle, need, now, deps.idleYieldAfterMs)) yieldIfLongestIdle(coop, now, idleness.lastActivityEpochMs)
    }
  }

  /** Under the lock, with the others fresh (design §5.1, "one server yields per tick, decided under the lock"): re-read the need, and go only if no other
    * server is already going and none has been idle longer. Going means `shuttingDown` in this server's state.json before the lock is released, so the next
    * holder sees it — then the effect, after the release, which takes the daemon down its clean path.
    */
  private def yieldIfLongestIdle(coop: SchedulingMode.Cooperative, now: Long, idleSinceEpochMs: Long): Unit = {
    val (lockState, yielded) = coop.lock.locked(coop.lockWaitMs) { (lockState, timer) =>
      lockState match {
        case LockState.Held =>
          val view = timer.step("probe")(machineView(coop, coop.machineProbe.sample(), now))
          val others = timer.step("read")(coop.discovery.others())
          lastMachine = Some(view)
          lastOthers = others
          liveServersSeen = others.size + 1
          val need = MemoryNeed.of(view.pressure, others)
          val goes = need.isDefined && Yield.goes(idleSinceEpochMs, deps.identity.pid, others)
          if (goes) {
            state = state.copy(shuttingDown = true)
            timer.step("write")(publishIfChanged(coop.ownSocketDir, idleStateJson(now, Some(idleSinceEpochMs), shuttingDown = true)))
          }
          (lockState, need.filter(_ => goes))
        case LockState.Unavailable(_) | LockState.NotNeeded => (lockState, None)
      }
    }
    lastLock = lockState
    lockState match {
      case LockState.Unavailable(holder)        => deps.effects.lockUnavailable(holder)
      case LockState.Held | LockState.NotNeeded => ()
    }
    yielded.foreach(need => deps.effects.yieldServer(need, idleForMs = now - idleSinceEpochMs))
  }

  /** What an idle server publishes: nothing scheduled, and since when it has been idle. */
  private def idleStateJson(now: Long, idleSinceEpochMs: Option[Long], shuttingDown: Boolean): StateJson =
    StateJson(
      version = StateJson.CurrentVersion,
      pid = deps.identity.pid,
      startedAtEpochMs = deps.identity.startedAtEpochMs,
      bleepVersion = deps.identity.bleepVersion,
      updatedAtEpochMs = now,
      requests = 0,
      cpuInUse = 0,
      wantsMore = false,
      shuttingDown = shuttingDown,
      forks = Nil,
      idleSinceEpochMs = idleSinceEpochMs
    )

  /** Memory needed elsewhere, as of the last reading (design §5.2): shed what this server holds for nobody. Once per slow-check interval while it lasts — the
    * caches refill only when a workspace is used again, and a shed with nothing to shed is cheap but its log line is not.
    */
  private def relief(now: Long): Unit =
    lastMachine.flatMap(view => MemoryNeed.of(view.pressure, lastOthers)).foreach { need =>
      if (lastShedMs.forall(last => now - last >= deps.slowCheckIntervalMs)) {
        lastShedMs = Some(now)
        deps.effects.shedIdleCaches(need)
      }
    }

  private def decideTick(now: Long): Unit = {
    state = state.copy(heap = deps.heapUsage())
    val params = deps.params()

    var claimed = false
    var holdBreakdownMs: List[(String, Long)] = Nil
    val (decision, lockState, view) = deps.mode match {
      case SchedulingMode.Unconstrained(_) =>
        // Nothing machine-wide exists in this mode (design §9.1): no probe, no lock, no file, by construction — the mode carries none of them.
        (Decide.decide(Machine.Unconstrained(now), state, params, deps.heapGate, deps.identity), LockState.NotNeeded, Option.empty[MachineView])

      case coop: SchedulingMode.Cooperative =>
        state = measure(coop.forkProbe, state, now)
        if (claimsPossible(state)) {
          claimed = true
          coop.lock.locked(coop.lockWaitMs) { (lockState, timer) =>
            val sample = timer.step("probe")(coop.machineProbe.sample())
            val others = lockState match {
              case LockState.Held                                 => timer.step("read")(coop.discovery.others())
              case LockState.Unavailable(_) | LockState.NotNeeded => Nil
            }
            if (lockState == LockState.Held) {
              liveServersSeen = others.size + 1
              lastOthers = others
              lastSlowCheckMs = Some(now)
            }
            val view = machineView(coop, sample, now)
            val decision = timer.step("decide")(Decide.decide(Machine.Cooperative(view, others, lockState), state, params, deps.heapGate, deps.identity))
            timer.step("write")(publishIfChanged(coop.ownSocketDir, decision.publish))
            if (lockState == LockState.Held) holdBreakdownMs = timer.breakdown.map { case (name, nanos) => name -> nanos / 1_000_000L }
            (decision, lockState, Some(view))
          }
        } else {
          val view = machineView(coop, coop.machineProbe.sample(), now)
          val decision = Decide.decide(Machine.Cooperative(view, Nil, LockState.NotNeeded), state, params, deps.heapGate, deps.identity)
          publishIfChanged(coop.ownSocketDir, decision.publish)
          (decision, LockState.NotNeeded, Some(view))
        }
    }

    state = decision.next
    grantedTasks ++= decision.spawn.map(s => (s.demand.request, s.demand.taskId)) ++
      decision.reuse.map(r => (r.demand.request, r.demand.taskId)) ++
      decision.admitInHeap.map(a => (a.demand.request, a.demand.taskId))
    lastMachine = view.orElse(lastMachine)
    lastLock = lockState
    deps.observer.tick(TickReport.of(decision, now, claimed, lockState, holdBreakdownMs, view.map(_.pressure), liveServersSeen))

    // Effects strictly after the lock is released (design §7, §8 point 1).
    decision.evict.foreach(e => deps.effects.evict(e.fork, e.reason))
    decision.reuse.foreach(r => deps.effects.reuse(r.demand, r.fork, r.guaranteed))
    decision.spawn.foreach(s => deps.effects.spawn(s.demand, s.fork, s.guaranteed))
    decision.admitInHeap.foreach(a => deps.effects.startInHeap(a.demand, a.guaranteed))
    decision.heapDeferred.foreach(h => deps.effects.heapDeferred(h.demand, h.delayMs, h.firstDeferredAtMs))
    lockState match {
      case LockState.Unavailable(holder)        => deps.effects.lockUnavailable(holder)
      case LockState.Held | LockState.NotNeeded => ()
    }
    view.map(_.pressure) match {
      case Some(Pressure.NoSignal(reason)) if !pressureSignalWarned =>
        pressureSignalWarned = true
        deps.effects.pressureSignalMissing(reason)
      case _ => ()
    }
  }

  private def machineView(coop: SchedulingMode.Cooperative, sample: MachineSample, now: Long): MachineView =
    MachineView(physicalMb = sample.physicalMb, usedMb = sample.usedMb, pressure = Pressure.normalise(sample.pressure, coop.thresholds), nowMs = now)

  /** The file is rewritten only when this server's entry changed (design §7); `updatedAtEpochMs` alone is not a change. */
  private def publishIfChanged(ownSocketDir: Path, publish: StateJson): Unit = {
    val changed = lastPublished.forall(last => last.copy(updatedAtEpochMs = publish.updatedAtEpochMs) != publish)
    if (changed) {
      StateFile.write(ownSocketDir, publish)
      lastPublished = Some(publish)
    }
  }

  private def aggregateUnstarted(): Map[ForkKey, Int] =
    unstartedByRequest.values.flatten.groupMapReduce(_._1)(_._2)(_ + _)

  private def forkOrThrow(fork: ForkId): RunningFork =
    state.forks.find(_.id == fork).getOrElse(throw new IllegalArgumentException(s"fork ${fork.value} is not registered"))

  private def update(f: RunningFork): Unit =
    state = state.copy(forks = state.forks.map(x => if (x.id == f.id) f else x))

  private def measure(forkProbe: ForkProbe, s: MyState, now: Long): MyState =
    s.copy(forks = s.forks.map { f =>
      val due = f.state match {
        case ForkState.Starting          => now - f.pidSinceMs >= MeasureAfterMs
        case ForkState.Measured(_, atMs) => now - atMs >= MeasureAfterMs
      }
      if (f.pids.isEmpty || !due) f
      else {
        // Every live process's tree, summed; a process that is gone leaves the set. With nothing left alive the grant is still open (its task has not
        // reported it gone), so it goes back to Starting and is charged its bound — never undercounted while something could still start under it.
        val measured = f.pids.toList.flatMap(pid => Ticker.footprintOfTree(forkProbe, pid).map(pid -> _))
        if (measured.isEmpty) f.copy(pids = Set.empty, state = ForkState.Starting, pidSinceMs = now)
        else f.copy(pids = measured.map(_._1).toSet, state = ForkState.Measured(measured.map(_._2).sum, now))
      }
    })
}

object Ticker {

  /** A fork is charged its bound until it has run one full second, and remeasured at most once a second after that (design §5 rule 1). */
  val MeasureAfterMs: Long = 1000L

  /** What a fork's process costs together with everything it has spawned: a linker JVM and the node or clang it runs, a sourcegen script and whatever it shells
    * out to. Summed over the live tree through the per-pid probe — no probe measures a tree. `None` only when the root process itself is gone; a child that
    * exits mid-sweep is skipped. Pages shared within the tree are counted once per sharer, which overstates it slightly: the figure is for display and eviction
    * choice, never for the room arithmetic, and overstating is the safe direction.
    */
  def footprintOfTree(forkProbe: ForkProbe, pid: Long): Option[Long] =
    forkProbe.footprintMb(pid).map { root =>
      val handle = ProcessHandle.of(pid)
      if (!handle.isPresent) root
      else {
        val it = handle.get().descendants().iterator()
        var children = 0L
        while (it.hasNext) children += forkProbe.footprintMb(it.next().pid()).getOrElse(0L)
        root + children
      }
    }

  /** Whether this tick may add a memory claim (design §6.3): a ready fork demand nothing warm can absorb. Conservative — a demand that would then fail rule 6
    * still costs the lock — but exact enough that a server reusing warm forks, compiling, or idle never takes it.
    */
  def claimsPossible(s: MyState): Boolean =
    s.ready.exists {
      case d: ForkDemand =>
        val idleOfKey = s.forks.exists(f => f.available && f.key == d.key)
        val joinable = d.shared && s.forks.exists(f => f.shared && f.key == d.key && f.owner == d.request && !f.idle && !f.evicting)
        !idleOfKey && !joinable
      case _: InHeap => false
    }

  sealed trait Event
  object Event {
    case class RegisterRequest(id: RequestId, kind: RequestKind) extends Event
    case class UnregisterRequest(id: RequestId) extends Event
    case class SubmitReady(request: RequestId, ready: List[Demand], unstartedSuitesByKey: Map[ForkKey, Int]) extends Event
    case class InHeapFinished(request: RequestId, taskId: TaskId) extends Event
    case class ForkSpawned(fork: ForkId, pid: Long) extends Event
    case class ForkWorkFinished(fork: ForkId, cpu: Int) extends Event
    case class ForkExited(fork: ForkId) extends Event
  }

  /** How this server takes part in machine-wide scheduling (design §9.1). The machine-wide dependencies — probes, lock, state files — live only in
    * [[SchedulingMode.Cooperative]], so an unconstrained server cannot touch them: there is nothing to touch.
    *
    * Phase C selects it: `machineScheduling: cooperative | unconstrained` from the user config (not yet a `BleepConfig` field), and `Unconstrained` with a
    * reason whenever the probe module (`Probes.forThisMachine`) has no probe for this OS/architecture — reported loudly through
    * `SchedulerEffects.schedulingUnconstrained`. A missing pressure source alone does not change the mode (`Pressure.NoSignal`).
    */
  sealed trait SchedulingMode
  object SchedulingMode {

    /** @param ownSocketDir
      *   where this server's `state.json` goes
      * @param discovery
      *   the other servers' `state.json`, over the socket-dir layout
      */
    case class Cooperative(
        machineProbe: MachineProbe,
        forkProbe: ForkProbe,
        thresholds: PressureThresholds,
        lock: MachineLock,
        lockWaitMs: Long,
        ownSocketDir: Path,
        discovery: ServerDiscovery
    ) extends SchedulingMode

    /** @param reason
      *   why: the user's config, or the OS/architecture the probes cannot run on
      */
    case class Unconstrained(reason: String) extends SchedulingMode
  }

  /** Everything a tick reaches for. All of it is injectable: the runtime's tests use fake probes, a fake clock, a fake lock, temp directories and an in-memory
    * effect sink, and never spawn a process or touch the real cache dir.
    *
    * @param params
    *   re-read every tick, so a `parallelism` change in the user config applies to the next decision
    * @param clock
    *   epoch milliseconds
    * @param slowCheckIntervalMs
    *   how often a server with nothing to claim probes and reads the others (design §5.1); [[Ticker.SlowCheckIntervalMs]] in the daemon
    * @param idleness
    *   the connection registry's account of clients and last activity, read on each slow check
    * @param idleYieldAfterMs
    *   how long idle before yielding is considered; [[Yield.IdleYieldAfterMs]] in the daemon
    * @param observer
    *   told what every deciding tick did, for metrics
    */
  case class Deps(
      mode: SchedulingMode,
      identity: ServerIdentity,
      params: () => Params,
      heapGate: HeapGate,
      heapUsage: () => HeapUsage,
      clock: () => Long,
      effects: SchedulerEffects,
      tickIntervalPerServerMs: Long,
      slowCheckIntervalMs: Long,
      idleness: () => Yield.Idleness,
      idleYieldAfterMs: Long,
      observer: TickObserver
  ) {
    require(slowCheckIntervalMs > 0L, s"slowCheckIntervalMs $slowCheckIntervalMs must be positive")
    require(idleYieldAfterMs > 0L, s"idleYieldAfterMs $idleYieldAfterMs must be positive")
  }

  /** The slow check's cadence (design §5.1, "every few seconds"). PROVISIONAL: a probe call and a few small file reads every three seconds cost nothing
    * measurable, and three seconds is well inside the ~30 s it takes ZGC to hand freed heap back to the machine, so no finer cadence would show.
    */
  val SlowCheckIntervalMs: Long = 3000L
}
