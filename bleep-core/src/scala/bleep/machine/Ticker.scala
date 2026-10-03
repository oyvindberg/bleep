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
  * With no request, no fork and no in-heap task registered it returns at once: no probe, no file, no lock.
  */
final class Ticker(deps: Ticker.Deps) {
  import Ticker._

  private var state: MyState = MyState.empty
  private var unstartedByRequest: Map[RequestId, Map[ForkKey, Int]] = Map.empty
  private var lastPublished: Option[StateJson] = None
  private var liveServersSeen: Int = 1
  private var pressureSignalWarned: Boolean = false

  /** This server's view of itself, for tests and `top`. */
  def current: MyState = state

  /** The cadence the runtime ticks at while requests exist: 10 ms × the servers seen on the last claiming tick, so the machine as a whole ticks about every 10
    * ms (design §7).
    */
  def cadenceMs: Long = deps.tickIntervalPerServerMs * liveServersSeen.toLong

  def idle: Boolean = state.requests.isEmpty && state.forks.isEmpty && state.inHeap.isEmpty

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
      state = state.copy(unstartedSuitesByKey = aggregateUnstarted())

    case Event.SubmitReady(request, ready, unstarted) =>
      require(state.requests.exists(_.id == request), s"ready set submitted for unregistered request ${request.value}")
      ready.foreach(d => require(d.request == request, s"demand ${d.taskId.value} of request ${d.request.value} submitted under ${request.value}"))
      unstartedByRequest += (request -> unstarted)
      state = state.copy(ready = state.ready.filterNot(_.request == request) ++ ready, unstartedSuitesByKey = aggregateUnstarted())

    case Event.InHeapFinished(request, taskId) =>
      require(state.inHeap.exists(t => t.request == request && t.taskId == taskId), s"in-heap task ${taskId.value} of ${request.value} is not running")
      state = state.copy(inHeap = state.inHeap.filterNot(t => t.request == request && t.taskId == taskId))

    case Event.ForkSpawned(fork, pid) =>
      val f = forkOrThrow(fork)
      require(f.pid.isEmpty, s"fork ${fork.value} already has pid ${f.pid.get}, reported again as $pid")
      update(f.copy(pid = Some(pid)))

    case Event.ForkWorkFinished(fork, cpu) =>
      val f = forkOrThrow(fork)
      require(f.busyCpu >= cpu, s"fork ${fork.value} holds ${f.busyCpu} cpu, cannot give back $cpu")
      update(f.copy(busyCpu = f.busyCpu - cpu))

    case Event.ForkExited(fork) =>
      forkOrThrow(fork): Unit
      state = state.copy(forks = state.forks.filterNot(_.id == fork))
  }

  def tick(): Unit =
    if (!idle) {
      val now = deps.clock()
      state = state.copy(heap = deps.heapUsage())
      val params = deps.params()

      val (decision, lockState, pressure) = deps.mode match {
        case SchedulingMode.Unconstrained(_) =>
          // Nothing machine-wide exists in this mode (design §9.1): no probe, no lock, no file, by construction — the mode carries none of them.
          (Decide.decide(Machine.Unconstrained(now), state, params, deps.heapGate, deps.identity), LockState.NotNeeded, Option.empty[Pressure])

        case coop: SchedulingMode.Cooperative =>
          state = measure(coop.forkProbe, state, now)
          if (claimsPossible(state))
            coop.lock.locked(coop.lockWaitMs) { (lockState, timer) =>
              val sample = timer.step("probe")(coop.machineProbe.sample())
              val others = lockState match {
                case LockState.Held                                 => timer.step("read")(coop.discovery.others())
                case LockState.Unavailable(_) | LockState.NotNeeded => Nil
              }
              if (lockState == LockState.Held) liveServersSeen = others.size + 1
              val view = machineView(coop, sample, now)
              val decision = timer.step("decide")(Decide.decide(Machine.Cooperative(view, others, lockState), state, params, deps.heapGate, deps.identity))
              timer.step("write")(publishIfChanged(coop.ownSocketDir, decision.publish))
              (decision, lockState, Some(view.pressure))
            }
          else {
            val view = machineView(coop, coop.machineProbe.sample(), now)
            val decision = Decide.decide(Machine.Cooperative(view, Nil, LockState.NotNeeded), state, params, deps.heapGate, deps.identity)
            publishIfChanged(coop.ownSocketDir, decision.publish)
            (decision, LockState.NotNeeded, Some(view.pressure))
          }
      }

      state = decision.next

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
      pressure match {
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
        case ForkState.Starting          => now - f.startedAtMs >= MeasureAfterMs
        case ForkState.Measured(_, atMs) => now - atMs >= MeasureAfterMs
      }
      f.pid match {
        case Some(pid) if due =>
          forkProbe.footprintMb(pid) match {
            case Some(footprint) => f.copy(state = ForkState.Measured(footprint, now))
            case None            => f // exited between the decision to measure and the measurement; its exit event is on its way
          }
        case _ => f
      }
    })
}

object Ticker {

  /** A fork is charged its bound until it has run one full second, and remeasured at most once a second after that (design §5 rule 1). */
  val MeasureAfterMs: Long = 1000L

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
    */
  case class Deps(
      mode: SchedulingMode,
      identity: ServerIdentity,
      params: () => Params,
      heapGate: HeapGate,
      heapUsage: () => HeapUsage,
      clock: () => Long,
      effects: SchedulerEffects,
      tickIntervalPerServerMs: Long
  )
}
