package bleep.machine

import bleep.machine.Decision.{AdmitInHeap, Evict, EvictReason, HeapDeferred, Reuse, Spawn}

/** The decision rules of `machine-scheduler-design.md` §5, as one pure function. No clock, no files, no processes: every input is data, and the runtime carries
  * the result out after it has released the lock.
  */
object Decide {

  /** @param view
    *   the machine, probed under the lock when one is held
    * @param others
    *   every other live server's published state; empty on a tick that took no lock (nothing to claim, so nothing to count against)
    * @param me
    *   this server
    * @param lock
    *   whether this tick may claim new memory
    * @param heapGate
    *   the compile heap gate's policy
    */
  def decide(
      view: MachineView,
      others: List[StateJson],
      me: MyState,
      lock: LockState,
      params: Params,
      heapGate: HeapGate,
      identity: ServerIdentity
  ): Decision = {
    val now = view.nowMs
    val registered = me.requests.map(_.id).toSet
    me.ready.foreach(d => require(registered.contains(d.request), s"ready demand ${d.taskId.value} belongs to unregistered request ${d.request.value}"))
    require(me.ready.map(d => (d.request, d.taskId)).distinct.size == me.ready.size, "ready demands must be unique per request and task")

    // ---- rule 1: room. Every fork still charged at its bound, on every server, is spent; the measured ones are in usedMb already.
    val ceiling = view.physicalMb - params.headroomMb
    var room: Long = ceiling - view.usedMb - others.map(_.startingBoundMb).sum - me.forks.collect { case f if f.state == ForkState.Starting => f.boundMb }.sum

    // ---- the working set this tick admits into. Local mutation only; the function is pure from outside.
    var forks: List[RunningFork] = me.forks
    var evicted: List[Evict] = Nil
    var inHeap: List[InHeapRunning] = me.inHeap
    var cpuInUse: Int = inHeap.map(_.cpu).sum + forks.map(_.busyCpu).sum
    var nextForkId: Long = me.nextForkId
    var reuses: List[Reuse] = Nil
    var spawns: List[Spawn] = Nil
    var admitted: List[AdmitInHeap] = Nil
    var deferred: List[HeapDeferred] = Nil
    var deferredSince: Map[TaskId, Long] = me.heapDeferredSince
    var granted: Set[(RequestId, TaskId)] = Set.empty
    var spawnedThisTick = 0

    /** Rule 5's "running fork" is one working for the request. Decision: an idle fork does not count — a request whose only fork sits idle while its next suite
      * is ready is not progressing, and under Elevated or Critical pressure the reuse that would fix that is exactly what rule 2 withholds.
      */
    def hasWorkingFork(request: RequestId): Boolean = forks.exists(f => f.owner == request && !f.idle)
    def hasRunningInHeap(request: RequestId): Boolean = inHeap.exists(_.request == request)

    /** Rule 4, free: an idle fork of the same key, this request's own before another's. */
    def idleForkFor(d: ForkDemand): Option[RunningFork] =
      forks.filter(f => f.available && f.key == d.key).sortBy(f => (f.owner != d.request, f.startedAtMs)).headOption

    /** Rule 4, cheap: the busy per-project shared fork this request's suites already run on. */
    def joinableFor(d: ForkDemand): Option[RunningFork] =
      if (!d.shared) None else forks.find(f => f.shared && f.key == d.key && f.owner == d.request && !f.idle && !f.evicting)

    def takeReuse(d: ForkDemand, fork: RunningFork, guaranteed: Boolean): Unit = {
      forks = forks.map(f => if (f.id == fork.id) f.copy(owner = d.request, busyCpu = f.busyCpu + d.cpu) else f)
      cpuInUse += d.cpu
      reuses = reuses :+ Reuse(d, fork.id, guaranteed)
      granted += ((d.request, d.taskId))
    }

    def takeSpawn(d: ForkDemand, guaranteed: Boolean): Unit = {
      val id = ForkId(nextForkId)
      nextForkId += 1
      forks = forks :+ RunningFork(
        id = id,
        pid = None,
        owner = d.request,
        key = d.key,
        kind = d.kind,
        boundMb = d.boundMb,
        shared = d.shared,
        state = ForkState.Starting,
        busyCpu = d.cpu,
        startedAtMs = now,
        evicting = false
      )
      room -= d.boundMb
      cpuInUse += d.cpu
      spawns = spawns :+ Spawn(d, id, guaranteed)
      granted += ((d.request, d.taskId))
    }

    def takeInHeap(d: InHeap, guaranteed: Boolean): Unit = {
      inHeap = inHeap :+ InHeapRunning(d.request, d.taskId, d.kind, d.cpu)
      cpuInUse += d.cpu
      admitted = admitted :+ AdmitInHeap(d, guaranteed)
      deferredSince -= d.taskId
      granted += ((d.request, d.taskId))
    }

    val requestsInOrder = me.requests.sortBy(r => (r.startedAtMs, r.id.value))
    val readyByRequest: Map[RequestId, List[Demand]] = me.ready.groupBy(_.request)

    def guaranteeCandidate(r: Request): Option[ForkDemand] =
      if (hasWorkingFork(r.id)) None else readyByRequest.getOrElse(r.id, Nil).collectFirst { case d: ForkDemand => d }

    // ---- rule 5 by reuse, before rule 2/3's evictions. Decision: the design orders the guarantee after the eviction, which under Critical pressure would
    // evict a warm idle fork and then spawn a cold one for the same key — more memory, not less. Reusing first is free and strictly safer; the guaranteed
    // *spawns* still come after the evictions, as designed.
    requestsInOrder.foreach(r => guaranteeCandidate(r).foreach(d => idleForkFor(d).foreach(fork => takeReuse(d, fork, guaranteed = true))))

    // ---- rules 2 and 3: evictions that need no demand to justify them.
    // An evicted fork stays in the state, flagged, until its exit is reported: the process holds its memory until then, and a second order to kill it
    // would be a bug. Its memory is counted as room from now (rule 3: evicted *before* anything new is admitted).
    def evict(fork: RunningFork, reason: EvictReason): Unit = {
      forks = forks.map(f => if (f.id == fork.id) f.copy(evicting = true) else f)
      room += fork.reclaimableMb
      evicted = evicted :+ Evict(fork.id, reason)
    }
    forks.filter(_.available).foreach { f =>
      if (view.pressure == Pressure.Critical) evict(f, EvictReason.CriticalPressure)
      else if (me.unstartedSuitesByKey.getOrElse(f.key, 0) <= 0) evict(f, EvictReason.NothingToReuseIt)
    }

    // ---- rule 5 by spawn: one fork per command, regardless of room, cpu, pressure or lock — but a spawn is a spawn, and the tick spawns at most
    // maxNewForksPerTick of them, guarantees first, oldest request first. A request whose guarantee needs a new fork may wait several ticks for the slot.
    requestsInOrder.foreach { r =>
      guaranteeCandidate(r) match {
        case Some(d) =>
          if (spawnedThisTick < params.maxNewForksPerTick) {
            takeSpawn(d, guaranteed = true)
            spawnedThisTick += 1
          }
        case None =>
          // Only compiling: one in-heap slot the same way.
          if (!hasWorkingFork(r.id) && !hasRunningInHeap(r.id))
            readyByRequest.getOrElse(r.id, Nil).collectFirst { case d: InHeap => d }.foreach(d => takeInHeap(d, guaranteed = true))
      }
    }

    // ---- rules 6 and 7: beyond the guarantee, in priority order. Decision: priority is interleaved by rank across requests, oldest request first within a
    // rank, so one request with a long ready list cannot starve another of its second slot.
    val remaining: List[Demand] = interleave(
      requestsInOrder.map(r => readyByRequest.getOrElse(r.id, Nil).filterNot(d => granted.contains((d.request, d.taskId))))
    )
    val forksAllowed = view.pressure == Pressure.Normal
    remaining.foreach {
      case d: InHeap =>
        if (cpuInUse + d.cpu <= params.parallelism) {
          val verdict = d.kind match {
            case InHeapKind.Compile =>
              heapGate.verdict(me.heap, othersCompiling = inHeap.exists(_.kind == InHeapKind.Compile), deferredSince.get(d.taskId), now)
            case InHeapKind.Discover | InHeapKind.ResolveAnnotationProcessors => HeapVerdict.Admit
          }
          verdict match {
            case HeapVerdict.Admit          => takeInHeap(d, guaranteed = false)
            case HeapVerdict.Defer(delayMs) =>
              val first = deferredSince.getOrElse(d.taskId, now)
              deferredSince += (d.taskId -> first)
              deferred = deferred :+ HeapDeferred(d, delayMs, first)
          }
        }

      case d: ForkDemand =>
        if (forksAllowed && cpuInUse + d.cpu <= params.parallelism)
          idleForkFor(d).orElse(joinableFor(d)) match {
            case Some(fork) => takeReuse(d, fork, guaranteed = false)
            case None       =>
              if (lock == LockState.Held && spawnedThisTick < params.maxNewForksPerTick) {
                if (d.boundMb > room) {
                  // Rule 3 under shortage: idle forks, oldest first, before anything new — but only as many as make this admission possible. Decision: when
                  // even all of them would not make it fit, none is evicted; the demand waits for room, and warm forks for keys still in use stay warm.
                  val idle = forks.filter(_.available).sortBy(_.startedAtMs)
                  val needed = d.boundMb - room
                  val chosen =
                    idle.scanLeft((List.empty[RunningFork], 0L)) { case ((acc, freed), f) => (acc :+ f, freed + f.reclaimableMb) }.find(_._2 >= needed)
                  chosen.foreach { case (toEvict, _) => toEvict.foreach(f => evict(f, EvictReason.RoomShortage)) }
                }
                if (d.boundMb <= room) {
                  takeSpawn(d, guaranteed = false)
                  spawnedThisTick += 1
                }
              }
          }
    }

    // ---- rule 8: publish, with new forks as Starting at their bound so the next lock holder counts them.
    val nextReady = me.ready.filterNot(d => granted.contains((d.request, d.taskId)))
    val stillReady = nextReady.map(_.taskId).toSet
    val next = me.copy(
      forks = forks,
      inHeap = inHeap,
      ready = nextReady,
      heapDeferredSince = deferredSince.filter { case (task, _) => stillReady.contains(task) },
      nextForkId = nextForkId
    )
    val publish = StateJson(
      version = StateJson.CurrentVersion,
      pid = identity.pid,
      startedAtEpochMs = identity.startedAtEpochMs,
      bleepVersion = identity.bleepVersion,
      updatedAtEpochMs = now,
      requests = me.requests.size,
      cpuInUse = next.cpuInUse,
      wantsMore = nextReady.nonEmpty,
      shuttingDown = me.shuttingDown,
      forks = forks.map(toStateFork)
    )
    Decision(next = next, reuse = reuses, spawn = spawns, admitInHeap = admitted, evict = evicted, heapDeferred = deferred, publish = publish)
  }

  def toStateFork(f: RunningFork): StateFork =
    StateFork(
      id = f.id.value,
      pid = f.pid,
      kind = f.kind,
      boundMb = f.boundMb,
      state = f.state match {
        case ForkState.Starting               => StateForkState.Starting
        case ForkState.Measured(footprint, _) => StateForkState.Measured(footprint)
      },
      startedAtEpochMs = f.startedAtMs
    )

  /** Round-robin over the lists: every first element, then every second, ... */
  private[machine] def interleave[A](lists: List[List[A]]): List[A] = {
    val longest = if (lists.isEmpty) 0 else lists.map(_.length).max
    (0 until longest).toList.flatMap(i => lists.flatMap(_.lift(i)))
  }
}
