package bleep.machine

/** What the rest of bleep-bsp will use once the scheduler is wired in (design §10, Phase C). Not wired yet: nothing in the server calls this.
  *
  * The contract, in the order a request lives through it:
  *
  *   1. `registerRequest` when a user command starts; `bleep run` registers nothing.
  *   1. `submitReady` whenever the request's DAG has a new ready set — the whole set, in priority order, replacing the previous one. It also carries how many
  *      test suites per fork key the DAG has still to start, which is what keeps a warm fork alive between them. A demand stays in the set until the scheduler
  *      grants it (through [[SchedulerEffects]]) or the next `submitReady` leaves it out.
  *   1. Granted work reports back: `inHeapFinished` when a compile/discover/resolve task ends; `forkSpawned` once the process exists and its pid is known — and
  *      again for each successor process under the same grant when a task runs several in a row (one fork, one charge, whichever process is alive);
  *      `forkWorkFinished` when a fork finishes a unit of work and gives its cpu slots back (the fork is then idle, or less busy if shared); `forkExited` when
  *      the process is gone, whether it finished, crashed or was evicted.
  *   1. `unregisterRequest` when the command ends. Its forks stay registered until they exit; idle ones are evicted on the next tick, since no unstarted suite
  *      wants them.
  *
  * Every call is non-blocking: it queues an event and wakes the tick thread. Nothing is decided on the caller's thread.
  *
  * A scheduler that dies takes the server with it: [[TickRuntime]] hands the failure to its `onDeath` callback, and the daemon must wire that to a loud
  * shutdown (log, exit). Later calls throw the stored failure meanwhile. There is no running without a scheduler — a server that stopped deciding would hold
  * every request forever.
  */
trait MachineScheduler {
  def registerRequest(id: RequestId, kind: RequestKind): Unit
  def unregisterRequest(id: RequestId): Unit
  def submitReady(request: RequestId, ready: List[Demand], unstartedSuitesByKey: Map[ForkKey, Int]): Unit
  def inHeapFinished(request: RequestId, taskId: TaskId): Unit
  def forkSpawned(fork: ForkId, pid: Long): Unit
  def forkWorkFinished(fork: ForkId, cpu: Int): Unit
  def forkExited(fork: ForkId): Unit
}

/** The scheduler's instructions to the server, carried out after the tick has released the lock. Phase C implements this against the fork registry and the DAG
  * executor; tests implement it as an in-memory sink.
  *
  * Each instruction is for a demand the executor submitted, or a fork it was told to spawn. A spawn names the [[ForkId]] the scheduler has already recorded as
  * `Starting`; the executor reports `forkSpawned` with the pid once the process is up and `forkExited` if it never gets there.
  */
trait SchedulerEffects {
  def spawn(demand: ForkDemand, fork: ForkId, guaranteed: Boolean): Unit
  def reuse(demand: ForkDemand, fork: ForkId, guaranteed: Boolean): Unit
  def startInHeap(demand: InHeap, guaranteed: Boolean): Unit
  def evict(fork: ForkId, reason: Decision.EvictReason): Unit
  def heapDeferred(demand: InHeap, delayMs: Long, firstDeferredAtMs: Long): Unit

  /** The tick wanted the lock and did not get it within the deadline. For the log, metrics and `top` (design §8 point 5). */
  def lockUnavailable(holder: LockHolder): Unit

  /** Memory is needed elsewhere (design §5.2): drop the cached build and analyses of every workspace this server has no request for. Decided from tick data
    * without the lock; it releases memory, it never claims it. Issued at most once per slow-check interval while the need lasts.
    */
  def shedIdleCaches(need: MemoryNeed): Unit

  /** This server has marked itself `shuttingDown` under the lock (design §5.1) and must now take the clean shutdown path. Issued once. */
  def yieldServer(need: MemoryNeed, idleForMs: Long): Unit

  /** The platform reports no memory pressure, so the pressure brake is off (design §9.1). Once per server, on the first tick that sees it: a loud warning. */
  def pressureSignalMissing(reason: String): Unit

  /** This server schedules unconstrained (design §9.1): no machine-wide coordination at all. Once, at start — the user asked for it, or the probes cannot run
    * on this OS/architecture, in which case this is the loud warning.
    */
  def schedulingUnconstrained(reason: String): Unit
}
