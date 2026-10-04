package bleep.bsp

import bleep.machine.*
import bleep.model.BspServerConfig
import bleep.testing.JvmPool
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import ryddig.Logger

/** The machine scheduler as one daemon runs it: the tick runtime, the bridge that carries its instructions out, the registers of requests and forks, and the
  * daemon's pool of test forks. Built once in `BspServerDaemon.runWithLock` and passed structurally to every connection (design §3, §10 step 11).
  *
  * @param requests
  *   the daemon's in-flight requests by workspace, for `bleep/status` and cancellation; every request here is also registered with the scheduler
  * @param reason
  *   why the server runs unconstrained, when it does
  */
final class DaemonScheduling private (
    val mode: Ticker.SchedulingMode,
    val runtime: TickRuntime,
    val bridge: SchedulerBridge,
    val requests: RequestRegistry,
    val forks: ForkRegistry,
    val pool: JvmPool,
    releasePool: IO[Unit],
    val parallelism: () => Int,
    val reason: Option[String]
) extends AutoCloseable {

  /** Open a request's line to the scheduler. The request closes it when it ends. */
  def openRequest(id: RequestId, kind: RequestKind, heapWaits: HeapWaitListener): RequestChannel =
    bridge.open(runtime, id, kind, heapWaits)

  /** The scheduler's view after its last tick. */
  def snapshot: Option[SchedulerSnapshot] = runtime.snapshot

  /** The mode's name as `bleep/status` and `state.json` readers spell it. */
  def modeName: String = mode match {
    case Ticker.SchedulingMode.Unconstrained(_) => bleep.bsp.protocol.SchedulerDto.Unconstrained
    case _: Ticker.SchedulingMode.Cooperative   => bleep.bsp.protocol.SchedulerDto.Cooperative
  }

  override def close(): Unit = {
    releasePool.unsafeRunSync()
    runtime.close()
    bridge.close()
  }
}

object DaemonScheduling {

  /** Start the scheduler for this daemon.
    *
    * @param config
    *   the user's server config, re-read by the caller so an edited `parallelism` or `heapPressureThreshold` applies to the next tick
    * @param onDeath
    *   what the daemon does when the scheduler stops: shut down, loudly (design §7)
    */
  def start(
      selected: MachineSchedulingSetup.Selected,
      identity: ServerIdentity,
      config: () => BspServerConfig,
      requests: RequestRegistry,
      heapUsage: () => HeapUsage,
      relief: MemoryRelief,
      idleness: () => Yield.Idleness,
      logger: Logger,
      onDeath: Throwable => Unit
  ): DaemonScheduling =
    create(
      mode = selected.mode,
      identity = identity,
      params = () => Params(headroomMb = selected.headroomMb, parallelism = config().effectiveParallelism, maxNewForksPerTick = 1),
      parallelism = () => config().effectiveParallelism,
      heapGate = HeapPressureGate.asHeapGate(() => config().effectiveHeapPressureThreshold),
      heapUsage = heapUsage,
      requests = requests,
      relief = relief,
      idleness = idleness,
      observer = SchedulerMetrics.toMetricsFile,
      reason = selected.reason,
      logger = logger,
      onDeath = onDeath
    )

  /** An unconstrained scheduler with a fixed parallelism: what in-process servers and tests run with, where coordinating with the developer's real daemons
    * through the real cache directory would be wrong.
    */
  def unconstrained(parallelism: Int, reason: String, heapGate: HeapGate, logger: Logger): DaemonScheduling =
    create(
      mode = Ticker.SchedulingMode.Unconstrained(reason),
      identity = StateFile.selfIdentity(bleep.model.BleepVersion.current.value),
      params = () => Params(headroomMb = 0L, parallelism = parallelism, maxNewForksPerTick = 1),
      parallelism = () => parallelism,
      heapGate = heapGate,
      heapUsage = () => {
        val heap = HeapMonitor.system.heapUsage()
        HeapUsage(usedMb = heap.usedMb.value, maxMb = heap.maxMb.value)
      },
      requests = new RequestRegistry,
      relief = MemoryRelief.unreachable(reason),
      // Never read: an unconstrained scheduler runs no slow check. Loud if that ever changes.
      idleness = () => throw new IllegalStateException(s"an unconstrained scheduler ($reason) asked about idleness, which only the slow check needs"),
      observer = TickObserver.none,
      reason = Some(reason),
      logger = logger,
      onDeath = t => throw new IllegalStateException("the in-process machine scheduler died", t)
    )

  /** Any mode, any params: what [[start]] and [[unconstrained]] are built from, and what a test uses to run a cooperative scheduler against temp directories
    * and real probes.
    */
  def create(
      mode: Ticker.SchedulingMode,
      identity: ServerIdentity,
      params: () => Params,
      parallelism: () => Int,
      heapGate: HeapGate,
      heapUsage: () => HeapUsage,
      requests: RequestRegistry,
      relief: MemoryRelief,
      idleness: () => Yield.Idleness,
      observer: TickObserver,
      reason: Option[String],
      logger: Logger,
      onDeath: Throwable => Unit
  ): DaemonScheduling = {
    val forks = new ForkRegistry
    val bridge = new SchedulerBridge(forks, heapUsage, relief, logger)
    val runtime = new TickRuntime(
      Ticker.Deps(
        mode = mode,
        identity = identity,
        params = params,
        heapGate = heapGate,
        heapUsage = heapUsage,
        clock = () => System.currentTimeMillis(),
        effects = bridge,
        tickIntervalPerServerMs = 10L,
        slowCheckIntervalMs = Ticker.SlowCheckIntervalMs,
        idleness = idleness,
        idleYieldAfterMs = Yield.IdleYieldAfterMs,
        observer = observer
      ),
      logger,
      onDeath
    )
    val lifecycle: ForkLifecycle = new ForkLifecycle {
      def spawned(fork: ForkId, pid: Long): Unit = runtime.forkSpawned(fork, pid)
      def workFinished(fork: ForkId, cpu: Int): Unit = runtime.forkWorkFinished(fork, cpu)
      def exited(fork: ForkId): Unit = runtime.forkExited(fork)
    }
    val (pool, releasePool) = JvmPool.create(BspMetrics.jvmPoolListener, lifecycle, forks).allocated.unsafeRunSync()
    runtime.start()
    new DaemonScheduling(mode, runtime, bridge, requests, forks, pool, releasePool, parallelism, reason)
  }
}
