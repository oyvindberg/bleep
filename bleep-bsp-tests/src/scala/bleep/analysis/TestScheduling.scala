package bleep.analysis

import bleep.bsp.{DaemonScheduling, HeapWaitListener, RequestChannel, RequestRegistry}
import bleep.machine.*
import ryddig.TypedLogger

import java.nio.file.Files

/** A scheduler for DAG tests: unconstrained, with the parallelism a case asks for, so admission is exercised through the real executor path without probes,
  * locks or state files. One scheduler per channel; its tick thread is a daemon thread that parks when the request is gone.
  */
object TestScheduling {
  val noHeapWaits: HeapWaitListener = new HeapWaitListener {
    def onWait(taskId: TaskId, heap: HeapUsage, delayMs: Long, nowMs: Long): Unit = ()
    def onResume(taskId: TaskId, heap: HeapUsage, waitedForMs: Long, nowMs: Long): Unit = ()
  }

  def openChannel(parallelism: Int, heapGate: HeapGate): RequestChannel = {
    val scheduling = DaemonScheduling.unconstrained(parallelism, "dag test", heapGate, TypedLogger.DevNull)
    scheduling.openRequest(RequestId(java.util.UUID.randomUUID().toString), RequestKind.Test, noHeapWaits)
  }

  def openChannel(parallelism: Int): RequestChannel = openChannel(parallelism, HeapGate.alwaysAdmit)

  /** A cooperative scheduler for a test: this machine's real probes, and a lock, state file and discovery confined to a temp directory — so forks are measured
    * as the daemon measures them, without touching the developer's cache directory or other servers.
    */
  def openCooperative(parallelism: Int): (DaemonScheduling, RequestChannel) = {
    val root = Files.createTempDirectory("bleep-scheduler-coop")
    val identity = StateFile.selfIdentity("test")
    val probes = Probes.forThisMachine(root.resolve("native"))
    val mode = Ticker.SchedulingMode.Cooperative(
      machineProbe = probes.machine,
      forkProbe = probes.fork,
      thresholds = PressureThresholds.provisional,
      lock = new FileMachineLock(root.resolve("machine.lock"), identity, TypedLogger.DevNull),
      lockWaitMs = 1000L,
      ownSocketDir = Files.createDirectories(root.resolve("socket").resolve("own")),
      discovery = new ServerDiscovery(root.resolve("socket"), identity, () => System.currentTimeMillis(), 1000L)
    )
    val scheduling = DaemonScheduling.create(
      mode = mode,
      identity = identity,
      params = () => Params(reserveMb = 0L, maxStartingForks = 1, parallelism = parallelism, maxNewForksPerTick = 1),
      parallelism = () => parallelism,
      heapGate = HeapGate.alwaysAdmit,
      heapUsage = () => HeapUsage(usedMb = 0L, maxMb = 1024L),
      requests = new RequestRegistry,
      // These schedulers run against the developer's real machine, whose pressure is whatever it is; they hold no build cache, so there is nothing to shed.
      relief = new bleep.bsp.MemoryRelief {
        def shedIdleCaches(need: bleep.machine.MemoryNeed): Unit = ()
        def yieldServer(need: bleep.machine.MemoryNeed, idleForMs: Long): Unit =
          throw new IllegalStateException("a test scheduler with a client connected yielded")
      },
      // A client is always connected here, so these schedulers never yield; the pressure of the developer's machine must not end a test.
      idleness = () => Yield.Idleness(nonObserverConnections = 1, lastActivityEpochMs = System.currentTimeMillis()),
      observer = TickObserver.none,
      reason = None,
      logger = TypedLogger.DevNull,
      onDeath = t => throw new IllegalStateException("the test scheduler died", t)
    )
    (scheduling, scheduling.openRequest(RequestId(java.util.UUID.randomUUID().toString), RequestKind.Compile, noHeapWaits))
  }
}
