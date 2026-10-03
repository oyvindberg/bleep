package bleep.machine

import java.nio.file.{Files, Path}
import java.util.concurrent.atomic.{AtomicInteger, AtomicLong, AtomicReference}
import scala.jdk.StreamConverters.StreamHasToScala

/** Fakes for driving [[Ticker]] and [[TickRuntime]] without a real machine, process or cache dir. */
object SchedulerFakes {

  final class FakeMachineProbe(initial: MachineSample) extends MachineProbe {
    val current = new AtomicReference[MachineSample](initial)
    val samples = new AtomicInteger(0)
    val delayMs = new AtomicLong(0L)
    val failWith = new AtomicReference[Throwable](null)
    override def sample(): MachineSample = {
      samples.incrementAndGet(): Unit
      val fail = failWith.get()
      if (fail != null) throw fail
      if (delayMs.get() > 0L) Thread.sleep(delayMs.get())
      current.get()
    }
  }

  final class FakeForkProbe extends ForkProbe {
    val footprints = new AtomicReference[Map[Long, Long]](Map.empty)
    val calls = new AtomicInteger(0)
    override def footprintMb(pid: Long): Option[Long] = {
      calls.incrementAndGet(): Unit
      footprints.get().get(pid)
    }
  }

  /** Records every `locked` call and answers with whatever state it is told to. */
  final class FakeLock extends MachineLock {
    val answer = new AtomicReference[LockState](LockState.Held)
    val calls = new AtomicInteger(0)
    override def locked[A](waitMs: Long)(body: (LockState, HoldTimer) => A): A = {
      calls.incrementAndGet(): Unit
      body(answer.get(), new HoldTimer)
    }
  }

  sealed trait Effect
  object Effect {
    case class Spawn(demand: ForkDemand, fork: ForkId, guaranteed: Boolean) extends Effect
    case class Reuse(demand: ForkDemand, fork: ForkId, guaranteed: Boolean) extends Effect
    case class StartInHeap(demand: InHeap, guaranteed: Boolean) extends Effect
    case class Evict(fork: ForkId, reason: Decision.EvictReason) extends Effect
    case class HeapDeferred(demand: InHeap, delayMs: Long, firstDeferredAtMs: Long) extends Effect
    case class LockUnavailable(holder: LockHolder) extends Effect
    case class PressureSignalMissing(reason: String) extends Effect
  }

  final class RecordingEffects extends SchedulerEffects {
    private val recorded = new java.util.concurrent.ConcurrentLinkedQueue[Effect]()
    def all: List[Effect] = recorded.toArray(Array.empty[Effect]).toList
    def clear(): Unit = recorded.clear()
    override def spawn(demand: ForkDemand, fork: ForkId, guaranteed: Boolean): Unit = recorded.add(Effect.Spawn(demand, fork, guaranteed)): Unit
    override def reuse(demand: ForkDemand, fork: ForkId, guaranteed: Boolean): Unit = recorded.add(Effect.Reuse(demand, fork, guaranteed)): Unit
    override def startInHeap(demand: InHeap, guaranteed: Boolean): Unit = recorded.add(Effect.StartInHeap(demand, guaranteed)): Unit
    override def evict(fork: ForkId, reason: Decision.EvictReason): Unit = recorded.add(Effect.Evict(fork, reason)): Unit
    override def heapDeferred(demand: InHeap, delayMs: Long, firstDeferredAtMs: Long): Unit =
      recorded.add(Effect.HeapDeferred(demand, delayMs, firstDeferredAtMs)): Unit
    override def lockUnavailable(holder: LockHolder): Unit = recorded.add(Effect.LockUnavailable(holder)): Unit
    override def pressureSignalMissing(reason: String): Unit = recorded.add(Effect.PressureSignalMissing(reason)): Unit
  }

  /** A whole fake world in a temp directory: `socket/<own>` for this server, `socket/` for discovery. */
  final class World(val root: Path) {
    val bspSocketDir: Path = Files.createDirectories(root.resolve("socket"))
    val ownSocketDir: Path = Files.createDirectories(bspSocketDir.resolve("own0000"))
    val machineProbe = new FakeMachineProbe(MachineSample(physicalMb = 16_384L, usedMb = 4_096L, pressure = RawPressure.MacOs(1)))
    val forkProbe = new FakeForkProbe
    val lock = new FakeLock
    val effects = new RecordingEffects
    val clock = new AtomicLong(1_000_000L)
    val params = new AtomicReference[Params](Params(headroomMb = 2_048L, parallelism = 4, maxNewForksPerTick = 1))
    val heap = new AtomicReference[HeapUsage](HeapUsage(usedMb = 100L, maxMb = 1_000L))

    /** Not this JVM, so that a state file written by this JVM's pid counts as another live server. */
    val identity: ServerIdentity = ServerIdentity(pid = 1L, startedAtEpochMs = 1L, bleepVersion = "test")

    def deps(lockWaitMs: Long, tickIntervalPerServerMs: Long): Ticker.Deps = Ticker.Deps(
      machineProbe = machineProbe,
      forkProbe = forkProbe,
      thresholds = PressureThresholds.provisional,
      lock = lock,
      ownSocketDir = ownSocketDir,
      discovery = new ServerDiscovery(bspSocketDir, identity, () => clock.get(), listingTtlMs = 1000L),
      identity = identity,
      params = () => params.get(),
      heapGate = HeapGate.alwaysAdmit,
      heapUsage = () => heap.get(),
      clock = () => clock.get(),
      effects = effects,
      lockWaitMs = lockWaitMs,
      tickIntervalPerServerMs = tickIntervalPerServerMs
    )

    /** Another live server's state file, in its own socket dir, naming this JVM so liveness holds. */
    def otherServer(hash: String, forks: List[StateFork]): Unit = {
      val self = StateFile.selfIdentity("other")
      val dir = Files.createDirectories(bspSocketDir.resolve(hash))
      StateFile.write(
        dir,
        StateJson(
          version = 1,
          pid = self.pid,
          startedAtEpochMs = self.startedAtEpochMs,
          bleepVersion = "other",
          updatedAtEpochMs = 0L,
          requests = 1,
          cpuInUse = 1,
          wantsMore = false,
          shuttingDown = false,
          forks = forks
        )
      )
    }

    def ownState: Option[StateJson] = StateFile.read(ownSocketDir)

    def delete(): Unit = {
      val walk = Files.walk(root)
      try walk.toScala(List).sortBy(p => -p.getNameCount).foreach(p => Files.delete(p))
      finally walk.close()
    }
  }

  def withWorld[A](f: World => A): A = {
    val world = new World(Files.createTempDirectory("bleep-scheduler-test"))
    try f(world)
    finally world.delete()
  }

  /** Polls until `condition` holds or `timeoutMs` passes. */
  def eventually(timeoutMs: Long)(condition: => Boolean): Boolean = {
    val deadline = System.nanoTime() + timeoutMs * 1_000_000L
    var ok = condition
    while (!ok && System.nanoTime() < deadline) {
      Thread.sleep(2L)
      ok = condition
    }
    ok
  }
}
