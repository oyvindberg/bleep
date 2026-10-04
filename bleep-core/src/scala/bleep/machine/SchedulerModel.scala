package bleep.machine

/** The scheduler's model: plain data, nothing else. [[Decide.decide]] is a pure function of these. See `machine-scheduler-design.md` §4.
  *
  * Two resources, two scopes (design §3.1): memory is machine-wide and governs forks only; cpu (`parallelism`) is per server and governs everything that runs.
  */

/** One user command (`bleep test`, `bleep compile`, ...). `bleep run` creates none. */
case class RequestId(value: String) extends AnyVal

/** A task in a request's DAG. Opaque here; the executor owns the real identity. */
case class TaskId(value: String) extends AnyVal

/** A fork this server started, numbered by this server. */
case class ForkId(value: Long) extends AnyVal

/** What makes a warm fork reusable for a demand: classpath, jvm options, env and cwd hashed together, exactly as `JvmPool.JvmKey` does today. */
case class ForkKey(value: String) extends AnyVal

sealed trait RequestKind
object RequestKind {
  case object Compile extends RequestKind
  case object Test extends RequestKind
  case object Link extends RequestKind
  case object Sourcegen extends RequestKind
  case object Script extends RequestKind
}

case class Request(id: RequestId, kind: RequestKind, startedAtMs: Long)

/** The kinds of process the scheduler charges to the machine. `json` is the `state.json` spelling, read by every bleep version from here on. */
sealed abstract class ForkKind(val json: String)
object ForkKind {
  case object TestSuite extends ForkKind("test-suite")
  case object TestBatch extends ForkKind("test-batch")
  case object Sourcegen extends ForkKind("sourcegen")
  case object AnnotationProcessor extends ForkKind("annotation-processor")
  case object Ksp extends ForkKind("ksp")
  case object Link extends ForkKind("link")
  case object PostCompile extends ForkKind("post-compile")

  val all: List[ForkKind] = List(TestSuite, TestBatch, Sourcegen, AnnotationProcessor, Ksp, Link, PostCompile)

  def fromJson(s: String): ForkKind =
    all.find(_.json == s).getOrElse(throw new IllegalArgumentException(s"unknown fork kind '$s'; known: ${all.map(_.json).mkString(", ")}"))
}

/** Work that runs inside the server's own heap. Only `Compile` is subject to the heap gate. */
sealed trait InHeapKind
object InHeapKind {
  case object Compile extends InHeapKind
  case object Discover extends InHeapKind
  case object ResolveAnnotationProcessors extends InHeapKind
}

/** Something a request's DAG could start now. A request's demands are submitted in the DAG's priority order. */
sealed trait Demand {
  def request: RequestId
  def taskId: TaskId
  def cpu: Int
}

/** Compile, discover, annotation-processor resolution: runs in the server's heap, which is already in the machine's `usedMb`. Needs a cpu slot and, for a
  * compile, the heap gate's consent. Never memory room, never the lock (design §5 rule 7).
  */
case class InHeap(request: RequestId, taskId: TaskId, kind: InHeapKind, cpu: Int) extends Demand {
  require(cpu >= 1, s"in-heap demand ${taskId.value} asks for $cpu cpu; work holds at least one slot")
}

/** A process to fork.
  *
  * @param key
  *   what a warm fork must match to be reused for this demand
  * @param boundMb
  *   the fork's footprint ceiling: `-Xmx` plus non-heap overhead (`heap + max(256, heap/4)`). Charged to the machine from admission until measured.
  * @param cpu
  *   slots the fork holds while it runs this work
  * @param shared
  *   a per-project shared fork: suites of the same request and key join it while it is busy instead of each getting a fork (`JvmPool.acquireShared`).
  */
case class ForkDemand(request: RequestId, taskId: TaskId, kind: ForkKind, key: ForkKey, boundMb: Long, cpu: Int, shared: Boolean) extends Demand {
  require(cpu >= 1, s"fork demand ${taskId.value} asks for $cpu cpu; work holds at least one slot, so a fork running it is never idle")
  require(boundMb >= 1L, s"fork demand ${taskId.value} has bound ${boundMb}MB")
}

sealed trait ForkState
object ForkState {

  /** Charged at `boundMb` (design §5 rule 1). */
  case object Starting extends ForkState

  /** Its memory is in the machine's `usedMb`. `footprintMb` is for display and eviction choice, never prediction. Remeasured at most every second. */
  case class Measured(footprintMb: Long, atMs: Long) extends ForkState
}

/** A fork this server runs.
  *
  * @param pid
  *   `None` until the process has been spawned and reported back
  * @param owner
  *   the request it currently works for; a reused idle fork changes owner
  * @param busyCpu
  *   cpu slots held by the work it runs now; `0` is idle. A shared fork holds as many as the suites running on it.
  * @param evicting
  *   the scheduler has ordered it killed; it stays here, neither reusable nor evictable again, until `forkExited` — the process holds its memory until then.
  * @param pidSinceMs
  *   when the process now running under this fork started. A fork is one grant and one charge; the process under it may be succeeded by another (a sourcegen
  *   task runs its scripts one after the other), and each successor is charged at the bound again until it has run a full second and been measured.
  */
case class RunningFork(
    id: ForkId,
    pid: Option[Long],
    owner: RequestId,
    key: ForkKey,
    kind: ForkKind,
    boundMb: Long,
    shared: Boolean,
    state: ForkState,
    busyCpu: Int,
    startedAtMs: Long,
    evicting: Boolean,
    pidSinceMs: Long
) {
  def idle: Boolean = busyCpu == 0

  /** Idle and still the scheduler's to hand out or to evict. */
  def available: Boolean = idle && !evicting

  /** What evicting it gives back to the machine: the whole charge while Starting, the measurement afterwards. */
  def reclaimableMb: Long = state match {
    case ForkState.Starting               => boundMb
    case ForkState.Measured(footprint, _) => footprint
  }
}

/** An in-heap task this server is running, holding `cpu` slots until it finishes. */
case class InHeapRunning(request: RequestId, taskId: TaskId, kind: InHeapKind, cpu: Int)

/** This server's heap, for the heap gate. */
case class HeapUsage(usedMb: Long, maxMb: Long)

/** Everything this server knows about itself. Owned by the tick runtime, changed only through [[Decide.decide]] and lifecycle events.
  *
  * @param ready
  *   every request's ready demands, in each request's DAG priority order
  * @param unstartedSuitesByKey
  *   how many test suites, across all requests, still have to start on a fork of each key — what decides whether an idle fork is worth keeping warm
  * @param heapDeferredSince
  *   when the heap gate first deferred each still-waiting compile, so its wait is bounded across ticks (`HeapPressureGate.MaxWaitMs`)
  * @param nextForkId
  *   the id the next spawned fork gets
  * @param shuttingDown
  *   this server has decided to yield (design §5.1). Published; not acted on here yet.
  */
case class MyState(
    requests: List[Request],
    forks: List[RunningFork],
    inHeap: List[InHeapRunning],
    ready: List[Demand],
    unstartedSuitesByKey: Map[ForkKey, Int],
    heap: HeapUsage,
    heapDeferredSince: Map[TaskId, Long],
    nextForkId: Long,
    shuttingDown: Boolean
) {
  def cpuInUse: Int = inHeap.map(_.cpu).sum + forks.map(_.busyCpu).sum
  def compilesRunning: Int = inHeap.count(_.kind == InHeapKind.Compile)
}

object MyState {
  val empty: MyState = MyState(
    requests = Nil,
    forks = Nil,
    inHeap = Nil,
    ready = Nil,
    unstartedSuitesByKey = Map.empty,
    heap = HeapUsage(usedMb = 0L, maxMb = 0L),
    heapDeferredSince = Map.empty,
    nextForkId = 1L,
    shuttingDown = false
  )
}

/** One reading of the machine, taken under the lock on a claiming tick so it includes every earlier claim. */
case class MachineView(physicalMb: Long, usedMb: Long, pressure: Pressure, nowMs: Long)

/** The outcome of trying for `machine.lock` this tick (design §6.3, §8). */
sealed trait LockState
object LockState {

  /** This tick holds the lock: it may claim new memory. */
  case object Held extends LockState

  /** The deadline passed with another server holding it. Guarantees, reuse and eviction still apply; nothing beyond them. */
  case class Unavailable(holder: LockHolder) extends LockState

  /** This tick had nothing to claim and did not try (design §6.3). Decision: a third state rather than overloading `Unavailable`, so a tick that chose not to
    * lock is distinguishable in logs and metrics from one that was refused.
    */
  case object NotNeeded extends LockState
}

/** Who held `machine.lock` when a waiter gave up, as read from the announcement in the file (design §8 point 3). */
sealed trait LockHolder {
  def describe: String
}

object LockHolder {

  /** The holder had written its announcement: a real, possibly stuck, holder. */
  case class Announced(pid: Long, startedAtEpochMs: Long, heldForMs: Long) extends LockHolder {
    def describe: String = s"pid $pid for ${heldForMs}ms"
  }

  /** The announcement was blank when the waiter looked. A holder blanks it just before releasing and writes it just after acquiring, so this is a holder in the
    * act of letting go or of taking the lock — a transient, not a stuck process, and nothing to name. Modelled rather than defaulted so that a log line saying
    * "unannounced" is a statement about the file, not a guess.
    */
  case object Unannounced extends LockHolder {
    def describe: String = "an unannounced holder (just released, or not yet announced)"
  }
}

/** The machine-wide half of a decision's inputs — or its absence. Design §9.1: `unconstrained` runs the same tick and `decide` without the machine-wide parts,
  * so those parts are one input that is either there or not, rather than three parameters a mode flag would have `decide` half-ignore. With
  * [[Machine.Unconstrained]] there is no room, no pressure brake and no lock to wait for; parallelism, the heap gate, warm reuse, idle eviction, one spawn per
  * tick and the guarantee are all that remain.
  */
sealed trait Machine {
  def nowMs: Long
}

object Machine {

  /** @param view
    *   the machine, probed under the lock when one is held
    * @param others
    *   every other live server's published state; empty on a tick that took no lock (nothing to claim, so nothing to count against)
    * @param lock
    *   whether this tick may claim new memory
    */
  case class Cooperative(view: MachineView, others: List[StateJson], lock: LockState) extends Machine {
    def nowMs: Long = view.nowMs
  }

  case class Unconstrained(nowMs: Long) extends Machine
}

/** The scheduler's tunables.
  *
  * @param headroomMb
  *   `ceiling = physical − headroom`. OPEN (design §11): the design's single tunable input.
  * @param parallelism
  *   this server's cpu slots, from the user config, re-read on change. CPU only, per server.
  * @param maxNewForksPerTick
  *   new forks per tick, guaranteed ones included (design: 1). Guaranteed spawns take the slot first, oldest request first; a guarantee that needs a new fork
  *   may take several ticks to be met. Reusing a warm fork is not a spawn and is not counted.
  */
case class Params(headroomMb: Long, parallelism: Int, maxNewForksPerTick: Int) {
  require(headroomMb >= 0L, s"headroomMb $headroomMb is negative")
  require(parallelism >= 1, s"parallelism $parallelism is below 1")
  require(maxNewForksPerTick >= 0, s"maxNewForksPerTick $maxNewForksPerTick is negative")
}

/** Who this server is, for `state.json`. */
case class ServerIdentity(pid: Long, startedAtEpochMs: Long, bleepVersion: String)

/** The compile heap gate's policy, as a pure function. `HeapPressureGate.decide` (bleep-bsp) is the real one; Phase C adapts it. A parameter of
  * [[Decide.decide]] rather than a call, because the gate lives in bleep-bsp and the scheduler in bleep-core — and because the decision stays testable with a
  * gate that always admits or always defers.
  */
trait HeapGate {
  def verdict(heap: HeapUsage, othersCompiling: Boolean, firstDeferredAtMs: Option[Long], nowMs: Long): HeapVerdict
}

sealed trait HeapVerdict
object HeapVerdict {
  case object Admit extends HeapVerdict

  /** Not now; `delayMs` is the stagger the gate would have slept, reported so the "waiting for memory" event carries a duration. */
  case class Defer(delayMs: Long) extends HeapVerdict
}

object HeapGate {
  val alwaysAdmit: HeapGate = (_, _, _, _) => HeapVerdict.Admit
}

/** A fork as published in `state.json` (design §6.2). `pid` is absent while the process has not been spawned yet. */
case class StateFork(
    id: Long,
    pid: Option[Long],
    kind: ForkKind,
    boundMb: Long,
    state: StateForkState,
    startedAtEpochMs: Long
)

sealed trait StateForkState
object StateForkState {
  case object Starting extends StateForkState
  case class Measured(footprintMb: Long) extends StateForkState
}

/** `state.json`, schema v1 (design §6.2). Every version must keep `pid`, `startedAtEpochMs` (liveness), `forks` and `cpuInUse`.
  *
  * @param wantsMore
  *   this server has ready demands it could not admit this tick
  * @param shuttingDown
  *   this server has decided to yield its memory and is shutting down (design §5.1); its forks still count until it is gone. Published from here on so that
  *   readers of v1 know the field; nothing sets it yet.
  */
case class StateJson(
    version: Int,
    pid: Long,
    startedAtEpochMs: Long,
    bleepVersion: String,
    updatedAtEpochMs: Long,
    requests: Int,
    cpuInUse: Int,
    wantsMore: Boolean,
    shuttingDown: Boolean,
    forks: List[StateFork]
) {

  /** What the next lock holder must count: forks still charged at their bound. */
  def startingBoundMb: Long = forks.collect { case f if f.state == StateForkState.Starting => f.boundMb }.sum
}

object StateJson {
  val CurrentVersion: Int = 1
}

/** What a tick decided. The runtime applies `next` as its state, writes `publish`, then — after releasing the lock — carries out the instructions. */
case class Decision(
    next: MyState,
    reuse: List[Decision.Reuse],
    spawn: List[Decision.Spawn],
    admitInHeap: List[Decision.AdmitInHeap],
    evict: List[Decision.Evict],
    heapDeferred: List[Decision.HeapDeferred],
    publish: StateJson
)

object Decision {

  /** Run `demand` on the existing fork `fork`: an idle fork of the same key, or a busy shared fork of the same key and owner (a join). */
  case class Reuse(demand: ForkDemand, fork: ForkId, guaranteed: Boolean)

  /** Start a new fork for `demand`; it is already in `next.forks` as `Starting` with id `fork`. */
  case class Spawn(demand: ForkDemand, fork: ForkId, guaranteed: Boolean)

  /** Start `demand` in the server's heap; it is already in `next.inHeap`. */
  case class AdmitInHeap(demand: InHeap, guaranteed: Boolean)

  case class Evict(fork: ForkId, reason: EvictReason)

  sealed trait EvictReason
  object EvictReason {

    /** Idle, and no unstarted suite would reuse it (rule 3). */
    case object NothingToReuseIt extends EvictReason

    /** Idle, and a new fork needs its memory (rule 3). */
    case object RoomShortage extends EvictReason

    /** Idle under Critical pressure (rule 2). */
    case object CriticalPressure extends EvictReason
  }

  case class HeapDeferred(demand: InHeap, delayMs: Long, firstDeferredAtMs: Long)
}

/** What the scheduler last decided from, published after every tick for `bleep/status` and metrics. Plain data, read from any thread.
  *
  * @param machine
  *   the last machine reading, absent in unconstrained mode and before the first probing tick
  * @param liveServers
  *   other servers counted on the last claiming tick, plus this one
  */
case class SchedulerSnapshot(
    mode: String,
    state: MyState,
    params: Params,
    machine: Option[MachineView],
    lock: LockState,
    liveServers: Int,
    ready: List[Demand]
)
