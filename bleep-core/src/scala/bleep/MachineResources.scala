package bleep

import cats.effect.{Deferred, IO, Poll, Ref, Resource}
import cats.syntax.all._
import ryddig.Logger

/** The finite resources of the machine this bleep-bsp server runs on — CPU cores and memory for forked processes — that every concurrent operation competes
  * for.
  *
  * Before this existed, each subsystem gated itself independently: compiles took permits from one `Semaphore(numCores)`, each client's test run took permits
  * from ITS OWN `Semaphore(numCores)`, and forked sourcegen/KSP JVMs were ungated. Together they could reserve several times the machine's actual cores and,
  * for forks, several times its RAM — the fork storms and swap-death seen under multi-agent load. They all draw from the SAME physical machine, so they must
  * draw from the same accounting.
  *
  * Two dimensions are modelled:
  *   - '''cpu''' (cores): reserved by everything CPU-bound — a compile and a forked test JVM compete here equally, because they run on the same cores.
  *   - '''memoryMb''': reserved by forked *processes* only. This is off-server-heap RAM (the budget is physical RAM minus the server's own heap minus an OS
  *     reserve). Compiles run inside the server JVM and are bounded by its `-Xmx`, tracked separately by [[bleep.bsp.HeapPressureGate]]; they reserve 0 here.
  *
  * Implemented purely on cats-effect: all state lives in a single [[Ref]]; a reservation that can't be granted immediately parks on a [[Deferred]] and is
  * completed by whoever frees the resources it needs. The park is `poll(gate.get)` under `Resource.makeFull` (whose `poll` is what keeps the wait cancelable —
  * plain `Resource.make` runs acquisition uncancelably and would hang a cancelled reservation), so cancelling the waiting IO (a cancelled test run, say)
  * dequeues the reservation cleanly via `onCancel` — no thread blocking, no interruption. Grants are work-conserving and oldest-first: a cheap CPU-only compile
  * is never stuck behind a large memory-hungry fork it doesn't compete with, but among requests that DO fit the oldest wins, so nothing starves. Every state is
  * observable via [[snapshot]].
  */
final class MachineResources private (
    val totalCpu: Int,
    val totalMemoryMb: Long,
    /** Memory weight to charge a forked JVM that declares no `-Xmx` — a JVM left uncapped can grow to ~¼ of physical RAM, so that is what it must reserve, or
      * the budget would under-count it.
      */
    val defaultForkMemoryMb: Long,
    state: Ref[IO, MachineResources.St],
    logger: Logger,
    longWaitWarnMs: Long
) {
  import MachineResources._

  /** Reserve `cpu` cores and `memoryMb` of fork memory for the lifetime of the returned Resource. Semantically waits until both fit; the request is clamped to
    * the machine totals so asking for more than the machine has waits for the whole machine rather than never being grantable.
    */
  def reserve(kind: ResourceKind, label: String, cpu: Int, memoryMb: Long): Resource[IO, Unit] =
    // makeFull (not make): make runs acquisition fully uncancelable, which would make the wait below
    // uninterruptible and a cancelled reservation hang forever. makeFull hands us a `poll` so only
    // the enqueue is uncancelable and the actual waiting stays cancelable.
    Resource.makeFull[IO, Long](poll => acquire(poll, kind, label, cpu, memoryMb))(id => dispose(id)).void

  private def acquire(poll: Poll[IO], kind: ResourceKind, label: String, cpu: Int, memoryMb: Long): IO[Long] = {
    val cpuReq = math.max(0, math.min(cpu, totalCpu))
    val memReq = math.max(0L, math.min(memoryMb, totalMemoryMb))
    for {
      now <- IO.realTime.map(_.toMillis)
      gate <- Deferred[IO, Unit]
      // Enqueue + grant is a single atomic Ref update (uncancelable — makeFull masks it).
      result <- state.modify { st =>
        val id = st.nextId
        val queued = st.copy(nextId = id + 1, waiting = st.waiting :+ Waiter(id, kind, label, cpuReq, memReq, now, gate))
        val (next, granted) = grantEligible(queued)
        (next, (id, granted))
      }
      (id, granted) = result
      _ <- completeAll(granted)
      // If we weren't granted synchronously, park until someone completes our gate. `poll` makes
      // this the one cancelable point; on cancel `dispose` dequeues us (and frees us if we raced to
      // being granted). The release below also runs `dispose`, which is idempotent.
      _ <-
        if (granted.exists(_.id == id)) IO.unit
        else poll(gate.get).onCancel(dispose(id))
    } yield id
  }

  /** Release or cancel reservation `id`, then grant whoever was waiting on the freed resources. Idempotent, and handles both states: still-queued (drop from
    * the wait queue, nothing was held) and already-granted (return the resources).
    */
  private def dispose(id: Long): IO[Unit] =
    state
      .modify { st =>
        if (st.waiting.exists(_.id == id))
          (st.copy(waiting = st.waiting.filterNot(_.id == id)), Nil)
        else
          st.active.get(id) match {
            case Some(r) =>
              val freed = st.copy(freeCpu = st.freeCpu + r.cpu, freeMemoryMb = st.freeMemoryMb + r.memoryMb, active = st.active - id)
              grantEligible(freed)
            case None => (st, Nil)
          }
      }
      .flatMap(completeAll)

  /** Complete the gates of newly-granted waiters (wakes them), logging each grant. Runs outside any state update.
    */
  private def completeAll(granted: List[Waiter]): IO[Unit] =
    granted.traverse_ { w =>
      val log =
        if (w.sinceMs > 0) {
          IO.realTime.map(_.toMillis - w.sinceMs).flatMap { waited =>
            if (waited >= longWaitWarnMs) IO(logger.info(s"[machine] granted '${w.label}' (${w.kind}) after waiting ${waited}ms"))
            else IO(logger.debug(s"[machine] grant '${w.label}' (${w.kind}) cpu=${w.cpu} mem=${w.memoryMb}MB"))
          }
        } else IO.unit
      log *> w.gate.complete(()).void
    }

  /** A consistent point-in-time view of what's running and what's queued, for logging / metrics / debugging "why is my build stalled".
    */
  def snapshot: IO[Snapshot] =
    (state.get, IO.realTime.map(_.toMillis)).mapN { (st, now) =>
      Snapshot(
        totalCpu = totalCpu,
        usedCpu = totalCpu - st.freeCpu,
        totalMemoryMb = totalMemoryMb,
        usedMemoryMb = totalMemoryMb - st.freeMemoryMb,
        active = st.active.values.toList.sortBy(_.id).map(r => Entry(r.kind, r.label, r.cpu, r.memoryMb, now - r.sinceMs)),
        waiting = st.waiting.toList.map(w => Entry(w.kind, w.label, w.cpu, w.memoryMb, now - w.sinceMs))
      )
    }

  /** True when work is queued waiting for resources — the signal worth logging periodically. */
  def isContended: IO[Boolean] = state.get.map(_.waiting.nonEmpty)
}

object MachineResources {

  sealed trait ResourceKind
  object ResourceKind {
    case object Compile extends ResourceKind
    case object TestFork extends ResourceKind
    case object SourcegenFork extends ResourceKind
    case object KspFork extends ResourceKind
  }

  private case class Reservation(id: Long, kind: ResourceKind, label: String, cpu: Int, memoryMb: Long, sinceMs: Long)
  private case class Waiter(id: Long, kind: ResourceKind, label: String, cpu: Int, memoryMb: Long, sinceMs: Long, gate: Deferred[IO, Unit])

  /** All governor state, held in one Ref. */
  private case class St(
      freeCpu: Int,
      freeMemoryMb: Long,
      nextId: Long,
      active: Map[Long, Reservation],
      waiting: Vector[Waiter]
  )

  /** Grant every currently-fitting waiter, oldest first (work-conserving: a waiter that doesn't fit is skipped rather than head-of-line-blocking). Pure:
    * returns the updated state and the waiters that were granted (whose gates the caller then completes).
    */
  private def grantEligible(st: St): (St, List[Waiter]) = {
    var freeCpu = st.freeCpu
    var freeMem = st.freeMemoryMb
    var active = st.active
    val granted = List.newBuilder[Waiter]
    val stillWaiting = Vector.newBuilder[Waiter]
    st.waiting.foreach { w =>
      if (freeCpu >= w.cpu && freeMem >= w.memoryMb) {
        freeCpu -= w.cpu
        freeMem -= w.memoryMb
        active = active.updated(w.id, Reservation(w.id, w.kind, w.label, w.cpu, w.memoryMb, w.sinceMs))
        granted += w
      } else stillWaiting += w
    }
    (st.copy(freeCpu = freeCpu, freeMemoryMb = freeMem, active = active, waiting = stillWaiting.result()), granted.result())
  }

  case class Entry(kind: ResourceKind, label: String, cpu: Int, memoryMb: Long, ageMs: Long)

  case class Snapshot(
      totalCpu: Int,
      usedCpu: Int,
      totalMemoryMb: Long,
      usedMemoryMb: Long,
      active: List[Entry],
      waiting: List[Entry]
  ) {

    /** Multi-line human-readable rendering for the server log. */
    def render: String = {
      val sb = new StringBuilder
      sb.append(f"machine: cpu $usedCpu%d/$totalCpu%d, mem $usedMemoryMb%d/$totalMemoryMb%dMB, running ${active.size}%d, waiting ${waiting.size}%d")
      def line(prefix: String, e: Entry): Unit =
        sb.append(f"\n  $prefix%-8s ${e.kind}%-14s cpu=${e.cpu}%d mem=${e.memoryMb}%dMB age=${e.ageMs / 1000}%ds  ${e.label}")
      active.foreach(line("running", _))
      waiting.foreach(line("waiting", _))
      sb.toString()
    }
  }

  /** Build a governor for this machine.
    *
    * @param totalCpu
    *   number of CPU cores available for concurrent work (typically `availableProcessors`).
    * @param totalMemoryMb
    *   RAM budget in MB for forked processes (physical RAM minus the server's own heap minus an OS reserve — see [[forkMemoryBudgetMb]]).
    * @param defaultForkMemoryMb
    *   memory to charge a forked JVM that declares no `-Xmx`.
    * @param longWaitWarnMs
    *   log at INFO (rather than DEBUG) when a reservation is finally granted after waiting at least this long.
    */
  /** The wait after which a finally-granted reservation is logged at INFO rather than DEBUG. */
  val DefaultLongWaitWarnMs: Long = 30000L

  def create(
      totalCpu: Int,
      totalMemoryMb: Long,
      defaultForkMemoryMb: Long,
      logger: Logger,
      longWaitWarnMs: Long
  ): MachineResources = {
    val cpu = math.max(1, totalCpu)
    val mem = math.max(1L, totalMemoryMb)
    val defFork = math.max(1L, math.min(defaultForkMemoryMb, mem))
    logger.info(s"[machine] resource governor: $cpu CPU core(s), ${mem}MB fork-memory budget, default fork weight ${defFork}MB")
    val state = Ref.unsafe[IO, St](St(freeCpu = cpu, freeMemoryMb = mem, nextId = 0L, active = Map.empty, waiting = Vector.empty))
    new MachineResources(cpu, mem, defFork, state, logger, longWaitWarnMs)
  }

  /** Governor sized for the machine this JVM is running on: `totalCpu` cores, and a fork-memory budget of physical RAM minus this JVM's own max heap minus an
    * OS reserve. The single place that derivation lives — callers that need to override a dimension (the daemon's `BLEEP_FORK_MEMORY_BUDGET_MB`) call
    * [[create]] directly.
    */
  def forThisMachine(totalCpu: Int, logger: Logger): MachineResources = {
    val ownHeapMb = Runtime.getRuntime.maxMemory() / (1024L * 1024L)
    val physicalMb = physicalMemoryMb(fallbackMb = ownHeapMb * 2)
    create(
      totalCpu = totalCpu,
      totalMemoryMb = forkMemoryBudgetMb(physicalMb, ownHeapMb),
      defaultForkMemoryMb = math.max(512L, physicalMb / 4),
      logger = logger,
      longWaitWarnMs = DefaultLongWaitWarnMs
    )
  }

  /** Parse a heap-size string (`"512m"`, `"2g"`, `"1500m"`, optionally `-Xmx`-prefixed) into MB. Used to weight a forked JVM by its configured max heap.
    *
    * `None` means "no size stated here" — the caller then charges the default fork weight. It does NOT mean "unparseable": a string that looks like a size but
    * isn't one we understand throws, because silently mis-weighting a fork is exactly how the budget stops bounding anything. An unknown suffix is the
    * dangerous case — reading `2t` as 2MB under-counts a fork a millionfold, and the governor would happily admit hundreds of them.
    */
  def parseMemoryMb(raw0: String): Option[Long] = {
    val raw = raw0.trim.stripPrefix("-Xmx").trim
    if (raw.isEmpty) None
    else {
      val (num, unit) = raw.span(_.isDigit)
      if (num.isEmpty) throw new IllegalArgumentException(s"cannot parse memory size '$raw0': expected digits, optionally suffixed with k/m/g")
      val n =
        try num.toLong
        catch { case _: NumberFormatException => throw new IllegalArgumentException(s"cannot parse memory size '$raw0': '$num' is not a valid number") }
      Some(unit.trim.toLowerCase match {
        case "g" | "gb" => n * 1024L
        case "m" | "mb" => n
        case "k" | "kb" => math.max(1L, n / 1024L)
        case ""         => math.max(1L, n / (1024L * 1024L)) // bare number is bytes, per -Xmx semantics
        case other      => throw new IllegalArgumentException(s"cannot parse memory size '$raw0': unknown unit '$other' (expected k/m/g)")
      })
    }
  }

  /** Total physical RAM in MB.
    *
    * `fallbackMb` covers the one genuinely-expected miss: a JVM whose OperatingSystemMXBean isn't the `com.sun` one that exposes total memory. An *exception*
    * from the platform bean is not that case and is not swallowed — it would mean the budget is being computed from a number we never actually read.
    */
  def physicalMemoryMb(fallbackMb: Long): Long =
    java.lang.management.ManagementFactory.getOperatingSystemMXBean match {
      case os: com.sun.management.OperatingSystemMXBean => os.getTotalMemorySize / (1024L * 1024L)
      case _                                            => fallbackMb
    }

  /** What a forked JVM asking for `-Xmx heapMb` actually costs the machine.
    *
    * `-Xmx` bounds the *heap*, not the process. On top of it a JVM commits metaspace, the code cache, a stack per thread, direct/mapped byte buffers, and the
    * GC's own bookkeeping — for a large heap that is comfortably another 10-25%. Charging bare `-Xmx` therefore under-counts every fork, and the error
    * compounds: two "12GB" forks admitted against a 36GB budget were really ~30GB resident, which is how they got killed by the OS while the governor still
    * believed it had 11GB spare.
    *
    * Deliberately an over-estimate rather than an under-estimate: being wrong in this direction serializes a suite that could have run in parallel, while being
    * wrong in the other direction kills it with SIGKILL and no diagnostic.
    */
  def forkFootprintMb(heapMb: Long): Long =
    heapMb + math.max(256L, heapMb / 4)

  /** Default fork-memory budget: physical RAM, minus the server's own max heap, minus a reserve for everything that is not bleep.
    *
    * The reserve is a quarter of RAM (at least 4GB) because a bleep-bsp daemon does not own the machine it runs on. The previous tenth-of-RAM covered the OS
    * and nothing else, which is only true on a dedicated CI box; on the developer machine this actually runs on there is also an IDE, a browser, and several
    * agent processes, so the headroom the governor thought it had did not exist. Set BLEEP_FORK_MEMORY_BUDGET_MB to override when bleep really does own the
    * machine.
    */
  def forkMemoryBudgetMb(physicalMb: Long, serverHeapMb: Long): Long = {
    val reserve = math.max(4096L, physicalMb / 4)
    math.max(1024L, physicalMb - serverHeapMb - reserve)
  }
}
