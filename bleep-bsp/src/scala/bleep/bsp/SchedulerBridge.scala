package bleep.bsp

import bleep.machine.*
import cats.effect.{Deferred, IO}
import cats.effect.unsafe.implicits.global
import ryddig.Logger

import java.nio.file.Path

import java.util.concurrent.{ConcurrentHashMap, ConcurrentLinkedQueue, Executors}
import java.util.concurrent.atomic.AtomicLong
import scala.jdk.CollectionConverters.*

/** The daemon's side of the scheduler's effects (design §10 step 11).
  *
  * The scheduler decides on its own thread and hands out instructions through [[SchedulerEffects]]; this routes each to the request it concerns, kept as a
  * [[RequestChannel]] per in-flight request. Everything here is non-blocking from the tick thread's point of view: a grant completes a `Deferred` or lands in a
  * queue and wakes the DAG loop, an eviction is handed to a thread of its own (killing a process can take seconds), and a warning is a log line.
  *
  * One per daemon, passed structurally; nothing here is global.
  */
final class SchedulerBridge(forks: ForkRegistry, heapUsage: () => HeapUsage, relief: MemoryRelief, children: ChildWatch, logger: Logger)
    extends SchedulerEffects {
  private val channels = new ConcurrentHashMap[RequestId, RequestChannel]()
  private val lastLockWarningMs = new AtomicLong(0L)

  /** Evictions run here, never on the tick thread: `kill` waits up to ten seconds for a fork to go. */
  private val evictions = Executors.newSingleThreadExecutor { r =>
    val t = new Thread(r, "bleep-scheduler-evictions")
    t.setDaemon(true)
    t
  }

  /** Register a request with the scheduler and open its channel. The channel is closed by the request when it ends. */
  def open(scheduler: MachineScheduler, id: RequestId, kind: RequestKind, heapWaits: HeapWaitListener): RequestChannel = {
    // Channels of requests that ended are kept for a tick's worth of time, so a grant decided before the scheduler heard the request end still finds a channel
    // to return the resource through, and is not dropped on the floor.
    val now = System.currentTimeMillis()
    channels.values().asScala.filter(ch => ch.closedAtMs.exists(at => now - at > RequestChannel.ClosedRetentionMs)).foreach(ch => channels.remove(ch.id))
    val channel = new RequestChannel(id, kind, scheduler, forks, children, heapWaits, heapUsage, logger)
    if (channels.putIfAbsent(id, channel) != null) throw new IllegalStateException(s"request ${id.value} is already open")
    scheduler.registerRequest(id, kind)
    channel
  }

  private def route(request: RequestId, what: String)(f: RequestChannel => Unit): Unit =
    Option(channels.get(request)) match {
      case Some(channel) => f(channel)
      case None          => throw new IllegalStateException(s"the scheduler $what for request ${request.value}, which this daemon never opened")
    }

  override def spawn(demand: ForkDemand, fork: ForkId, guaranteed: Boolean): Unit =
    route(demand.request, s"ordered a spawn of fork ${fork.value}")(_.granted(demand.taskId, demand.cpu, Grant.Fork(ForkGrant.Spawn(fork)), guaranteed))

  override def reuse(demand: ForkDemand, fork: ForkId, guaranteed: Boolean): Unit =
    route(demand.request, s"ordered reuse of fork ${fork.value}")(_.granted(demand.taskId, demand.cpu, Grant.Fork(ForkGrant.Reuse(fork)), guaranteed))

  override def startInHeap(demand: InHeap, guaranteed: Boolean): Unit =
    route(demand.request, s"admitted ${demand.taskId.value}")(_.granted(demand.taskId, demand.cpu, Grant.InHeap, guaranteed))

  override def evict(fork: ForkId, reason: Decision.EvictReason): Unit =
    forks.get(fork) match {
      case Some(live) => evictions.execute(() => live.kill(s"bleep: evicted by the machine scheduler (${reason.getClass.getSimpleName.stripSuffix("$")})"))
      case None       =>
        // Only an idle fork is evicted, and an idle fork has a registered process; one that does not is a bug worth hearing about, not a silent no-op.
        throw new IllegalStateException(s"the scheduler evicted fork ${fork.value}, which is not registered in this daemon")
    }

  override def heapDeferred(demand: InHeap, delayMs: Long, firstDeferredAtMs: Long): Unit =
    route(demand.request, s"deferred ${demand.taskId.value} on heap pressure")(_.heapDeferred(demand.taskId, delayMs, firstDeferredAtMs))

  override def lockUnavailable(holder: LockHolder): Unit = {
    val now = System.currentTimeMillis()
    // A stuck holder is reported on every tick that waited for it; once a few seconds is a warning, once every 10 ms is noise.
    if (now - lastLockWarningMs.get() > 5000L) {
      lastLockWarningMs.set(now)
      logger.warn(s"machine.lock is held by ${holder.describe}; this tick admitted only guaranteed forks and reuse")
    }
  }

  override def pressureSignalMissing(reason: String): Unit =
    logger.warn(s"This machine reports no memory pressure ($reason): the scheduler runs without the pressure brake, on used memory against the ceiling alone")

  override def schedulingUnconstrained(reason: String): Unit =
    logger.warn(s"Machine scheduling is UNCONSTRAINED: $reason")

  /** Off the tick thread: a shed walks the build cache under its monitor and releases analyses, which is not a tick's business to wait for. */
  override def shedIdleCaches(need: MemoryNeed): Unit =
    evictions.execute(() => relief.shedIdleCaches(need))

  override def yieldServer(need: MemoryNeed, idleForMs: Long): Unit =
    evictions.execute(() => relief.yieldServer(need, idleForMs))

  def close(): Unit = evictions.shutdownNow(): Unit
}

/** What a daemon does when the scheduler finds memory is needed elsewhere (design §5.1, §5.2). The caches and the shutdown path belong to the daemon, not the
  * scheduler, so they arrive here as actions it hands in — structurally, one per daemon.
  */
trait MemoryRelief {

  /** Drop the cached build and analyses of every workspace with no request in flight. */
  def shedIdleCaches(need: MemoryNeed): Unit

  /** The scheduler has marked this server `shuttingDown` under the lock: take the daemon down its clean path (socket closed, files released). */
  def yieldServer(need: MemoryNeed, idleForMs: Long): Unit
}

object MemoryRelief {

  /** For an unconstrained scheduler, which has no probe and reads no other server, so can never find a need: being asked is a bug, said loudly. */
  def unreachable(reason: String): MemoryRelief = new MemoryRelief {
    def shedIdleCaches(need: MemoryNeed): Unit =
      throw new IllegalStateException(s"an unconstrained scheduler ($reason) asked to shed caches for '${need.describe}', which it has no way of knowing")
    def yieldServer(need: MemoryNeed, idleForMs: Long): Unit =
      throw new IllegalStateException(s"an unconstrained scheduler ($reason) asked to yield for '${need.describe}', which it has no way of knowing")
  }
}

/** The scheduler's instruction to one request's DAG. */
sealed trait Grant
object Grant {
  case object InHeap extends Grant
  case class Fork(grant: ForkGrant) extends Grant
}

/** A granted demand, by the scheduler task id the DAG submitted it under; `cpu` is what the demand asked for, so a grant given back returns exactly that. */
case class Granted(taskId: TaskId, cpu: Int, grant: Grant, guaranteed: Boolean)

/** What a deferred compile's client hears: that it waits for heap, and that it resumed. The server sends these as BSP events. */
trait HeapWaitListener {
  def onWait(taskId: TaskId, heap: HeapUsage, delayMs: Long, nowMs: Long): Unit
  def onResume(taskId: TaskId, heap: HeapUsage, waitedForMs: Long, nowMs: Long): Unit
}

/** One request's line to the scheduler: what its DAG has ready and what its test handlers want forked go up as one ready set; grants come back down.
  *
  * Two kinds of demand meet here. The DAG's — compiles, discovery, resolution, and the sourcegen/KSP/link/post-compile forks — are submitted by the executor
  * whenever its ready set changes; their grants queue up for the executor, which starts the tasks. Test forks are asked for by the running test handler, which
  * knows the fork's key only once the classpath is resolved (design §11, two-stage test admission); their grants complete the handler's `Deferred`. Both are
  * one `submitReady` to the scheduler, since the guarantee and the per-tick spawn slot are per request.
  *
  * Also implements [[bleep.machine.ForkAcquirer]], which is how the pool asks on the handler's behalf.
  */
final class RequestChannel(
    val id: RequestId,
    val kind: RequestKind,
    scheduler: MachineScheduler,
    forks: ForkRegistry,
    children: ChildWatch,
    heapWaits: HeapWaitListener,
    heapUsage: () => HeapUsage,
    logger: Logger
) extends ForkAcquirer {
  import RequestChannel.ForkWait

  // Everything below is guarded by `this`; the critical sections are tiny and the tick thread is one of the callers.
  private var dagReady: List[Demand] = Nil
  private var forkWaits: Map[TaskId, ForkWait] = Map.empty
  private var pendingTests: Map[String, Int] = Map.empty
  private var grantedForks: Map[String, Int] = Map.empty
  private var keyByGroup: Map[String, ForkKey] = Map.empty
  private var deferredSince: Map[TaskId, Long] = Map.empty
  private var closed: Option[Long] = None
  private var wake: () => Unit = () => ()
  private val dagGrants = new ConcurrentLinkedQueue[Granted]()

  override def requestId: RequestId = id

  def closedAtMs: Option[Long] = synchronized(closed)

  /** The executor's hook: called when a grant for one of its demands has arrived. */
  def onGrant(wakeExecutor: () => Unit): Unit = synchronized { wake = wakeExecutor }

  /** The DAG's whole ready set, in priority order, replacing the previous one — and, per test group (a project), how many test tasks are not finished, from
    * which the scheduler learns how many suites each warm fork still has to serve.
    */
  def setDagReady(demands: List[Demand], pendingTestsByGroup: Map[String, Int]): Unit = synchronized {
    demands.foreach(d => require(d.request == id, s"demand ${d.taskId.value} of ${d.request.value} submitted on ${id.value}'s channel"))
    // A demand the scheduler has already granted, and the executor has not yet taken, is not ready: submitting it again would have it admitted twice.
    val alreadyGranted = dagGrants.iterator().asScala.map(_.taskId).toSet
    dagReady = demands.filterNot(d => alreadyGranted.contains(d.taskId))
    pendingTests = pendingTestsByGroup
    publish()
  }

  /** Grants for DAG demands since the last call. */
  def takeGrants(): List[Granted] = {
    val out = List.newBuilder[Granted]
    var next = dagGrants.poll()
    while (next != null) {
      out += next
      next = dagGrants.poll()
    }
    out.result()
  }

  /** A test task of `group` finished: its fork grant no longer counts against the group's unstarted suites. */
  def testFinished(group: String): Unit = synchronized {
    grantedForks = grantedForks.updatedWith(group)(_.map(n => math.max(0, n - 1)))
    publish()
  }

  def inHeapFinished(taskId: TaskId): Unit = scheduler.inHeapFinished(id, taskId)

  /** A DAG-level fork task (sourcegen, KSP, link, post-compile) is done: its process is gone. */
  def forkExited(fork: ForkId): Unit = scheduler.forkExited(fork)

  /** A fork granted for reuse by a task that will not run: the cpu the demand asked for goes back; the fork runs on. */
  def forkWorkFinished(fork: ForkId, cpu: Int): Unit = scheduler.forkWorkFinished(fork, cpu)

  /** The handle a fork task's handler reports its process(es) through; see [[GrantedFork]]. */
  def grantedFork(id: ForkId, label: String, key: ForkKey): GrantedFork = new GrantedFork(id, label, key, scheduler, forks, children)

  // ---- ForkAcquirer: the pool asks for a test fork on a handler's behalf

  override def acquire(demand: ForkDemand, group: String): IO[ForkGrant] =
    Deferred[IO, Either[Throwable, ForkGrant]].flatMap { deferred =>
      val submit = IO {
        synchronized {
          if (closed.isDefined) throw new IllegalStateException(s"request ${id.value} has ended; no fork for ${demand.taskId.value}")
          forkWaits += (demand.taskId -> ForkWait(demand, group, deferred))
          keyByGroup += (group -> demand.key)
          publish()
        }
      }
      val withdraw = IO {
        synchronized {
          if (forkWaits.contains(demand.taskId)) {
            forkWaits -= demand.taskId
            publish()
          }
        }
      }
      submit >> deferred.get.onCancel(withdraw).flatMap(IO.fromEither)
    }

  // ---- effects, on the tick thread

  def granted(taskId: TaskId, cpu: Int, grant: Grant, guaranteed: Boolean): Unit = {
    val now = System.currentTimeMillis()
    // A grant nobody here asked for goes straight back — a spawned fork as exited (nothing will start it), a reused one with its cpu (it runs on), a slot as
    // finished. Said with the reason, since every such grant is a scheduler decision about a demand this request no longer has.
    def giveBack(why: String): () => Unit = {
      val returning = grant match {
        case Grant.Fork(ForkGrant.Spawn(fork)) => () => scheduler.forkExited(fork)
        case Grant.Fork(ForkGrant.Reuse(fork)) => () => scheduler.forkWorkFinished(fork, cpu)
        case Grant.InHeap                      => () => scheduler.inHeapFinished(id, taskId)
      }
      () => {
        logger.warn(s"request ${id.value}: the scheduler granted $grant for ${taskId.value}, $why; giving it back")
        returning()
      }
    }
    val action: () => Unit = synchronized {
      closed match {
        case Some(_) => giveBack("but the request has ended")
        case None    =>
          forkWaits.get(taskId) match {
            case Some(wait) =>
              forkWaits -= taskId
              grant match {
                case Grant.Fork(forkGrant) =>
                  grantedForks = grantedForks.updatedWith(wait.group)(n => Some(n.getOrElse(0) + 1))
                  () => wait.deferred.complete(Right(forkGrant)).unsafeRunAndForget()
                case Grant.InHeap => throw new IllegalStateException(s"fork demand ${taskId.value} was answered as in-heap work")
              }
            case None if !dagReady.exists(_.taskId == taskId) =>
              // Neither a handler waiting for a fork nor a DAG demand on the table: a demand this channel once had and no longer wants — the executor would
              // never take it, and a fork reused for it would stay busy forever.
              giveBack("which nothing here is waiting for")
            case None =>
              val resumed = deferredSince.get(taskId).map(now - _)
              deferredSince -= taskId
              dagGrants.add(Granted(taskId, cpu, grant, guaranteed)): Unit
              val wakeNow = wake
              () => {
                resumed.foreach(waited => heapWaits.onResume(taskId, heapUsage(), waited, now))
                wakeNow()
              }
          }
      }
    }
    action()
  }

  def heapDeferred(taskId: TaskId, delayMs: Long, firstDeferredAtMs: Long): Unit = {
    val first = synchronized {
      val already = deferredSince.contains(taskId)
      deferredSince += (taskId -> firstDeferredAtMs)
      !already
    }
    val heap = heapUsage()
    val now = System.currentTimeMillis()
    // One "waiting" notice per deferral spell, not one per tick; the resume names how long it took.
    if (first) heapWaits.onWait(taskId, heap, delayMs, now)
    BspMetrics.recordAdmissionDefer(
      project = taskId.value,
      reason = "heap_pressure",
      heapUsedMb = heap.usedMb,
      heapMaxMb = heap.maxMb,
      delayMs = delayMs,
      othersCompiling = -1
    )
  }

  /** The request is over. Anything still waiting for a fork is failed; the scheduler forgets the request. */
  def close(): Unit = {
    val waiting = synchronized {
      closed = Some(System.currentTimeMillis())
      val w = forkWaits.values.toList
      forkWaits = Map.empty
      dagReady = Nil
      w
    }
    waiting.foreach(w =>
      w.deferred
        .complete(Left(new IllegalStateException(s"request ${id.value} ended before ${w.demand.taskId.value} got a fork")))
        .attempt
        .void
        .unsafeRunAndForget()
    )
    scheduler.unregisterRequest(id)
  }

  /** Everything this request wants, as one ready set: the DAG's demands first (its priority order), then the test forks handlers are waiting for. */
  private def publish(): Unit = {
    val unstarted: Map[ForkKey, Int] =
      pendingTests.toList.flatMap { case (group, pending) =>
        keyByGroup.get(group).map(key => key -> math.max(0, pending - grantedForks.getOrElse(group, 0)))
      }.toMap
    scheduler.submitReady(id, dagReady ++ forkWaits.values.toList.map(_.demand), unstarted)
  }
}

object RequestChannel {
  private case class ForkWait(demand: ForkDemand, group: String, deferred: Deferred[IO, Either[Throwable, ForkGrant]])

  /** How long an ended request's channel is kept to catch grants decided before the scheduler heard it end: a few ticks' worth, generously. */
  val ClosedRetentionMs: Long = 60_000L
}

/** A fork the scheduler granted to a DAG task — sourcegen, KSP, a native link, post-compile, a Kotlin discovery — and the processes that run under it.
  *
  * One grant, one charge, over whatever is alive under it (design §5 rule 1). A process is reported the moment it exists, through [[started]] when bleep
  * started it or [[observed]] when a toolchain did (a Scala Native link's clang and lld, attributed to this grant by [[ChildWatch]]); the scheduler charges the
  * fork its bound until the set has been measured, and the registry's entry knows every process so an eviction or a cancel reaches all of them.
  *
  * `kill` destroys every live process forcibly. For a toolchain's children that is secondary — the task's own cancellation stops the toolchain, whose next
  * `Process.!` then fails — but it is what makes `bleep server kill` and the registry's cleanup reach a clang that would otherwise outlive its server. Only
  * idle forks are evicted and these are never idle, so eviction does not reach here.
  */
final class GrantedFork(val id: ForkId, label: String, key: ForkKey, scheduler: MachineScheduler, forks: ForkRegistry, children: ChildWatch) {
  private val owned = new java.util.concurrent.ConcurrentHashMap[Long, ProcessHandle]()
  private val registered = new java.util.concurrent.atomic.AtomicBoolean(false)

  /** A process bleep started now runs under this grant. */
  def started(process: Process): Unit = observed(process.toHandle)

  /** A process that exists now runs under this grant: registered for cancellation and `top`, reported to the scheduler for measurement. Reporting the same
    * process twice is a no-op, so a watcher that scans on a cadence can report what it sees without bookkeeping of its own.
    */
  def observed(handle: ProcessHandle): Unit =
    if (owned.putIfAbsent(handle.pid(), handle) == null) {
      if (registered.compareAndSet(false, true))
        forks.register(
          ForkRegistry.LiveFork(
            id = id,
            pids = () => livePids,
            label = label,
            key = key,
            startedAtEpochMs = System.currentTimeMillis(),
            kill = _ => owned.values().forEach(h => if (h.isAlive) h.destroyForcibly(): Unit)
          )
        )
      scheduler.forkSpawned(id, handle.pid())
    }

  /** A toolchain under this grant is about to spawn processes of its own, every one naming a path under `dir`: have the daemon's [[ChildWatch]] attribute the
    * server's children to this grant for as long as the result is open (design §5.3).
    */
  def observeChildrenUnder(dir: Path): AutoCloseable = children.claim(this, dir)

  /** The processes under this grant that are still alive. */
  def livePids: Set[Long] = owned.values().asScala.filter(_.isAlive).map(_.pid()).toSet

  /** `started` as the hook a process runner takes. */
  val onStarted: Process => Unit = started

  /** The task is over and no process runs under the grant any more. The executor also tells the scheduler the fork exited. */
  def ended(): Unit = forks.unregister(id): Unit
}
