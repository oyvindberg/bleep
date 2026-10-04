package bleep.bsp

import bleep.machine.*
import cats.effect.{Deferred, IO}
import cats.effect.unsafe.implicits.global
import ryddig.Logger

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
final class SchedulerBridge(forks: ForkRegistry, heapUsage: () => HeapUsage, relief: MemoryRelief, logger: Logger) extends SchedulerEffects {
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
    val channel = new RequestChannel(id, kind, scheduler, forks, heapWaits, heapUsage)
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
    route(demand.request, s"ordered a spawn of fork ${fork.value}")(_.granted(demand.taskId, Grant.Fork(ForkGrant.Spawn(fork)), guaranteed))

  override def reuse(demand: ForkDemand, fork: ForkId, guaranteed: Boolean): Unit =
    route(demand.request, s"ordered reuse of fork ${fork.value}")(_.granted(demand.taskId, Grant.Fork(ForkGrant.Reuse(fork)), guaranteed))

  override def startInHeap(demand: InHeap, guaranteed: Boolean): Unit =
    route(demand.request, s"admitted ${demand.taskId.value}")(_.granted(demand.taskId, Grant.InHeap, guaranteed))

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

/** A granted demand, by the scheduler task id the DAG submitted it under. */
case class Granted(taskId: TaskId, grant: Grant, guaranteed: Boolean)

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
    heapWaits: HeapWaitListener,
    heapUsage: () => HeapUsage
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

  /** The handle a fork task's handler reports its process(es) through; see [[GrantedFork]]. */
  def grantedFork(id: ForkId, label: String, key: ForkKey): GrantedFork = new GrantedFork(id, label, key, scheduler, forks)

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

  def granted(taskId: TaskId, grant: Grant, guaranteed: Boolean): Unit = {
    val now = System.currentTimeMillis()
    val action: () => Unit = synchronized {
      closed match {
        case Some(_) =>
          // The request ended between the scheduler's decision and this effect: give the resource straight back.
          grant match {
            case Grant.Fork(ForkGrant.Spawn(fork)) => () => scheduler.forkExited(fork)
            case Grant.Fork(ForkGrant.Reuse(fork)) => () => scheduler.forkWorkFinished(fork, cpuOf(taskId))
            case Grant.InHeap                      => () => scheduler.inHeapFinished(id, taskId)
          }
        case None =>
          forkWaits.get(taskId) match {
            case Some(wait) =>
              forkWaits -= taskId
              grant match {
                case Grant.Fork(forkGrant) =>
                  grantedForks = grantedForks.updatedWith(wait.group)(n => Some(n.getOrElse(0) + 1))
                  () => wait.deferred.complete(Right(forkGrant)).unsafeRunAndForget()
                case Grant.InHeap => throw new IllegalStateException(s"fork demand ${taskId.value} was answered as in-heap work")
              }
            case None =>
              val resumed = deferredSince.get(taskId).map(now - _)
              deferredSince -= taskId
              dagGrants.add(Granted(taskId, grant, guaranteed)): Unit
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

  private def cpuOf(taskId: TaskId): Int =
    dagReady.collectFirst { case d: ForkDemand if d.taskId == taskId => d.cpu }.getOrElse(1)

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

/** A fork the scheduler granted to a DAG task — sourcegen, KSP, link, post-compile — and how its handler reports the process running under it.
  *
  * One grant is one fork and one charge, whichever process is alive under it. A task may run several processes in a row (a sourcegen task runs its scripts one
  * after another; a Kotlin/Native link runs `konanc`), and each is reported as it starts: the scheduler charges the bound again until the newcomer has run a
  * second and been measured, and the registry's entry points at it, so an eviction or a cancel kills the process that exists now. Processes a toolchain starts
  * on its own (clang under a Scala Native link) are not reported, but they are children of the reported one where there is one, and the scheduler measures the
  * live tree; where the link runs in the server's own heap, nothing is reported and the fork stays charged at its bound — the server's heap is in the machine's
  * used memory already, so that only overstates.
  *
  * `kill` destroys the current process forcibly; the task's own cancellation path then sees it exit. Only idle forks are evicted and these are never idle, so
  * this is for cancellation and `top`.
  */
final class GrantedFork(val id: ForkId, label: String, key: ForkKey, scheduler: MachineScheduler, forks: ForkRegistry) {
  private val current = new java.util.concurrent.atomic.AtomicReference[Process](null)

  /** A process now runs under this grant. Reported for measurement and registered for eviction, cancellation and `top`; a previous process's entry is replaced.
    */
  def started(process: Process): Unit = {
    current.set(process)
    forks.unregister(id): Unit
    forks.register(
      ForkRegistry.LiveFork(
        id = id,
        pid = process.pid(),
        label = label,
        key = key,
        startedAtEpochMs = System.currentTimeMillis(),
        kill = _ => Option(current.get()).foreach(p => p.destroyForcibly(): Unit)
      )
    )
    scheduler.forkSpawned(id, process.pid())
  }

  /** `started` as the hook a process runner takes. */
  val onStarted: Process => Unit = started

  /** The task is over and no process runs under the grant any more. The executor also tells the scheduler the fork exited. */
  def ended(): Unit = forks.unregister(id): Unit
}
