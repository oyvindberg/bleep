package bleep.machine

import ryddig.Logger

import java.util.concurrent.ConcurrentLinkedQueue
import java.util.concurrent.atomic.{AtomicBoolean, AtomicReference}
import java.util.concurrent.locks.LockSupport

/** The scheduler's thread (design §7): dedicated, not the cats-effect compute pool, so a saturated compile pool cannot delay a lock holder.
  *
  * Callers queue [[Ticker.Event]]s and unpark the thread; the thread drains the queue, ticks once, and parks — indefinitely while there is nothing to schedule,
  * else for the cadence. Many events between two ticks coalesce into one tick, like `TaskDag`'s wakeup queue. A park permit is never lost: an unpark before the
  * park makes the park return at once.
  *
  * A tick that throws — a probe that cannot read the machine, a state file that cannot be parsed — ends the runtime: the failure is logged, kept, and rethrown
  * from every later call, because a scheduler that quietly stopped deciding would leave every request waiting forever with no diagnostic.
  */
final class TickRuntime(deps: Ticker.Deps, logger: Logger) extends MachineScheduler with AutoCloseable {
  import Ticker.Event

  private val ticker = new Ticker(deps)
  private val events = new ConcurrentLinkedQueue[Event]()
  private val closed = new AtomicBoolean(false)
  private val failure = new AtomicReference[Throwable](null)

  private val thread: Thread = new Thread(() => loop(), TickRuntime.ThreadName)
  thread.setDaemon(true)

  def start(): Unit = thread.start()

  private def loop(): Unit =
    try
      while (!closed.get()) {
        var event = events.poll()
        while (event != null) {
          ticker(event)
          event = events.poll()
        }
        ticker.tick()
        if (closed.get()) ()
        else if (ticker.idle) LockSupport.park(this)
        else LockSupport.parkNanos(this, ticker.cadenceMs * 1_000_000L)
      }
    catch {
      case t: Throwable =>
        failure.set(t)
        logger.error(s"the machine scheduler stopped: ${t.getMessage}", t)
        throw t
    }

  private def submit(event: Event): Unit = {
    val died = failure.get()
    if (died != null) throw new IllegalStateException("the machine scheduler has stopped", died)
    if (closed.get()) throw new IllegalStateException("the machine scheduler is closed")
    events.add(event): Unit
    LockSupport.unpark(thread)
  }

  override def registerRequest(id: RequestId, kind: RequestKind): Unit = submit(Event.RegisterRequest(id, kind))
  override def unregisterRequest(id: RequestId): Unit = submit(Event.UnregisterRequest(id))
  override def submitReady(request: RequestId, ready: List[Demand], unstartedSuitesByKey: Map[ForkKey, Int]): Unit =
    submit(Event.SubmitReady(request, ready, unstartedSuitesByKey))
  override def inHeapFinished(request: RequestId, taskId: TaskId): Unit = submit(Event.InHeapFinished(request, taskId))
  override def forkSpawned(fork: ForkId, pid: Long): Unit = submit(Event.ForkSpawned(fork, pid))
  override def forkWorkFinished(fork: ForkId, cpu: Int): Unit = submit(Event.ForkWorkFinished(fork, cpu))
  override def forkExited(fork: ForkId): Unit = submit(Event.ForkExited(fork))

  /** Why the runtime stopped, if it has. */
  def failed: Option[Throwable] = Option(failure.get())

  override def close(): Unit =
    if (closed.compareAndSet(false, true)) {
      LockSupport.unpark(thread)
      thread.join()
    }
}

object TickRuntime {
  val ThreadName = "bleep-machine-scheduler"
}
