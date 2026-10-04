package bleep.machine

import java.util.concurrent.ConcurrentHashMap
import scala.jdk.CollectionConverters.*

/** Every forked JVM this daemon has alive, by the scheduler's [[ForkId]], with the means to kill it.
  *
  * One per daemon, passed structurally. The scheduler's own state is the account of what each fork costs and holds; this is the daemon's handle table — what an
  * eviction instruction (`SchedulerEffects.evict`) reaches for, and what `bleep server top` can show. The component that started a fork registers it once its
  * process exists and unregisters it when the process is gone; process mechanics stay with that component (`JvmPool`).
  */
final class ForkRegistry {
  import ForkRegistry.LiveFork

  private val byId = new ConcurrentHashMap[ForkId, LiveFork]()

  /** A fork whose process exists. A [[ForkId]] registered twice is a bug: the scheduler hands each out once. */
  def register(fork: LiveFork): Unit = {
    val previous = byId.putIfAbsent(fork.id, fork)
    if (previous != null)
      throw new IllegalStateException(s"fork ${fork.id.value} ('${fork.label}', pid ${fork.pid}) is already registered as '${previous.label}'")
  }

  /** A fork that is gone. `false` when it was not registered — a pool's shutdown and its `destroy` can both reach the same fork, and the second to arrive is
    * not an error; it is simply too late.
    */
  def unregister(id: ForkId): Boolean = byId.remove(id) != null

  def get(id: ForkId): Option[LiveFork] = Option(byId.get(id))

  def live: List[LiveFork] = byId.values().asScala.toList.sortBy(_.startedAtEpochMs)

  def size: Int = byId.size()
}

object ForkRegistry {

  /** @param key
    *   what a warm fork must match to be reused, as the scheduler sees it
    * @param kill
    *   terminate it with the reason its exit is attributed to, synchronously: the process is gone and reported gone when this returns
    */
  case class LiveFork(id: ForkId, pid: Long, label: String, key: ForkKey, startedAtEpochMs: Long, kill: String => Unit)
}
