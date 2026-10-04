package bleep.machine

import java.util.concurrent.ConcurrentHashMap
import scala.jdk.CollectionConverters.*

/** Every forked JVM this daemon has alive, in one place.
  *
  * One per daemon, constructed in `BspServerDaemon.runWithLock` and passed structurally to every connection's server and on to each request's `JvmPool`. The
  * pools still make every decision as before — reservation, eviction, pooling, start limiting — and only report here what they started and what they stopped;
  * process mechanics stay in `ManagedJvm`. Design §3.2, §10 step 10: in Phase C this is where the scheduler sees every fork, so a [[ForkRegistry.LiveFork]]
  * carries what a scheduler `RunningFork` needs — identity, key, bound, start time — and the means to kill it.
  */
final class ForkRegistry {
  import ForkRegistry.LiveFork

  private val byPid = new ConcurrentHashMap[Long, LiveFork]()

  /** A fork that has just been started. A pid already registered is a bug: either the owner forgot to unregister, or two pools claim one process. */
  def register(fork: LiveFork): Unit = {
    val previous = byPid.putIfAbsent(fork.pid, fork)
    if (previous != null) throw new IllegalStateException(s"fork pid ${fork.pid} ('${fork.label}') is already registered as '${previous.label}'")
  }

  /** A fork that is gone. `false` when it was not registered — a pool's shutdown and its `destroy` can both reach the same fork, and the second to arrive is
    * not an error; it is simply too late.
    */
  def unregister(pid: Long): Boolean = byPid.remove(pid) != null

  def live: List[LiveFork] = byPid.values().asScala.toList.sortBy(_.startedAtEpochMs)

  def size: Int = byPid.size()
}

object ForkRegistry {

  /** @param key
    *   what a warm fork must match to be reused: the pool's key (classpath, options, environment, cwd), hashed
    * @param heapBoundMb
    *   the `-Xmx` the fork was started with, when one was given
    * @param kill
    *   terminate it, with the reason the fork's exit will be attributed to
    */
  case class LiveFork(pid: Long, label: String, key: String, heapBoundMb: Option[Long], startedAtEpochMs: Long, kill: String => Unit)
}
