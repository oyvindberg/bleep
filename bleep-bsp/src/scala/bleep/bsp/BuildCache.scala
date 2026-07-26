package bleep.bsp

import bleep.{model, BleepException, Started}
import ryddig.Logger

import java.nio.file.Path
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicLong
import scala.jdk.CollectionConverters.*

/** Resolved builds, cached for the lifetime of the daemon rather than of a connection.
  *
  * This used to be a `ConcurrentHashMap[Path, Started]` instance field on `MultiWorkspaceBspServer`, which is constructed per connection. Two consequences,
  * both fixed here:
  *
  *   - Every one-shot `bleep compile` opened a fresh connection and therefore re-resolved the whole build from scratch. Only a long-lived IDE session ever hit
  *     the cache.
  *   - It was keyed by workspace root alone — not by variant, and not by build identity — and both load paths early-returned the cached entry. A client that
  *     initialized with a *different* build for an already-loaded workspace silently got the old one.
  *
  * Keyed by (workspace, variant) and versioned by [[BuildId]], so "the same build again" is a cache hit and "a different build" is an explicit, logged
  * adoption.
  */
/** @param maxWorkspaces
  *   how many entries to keep before evicting the least recently used idle one. A resolved build is not small — exploded model, resolved classpaths, and the
  *   Zinc analysis reachable from it — and this cache used to be unbounded, so a daemon accumulated one per workspace it had ever served and never gave any of
  *   it back. Measured on a daemon serving 11 worktrees: the post-GC floor climbed from 3.5GB to 8.2GB of a 12GB heap over 45 minutes, after which compiles
  *   OOM'd at a concurrency of three. Bounding the cache bounds that floor.
  */
class BuildCache(maxWorkspaces: Int) {

  private case class Key(workspace: Path, variant: model.BuildVariant)
  private case class Entry(buildId: BuildId, started: Started, lastUsedMs: AtomicLong)

  private val entries = new ConcurrentHashMap[Key, Entry]()

  /** One monitor per key, so loading a build for one workspace does not block another. Deliberately not `entries.synchronized` or `entries.compute`: the load
    * can take seconds, and both of those would serialize unrelated workspaces.
    *
    * A `cats.effect.std.Mutex` would be the better fit, but every caller is currently a synchronous BSP handler, so an `IO`-returning `getOrLoad` would have to
    * be `unsafeRunSync`'d from inside the `IO.interruptible` that already wraps request dispatch — adding another nested-runtime site of exactly the kind the
    * handler-to-IO refactor exists to remove. Blocking here is safe in the meantime: this runs on the blocking pool, the load underneath it is blocking work
    * (coursier) either way, and nothing inside the monitor touches `IO` or re-enters this cache. Revisit together with that refactor.
    */
  private val loadLocks = new ConcurrentHashMap[Key, AnyRef]()

  /** Look up the build for this workspace+variant, loading it if absent or if the client means a different one.
    *
    * `load` is only invoked on a miss or on adoption. It runs while holding the per-key lock, so concurrent connections asking for the same build resolve it
    * once rather than racing.
    *
    * On adoption the previous entry is replaced but operations already in flight are left alone: each captured its own `Started` when it started and continues
    * against it. We deliberately do NOT stall the new client until they finish — an IDE connecting should not block behind a five-minute test run. Writes to
    * shared output directories stay serialized by `ProjectLock`, and Zinc recompiles when it sees a changed setup.
    */
  def getOrLoad(
      workspace: Path,
      variant: model.BuildVariant,
      buildId: BuildId,
      logger: Logger
  )(load: => Either[BleepException, Started]): Either[BleepException, Started] = {
    val key = Key(workspace, variant)

    loadLocks.computeIfAbsent(key, _ => new AnyRef).synchronized {
      Option(entries.get(key)) match {
        case Some(entry) if entry.buildId == buildId =>
          entry.lastUsedMs.set(System.currentTimeMillis())
          Right(entry.started)

        case existing =>
          existing.foreach { stale =>
            val inFlight = SharedWorkspaceState.getActiveOperations(workspace).size
            logger
              .withContext("workspace", workspace.toString)
              .withContext("variant", variant.toString)
              .withContext("from", stale.buildId.short)
              .withContext("to", buildId.short)
              .withContext("operationsInFlight", inFlight)
              .info("Adopting a different build for this workspace")
          }
          load.map { started =>
            entries.put(key, Entry(buildId, started, new AtomicLong(System.currentTimeMillis())))
            evictDownToBound(keep = key, logger)
            started
          }
      }
    }
  }

  /** Drop the entry for a workspace+variant, so the next `getOrLoad` reloads. Used by `workspace/reload`. */
  def evict(workspace: Path, variant: model.BuildVariant): Unit =
    entries.remove(Key(workspace, variant)): Unit

  /** The workspaces currently held, for telemetry. Distinct: one workspace can hold several variants, but the interesting quantity is how many builds' worth of
    * state is resident.
    */
  def cachedWorkspaces: List[String] =
    entries.keySet().iterator().asScala.map(_.workspace.toString).toList.distinct.sorted

  /** The bound in force, so telemetry can record what was being enforced rather than only what resulted. */
  def bound: Int = maxWorkspaces

  /** Evict least-recently-used entries until at most [[maxWorkspaces]] remain.
    *
    * Two entries are never candidates: the one just loaded, and any whose workspace has operations in flight. The second is not a correctness requirement —
    * every in-flight operation captured its own `Started` when it began and runs against that, exactly as it does when a client adopts a different build — but
    * evicting a workspace that is mid-build only means reloading it moments later, which is pure waste.
    *
    * Because of that exclusion the cache CAN exceed its bound: with more busy workspaces than slots, nothing is evictable and the bound yields rather than
    * stalling a build. It is a cache size, not an admission limit, and a daemon can still serve any number of workspaces at once.
    *
    * Synchronized on `entries` rather than on the per-key load locks: this pass touches every key, it holds no I/O, and taking the per-key lock of a workspace
    * we are about to drop would invert the lock order that `getOrLoad` establishes.
    */
  private def evictDownToBound(keep: Key, logger: Logger): Unit =
    entries.synchronized {
      val present = entries.entrySet().iterator().asScala.map(e => (e.getKey, e.getValue.lastUsedMs.get())).toVector
      val doomed = BuildCache.selectEvictions(
        present = present,
        keep = keep,
        bound = maxWorkspaces,
        isBusy = key => SharedWorkspaceState.getActiveOperations(key.workspace).nonEmpty
      )
      doomed.foreach { case (key, lastUsedMs) =>
        entries.remove(key): Unit
        logger
          .withContext("workspace", key.workspace.toString)
          .withContext("variant", key.variant.toString)
          .withContext("idleSeconds", (System.currentTimeMillis() - lastUsedMs) / 1000)
          .withContext("cacheSize", entries.size())
          .withContext("maxWorkspaces", maxWorkspaces)
          .info("Evicting a cached build to bound retained heap; it will be reloaded on next use")
        BspMetrics.recordCacheEvict("buildCache", key.workspace.toString)
      }
    }
}

object BuildCache {

  /** Which entries to drop so that at most `bound` remain: least recently used first, never `keep`, never a busy one.
    *
    * Pure and separate from the cache so the policy can be tested without standing up a resolved build. Returns fewer than needed — possibly none — when too
    * many entries are busy, which is deliberate: the bound yields to work in progress rather than stalling it.
    */
  private[bsp] def selectEvictions[K](
      present: Vector[(K, Long)],
      keep: K,
      bound: Int,
      isBusy: K => Boolean
  ): Vector[(K, Long)] = {
    val overBy = present.size - bound
    if (overBy <= 0) Vector.empty
    else
      present
        .filter { case (key, _) => key != keep && !isBusy(key) }
        .sortBy { case (_, lastUsedMs) => lastUsedMs }
        .take(overBy)
  }
}
