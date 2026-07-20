package bleep.bsp

import bleep.{model, BleepException, Started}
import ryddig.Logger

import java.nio.file.Path
import java.util.concurrent.ConcurrentHashMap

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
class BuildCache {

  private case class Key(workspace: Path, variant: model.BuildVariant)
  private case class Entry(buildId: BuildId, started: Started)

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
            entries.put(key, Entry(buildId, started))
            started
          }
      }
    }
  }

  /** Drop the entry for a workspace+variant, so the next `getOrLoad` reloads. Used by `workspace/reload`. */
  def evict(workspace: Path, variant: model.BuildVariant): Unit =
    entries.remove(Key(workspace, variant)): Unit
}
