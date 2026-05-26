package bleep.bsp

import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicReference
import scala.jdk.CollectionConverters.*

/** Tracks BSP diagnostic state across compilation cycles to implement the reset protocol.
  *
  * The BSP spec requires servers to manage diagnostic lifecycle:
  *   - First diagnostic per (document, target) in a cycle: `reset = true` (clears old diagnostics)
  *   - Subsequent diagnostics for the same pair: `reset = false` (appends)
  *   - After compilation: files that had errors last cycle but not this one need empty `reset = true` to clear stale diagnostics
  *
  * Without this, clients like Metals show "sticky" errors that persist after being fixed (issue #526).
  *
  * Thread-safe: diagnostic listeners from concurrent project compilations within the same cycle can call `recordDiagnostic` safely.
  *
  * Lifetime: ONE tracker per build operation. Don't share a tracker across concurrent operations — `handleCompile` and `handleTest` running side-by-side each
  * need their own. Sharing causes `startCycle` from one to wipe the other's in-flight state.
  */
class BspDiagnosticTracker {

  /** Key for tracking: (document URI string, target URI string) */
  private type FileTarget = (String, String)

  /** Atomic swap-on-startCycle. The "current cycle" set is held inside this ref so a `startCycle` rotates the entire set in one CAS, instead of two non-atomic
    * ops (snapshot, clear) that allowed concurrent `recordDiagnostic` callers to land in the "old" set after the snapshot and get wiped by the clear.
    *
    * `previousFiles` is captured by the rotation, also atomically.
    */
  private val currentRef: AtomicReference[ConcurrentHashMap.KeySetView[FileTarget, java.lang.Boolean]] =
    new AtomicReference(ConcurrentHashMap.newKeySet())

  @volatile private var previousFiles: Set[FileTarget] = Set.empty

  /** Record a diagnostic being published. Returns the `reset` value to use.
    *
    * @return
    *   `true` for the first diagnostic per (document, target) in this cycle (client should clear old diagnostics), `false` for subsequent ones (client should
    *   append).
    */
  def recordDiagnostic(docUri: String, targetUri: String): Boolean =
    currentRef.get().add((docUri, targetUri)) // ConcurrentHashMap.KeySetView.add returns true if newly added

  /** Prepare for a new compilation cycle. Moves current cycle's files to "previous" for clearing detection. Must be called before compilation begins. */
  def startCycle(): Unit = {
    val fresh = ConcurrentHashMap.newKeySet[FileTarget]()
    val prior = currentRef.getAndSet(fresh)
    previousFiles = prior.asScala.toSet
  }

  /** Get (docUri, targetUri) pairs that had diagnostics last cycle but not this one. These need empty diagnostics with `reset = true` to clear stale errors in
    * the client.
    */
  def filesToClear(): Set[FileTarget] =
    previousFiles -- currentRef.get().asScala.toSet
}
