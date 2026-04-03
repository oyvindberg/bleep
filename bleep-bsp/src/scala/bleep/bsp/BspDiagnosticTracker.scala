package bleep.bsp

import java.util.concurrent.ConcurrentHashMap
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
  * Thread-safe: diagnostic listeners from concurrent project compilations can call `recordDiagnostic` safely.
  */
class BspDiagnosticTracker {

  /** Key for tracking: (document URI string, target URI string) */
  private type FileTarget = (String, String)

  /** Files that had diagnostics in the previous compilation cycle */
  @volatile private var previousFiles: Set[FileTarget] = Set.empty

  /** Files that have received at least one diagnostic in the current cycle. `add()` returns true if the element was newly added (= first diagnostic = reset). */
  private val currentFiles: ConcurrentHashMap.KeySetView[FileTarget, java.lang.Boolean] =
    ConcurrentHashMap.newKeySet()

  /** Record a diagnostic being published. Returns the `reset` value to use.
    *
    * @return
    *   `true` for the first diagnostic per (document, target) in this cycle (client should clear old diagnostics), `false` for subsequent ones (client should
    *   append).
    */
  def recordDiagnostic(docUri: String, targetUri: String): Boolean =
    currentFiles.add((docUri, targetUri)) // ConcurrentHashMap.KeySetView.add returns true if newly added

  /** Prepare for a new compilation cycle. Moves current cycle's files to "previous" for clearing detection. Must be called before compilation begins. */
  def startCycle(): Unit = {
    previousFiles = Set.from(currentFiles.asScala)
    currentFiles.clear()
  }

  /** Get (docUri, targetUri) pairs that had diagnostics last cycle but not this one. These need empty diagnostics with `reset = true` to clear stale errors in
    * the client.
    */
  def filesToClear(): Set[FileTarget] =
    previousFiles -- currentFiles.asScala.toSet
}
