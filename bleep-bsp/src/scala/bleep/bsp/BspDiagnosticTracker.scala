package bleep.bsp

import java.util.concurrent.ConcurrentHashMap
import scala.jdk.CollectionConverters.*

/** What diagnostics the client currently has on screen, per build target — for the lifetime of a connection.
  *
  * Diagnostics in BSP are stateful on the client side: once we publish an error for a file it stays in the user's editor until we publish an empty
  * `reset = true` for that same file. So the knowledge of "what needs clearing" spans compilations, and cannot live in the per-operation
  * [[BspDiagnosticTracker]] — the compile that fixes an error is a *different* operation from the one that reported it. Without this, an error you have already
  * fixed stays on screen in Metals (issue #526).
  *
  * Bloop solves the same problem the same way, keeping `previouslyFailedCompilations` per client and splicing it into each request.
  *
  * Keyed by target so one project's compile never clears another's diagnostics.
  */
class BspDiagnosticMemory {
  private val docsByTarget = ConcurrentHashMap[String, Set[String]]()

  /** Documents currently carrying diagnostics for `targetUri`. */
  def docsFor(targetUri: String): Set[String] =
    docsByTarget.getOrDefault(targetUri, Set.empty)

  /** Replace what we remember about `targetUri`, now that a compile of it has published everything it is going to. */
  def replace(targetUri: String, docUris: Set[String]): Unit =
    if (docUris.isEmpty) docsByTarget.remove(targetUri): Unit
    else docsByTarget.put(targetUri, docUris): Unit
}

/** Tracks BSP diagnostic state within one build operation to implement the reset protocol.
  *
  * The BSP spec requires servers to manage diagnostic lifecycle:
  *   - First diagnostic per (document, target) in a cycle: `reset = true` (clears old diagnostics)
  *   - Subsequent diagnostics for the same pair: `reset = false` (appends)
  *   - After compilation: files that had diagnostics before but not now need an empty `reset = true` to clear the stale ones
  *
  * Lifetime: ONE tracker per build operation. Don't share a tracker across concurrent operations — `handleCompile` and `handleTest` running side-by-side each
  * need their own, or one wipes the other's in-flight state. The state that *must* outlive the operation lives in the shared [[BspDiagnosticMemory]] instead.
  *
  * Thread-safe: diagnostic listeners from concurrent project compilations within the same operation can call `recordDiagnostic` safely.
  */
class BspDiagnosticTracker(memory: BspDiagnosticMemory) {

  /** Documents this cycle has published diagnostics for, per target uri. */
  private val current = ConcurrentHashMap[String, ConcurrentHashMap.KeySetView[String, java.lang.Boolean]]()

  private def docsOf(targetUri: String): ConcurrentHashMap.KeySetView[String, java.lang.Boolean] =
    current.computeIfAbsent(targetUri, _ => ConcurrentHashMap.newKeySet[String]())

  /** Announce that `targetUri` is about to be compiled, so this cycle takes responsibility for clearing its stale diagnostics.
    *
    * Needed separately from `recordDiagnostic` because the interesting case is a target that compiles *clean*: it publishes nothing, yet its previous errors
    * are exactly the ones to clear. Targets that are not compiled at all (up-to-date, noop) must not be announced — their diagnostics still stand.
    */
  def beginTarget(targetUri: String): Unit =
    docsOf(targetUri): Unit

  /** Record a diagnostic being published. Returns the `reset` value to use.
    *
    * @return
    *   `true` for the first diagnostic per (document, target) in this cycle (client should clear old diagnostics), `false` for subsequent ones (client should
    *   append).
    */
  def recordDiagnostic(docUri: String, targetUri: String): Boolean =
    docsOf(targetUri).add(docUri) // ConcurrentHashMap.KeySetView.add returns true if newly added

  /** End the cycle: hand back the (docUri, targetUri) pairs that carried diagnostics before but don't now — these need empty `reset = true` notifications — and
    * commit this cycle's state as the new memory.
    */
  def finishCycle(): Set[(String, String)] =
    current.asScala.toMap.flatMap { case (targetUri, docsView) =>
      val docs = docsView.asScala.toSet
      val stale = memory.docsFor(targetUri) -- docs
      memory.replace(targetUri, docs)
      stale.map(docUri => (docUri, targetUri))
    }.toSet
}
