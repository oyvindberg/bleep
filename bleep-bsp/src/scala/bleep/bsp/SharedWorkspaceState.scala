package bleep.bsp

import bleep.analysis.CancellationToken

import java.nio.file.Path
import java.util.concurrent.ConcurrentHashMap
import scala.jdk.CollectionConverters.*

/** Tracks active operations per workspace in server memory.
  *
  * Multiple operations can run concurrently on the same workspace. This registry is for visibility and debugging — it does NOT block. Fine-grained per-project
  * serialization is handled by ProjectLock.
  */
object SharedWorkspaceState {

  case class ActiveWork(
      operationId: String,
      operation: String,
      projects: Set[String],
      cancellationToken: CancellationToken,
      startTimeMs: Long,
      forceKill: Runnable
  )

  // workspace -> (operationId -> work)
  private val activeWork = new ConcurrentHashMap[Path, ConcurrentHashMap[String, ActiveWork]]()

  /** Register an operation. Always succeeds — multiple operations can be active per workspace. */
  def register(workspace: Path, work: ActiveWork): Unit = {
    val ops = activeWork.computeIfAbsent(workspace, _ => new ConcurrentHashMap[String, ActiveWork]())
    ops.put(work.operationId, work): Unit
  }

  /** Unregister a specific operation by ID.
    *
    * We intentionally do NOT remove the empty inner map: doing so races with a concurrent `register` on the same workspace. `ConcurrentHashMap.remove(K, V)`
    * compares values by `equals`, and Java's `AbstractMap.equals` is content-based — so a `register` that snuck in between our `isEmpty` check and the
    * `remove(K, V)` call would leave the inner map *appearing* identical to itself (a single new entry), and we'd drop it, losing the just-registered work. The
    * inner map is bounded by the number of workspaces this server sees (one in the common case, a handful in tests), so the bookkeeping cost is negligible.
    */
  def unregister(workspace: Path, operationId: String): Unit = {
    val ops = activeWork.get(workspace)
    if (ops != null) ops.remove(operationId): Unit
  }

  /** Unregister specific operations by ID (connection cleanup). */
  def unregisterAll(workspace: Path, operationIds: Iterable[String]): Unit = {
    val ops = activeWork.get(workspace)
    if (ops != null) operationIds.foreach(ops.remove)
  }

  /** Get all active operations for a workspace. */
  def getActiveOperations(workspace: Path): List[ActiveWork] = {
    val ops = activeWork.get(workspace)
    if (ops == null) Nil
    else ops.values().asScala.toList
  }

  /** Cancel all active operations for a workspace — cancels tokens and force-kills child processes. */
  def cancelAll(workspace: Path): Unit =
    getActiveOperations(workspace).foreach { work =>
      work.cancellationToken.cancel()
      work.forceKill.run()
    }

  /** Cancel a specific operation by ID. */
  def cancelOperation(workspace: Path, operationId: String): Unit = {
    val ops = activeWork.get(workspace)
    if (ops != null) {
      val work = ops.get(operationId)
      if (work != null) {
        work.cancellationToken.cancel()
        work.forceKill.run()
      }
    }
  }
}
