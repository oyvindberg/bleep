package bleep.bsp

import bleep.analysis.CancellationToken

import java.nio.file.Path
import java.util.concurrent.{CompletableFuture, ConcurrentHashMap}

/** Tracks active work per workspace in server memory.
  *
  * Replaces the file-based ActiveWork mechanism. When a workspace is busy, other connections wait on the completion future.
  */
object SharedWorkspaceState {

  case class ActiveWork(
      operation: String,
      projects: Set[String],
      cancellationToken: CancellationToken,
      completion: CompletableFuture[Unit],
      startTimeMs: Long,
      forceKill: Runnable
  )

  private val activeWork = new ConcurrentHashMap[Path, ActiveWork]()

  /** Try to register active work. Returns true if successfully registered (workspace was free). */
  def trySetActive(workspace: Path, work: ActiveWork): Boolean =
    activeWork.putIfAbsent(workspace, work) == null

  /** Clear active work for a workspace and complete its future.
    *
    * Only removes the entry if it matches the expected ActiveWork instance. This prevents a disconnecting connection from clearing a DIFFERENT connection's
    * workspace registration.
    */
  def clearActive(workspace: Path, expected: ActiveWork): Unit =
    if (activeWork.remove(workspace, expected)) {
      expected.completion.complete(())
    }

  /** Clear active work unconditionally (used during connection cleanup as best-effort). */
  def clearActiveUnconditional(workspace: Path): Unit = {
    val work = activeWork.remove(workspace)
    if (work != null) work.completion.complete(())
  }

  /** Get active work for a workspace, if any. */
  def getActive(workspace: Path): Option[ActiveWork] =
    Option(activeWork.get(workspace))

  /** Cancel active work for a workspace — cancels token and force-kills child processes. */
  def cancelActive(workspace: Path): Unit =
    getActive(workspace).foreach { work =>
      work.cancellationToken.cancel()
      work.forceKill.run()
    }
}
