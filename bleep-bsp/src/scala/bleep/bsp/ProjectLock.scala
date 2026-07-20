package bleep.bsp

import bleep.model.CrossProjectName
import cats.effect.{IO, Resource}

import java.io.RandomAccessFile
import java.nio.channels.{FileChannel, FileLock, OverlappingFileLockException}
import java.nio.file.{Files, Path}
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.locks.ReentrantLock

/** Per-project read/write lock for compile and link operations.
  *
  * Cross-process: backed by Java NIO FileLock with shared/exclusive modes.
  *   - Exclusive (writer): one holder at a time, no concurrent readers anywhere.
  *   - Shared (reader): multiple holders allowed concurrently across processes; no concurrent writer.
  *
  * Within-JVM: NIO file locks throw OverlappingFileLockException if the same JVM tries to lock the same file twice through different channels, even when both
  * want shared. We work around this by holding ONE shared file lock per project per JVM, refcounting within-JVM holders. The first shared acquirer opens the
  * channel and acquires the file lock; subsequent shared acquirers in this JVM bump a refcount; the last shared releaser closes the channel.
  *
  * Lock files live in the parent of the output directory (e.g. `target/.bleep-lock`, not `target/classes/.bleep-lock`). On Windows, mandatory file locks
  * prevent ALL access to the locked file — placing the lock inside the classes directory would break Zinc's class file scanning.
  */
object ProjectLock {

  sealed trait LockMode
  object LockMode {
    case object Exclusive extends LockMode
    case object Shared extends LockMode
  }

  /** Per-project state. Mutated only under `getMonitor(project).synchronized`. */
  private final class ProjectState {
    // Within-JVM count of shared holders. When > 0, `sharedFileLock` must be defined.
    var sharedHolderCount: Int = 0
    var sharedFileLock: Option[LockInfo] = None
    var exclusiveLock: Option[LockInfo] = None
  }

  /** Keyed by lock file path, NOT by project name.
    *
    * The cross-process lock lives at `<targetDir>/.bleep-lock`, which encodes workspace, build variant and project. Keying the within-JVM bookkeeping by
    * `CrossProjectName` alone would make a `normal`-variant and a `bsp`-variant compile of the same project contend on one `ProjectState` even though they
    * write disjoint directories and take disjoint file locks — the loser then retried until the 5 minute timeout. Since each `CrossProjectName` has exactly one
    * `targetDir` (`ProjectPaths.classes` picks `classes` or `test-classes` from the project's own `isTestProject`), the lock file path is a faithful 1:1 key
    * for (workspace, variant, project).
    */
  private val states = new ConcurrentHashMap[Path, ProjectState]()

  /** Per-lock-file fair lock. Fairness (FIFO) prevents an exclusive waiter from being starved by a continuous stream of shared acquirers — under sustained IDE
    * compile bursts mixed with `bleep test` runs, the previous JVM-monitor model gave no ordering guarantees and could leave an exclusive wait queued for the
    * full 5-minute timeout.
    */
  private val locks = new ConcurrentHashMap[Path, ReentrantLock]()

  private def lockFileFor(outputDir: Path): Path =
    outputDir.getParent.resolve(".bleep-lock")

  private def getLock(lockFile: Path): ReentrantLock =
    locks.computeIfAbsent(lockFile, _ => new ReentrantLock( /* fair = */ true))

  private def getState(lockFile: Path): ProjectState =
    states.computeIfAbsent(lockFile, _ => new ProjectState)

  /** Acquire a lock for a project's read or write operations.
    *
    * @param project
    *   The project to lock
    * @param outputDir
    *   The project's output directory (lock file lives in its parent)
    * @param mode
    *   Exclusive for writes (compile/link), Shared for reads (compile of dependents that read this project's classes)
    * @param timeout
    *   Maximum time to wait for lock acquisition
    * @param onContention
    *   Called once on the first failed lock attempt (before retrying)
    * @return
    *   Resource that holds the lock while in scope. The boolean indicates whether there was contention (true = had to wait).
    */
  def acquire(
      project: CrossProjectName,
      outputDir: Path,
      mode: LockMode,
      timeout: scala.concurrent.duration.FiniteDuration,
      onContention: () => Unit
  ): Resource[IO, Boolean] = {
    val lockFile = lockFileFor(outputDir)
    Resource.make(acquireLock(project, outputDir, lockFile, mode, timeout, onContention))(_ => releaseLock(lockFile, mode))
  }

  private def acquireLock(
      project: CrossProjectName,
      outputDir: Path,
      lockFile: Path,
      mode: LockMode,
      timeout: scala.concurrent.duration.FiniteDuration,
      onContention: () => Unit
  ): IO[Boolean] = {
    val retryDelay = scala.concurrent.duration.Duration(50, scala.concurrent.duration.MILLISECONDS)
    val maxAttempts = (timeout.toMillis / retryDelay.toMillis).toInt.max(1)

    def attempt(remaining: Int, notified: Boolean): IO[Boolean] =
      IO.blocking {
        // Create the output dir OUTSIDE the per-project lock. mkdir is idempotent + safe to call
        // from many threads concurrently; doing it under the lock just serializes a stat syscall
        // that has no contention by itself.
        //
        // JDK's createDirectories has a known TOCTOU race when parallel threads create overlapping
        // trees — the internal symlink check uses NOFOLLOW_LINKS and can fail when the final
        // component is a symlink-to-directory created by another thread.
        try Files.createDirectories(outputDir)
        catch {
          case _: java.nio.file.FileAlreadyExistsException if Files.isDirectory(outputDir) => ()
        }

        val lock = getLock(lockFile)
        val state = getState(lockFile)
        lock.lock()
        try {
          mode match {
            case LockMode.Shared =>
              if (state.exclusiveLock.isDefined) throw new LockNotAcquiredException(project)
              state.sharedFileLock match {
                case Some(_) =>
                  // Already holding the within-JVM shared file lock. Bump refcount.
                  state.sharedHolderCount += 1
                case None =>
                  // First shared holder in this JVM. Open channel + acquire shared file lock.
                  val raf = new RandomAccessFile(lockFile.toFile, "rw")
                  var owned = false
                  try {
                    val channel = raf.getChannel
                    val fileLock = channel.tryLock(0L, java.lang.Long.MAX_VALUE, /* shared = */ true)
                    if (fileLock != null) {
                      state.sharedFileLock = Some(LockInfo(raf, channel, fileLock))
                      state.sharedHolderCount = 1
                      owned = true
                    } else {
                      throw new LockNotAcquiredException(project)
                    }
                  } catch {
                    case _: OverlappingFileLockException =>
                      throw new LockNotAcquiredException(project)
                  } finally
                    if (!owned) {
                      try raf.close()
                      catch { case _: Exception => () }
                    }
              }

            case LockMode.Exclusive =>
              if (state.exclusiveLock.isDefined || state.sharedFileLock.isDefined) {
                throw new LockNotAcquiredException(project)
              }
              val raf = new RandomAccessFile(lockFile.toFile, "rw")
              var owned = false
              try {
                val channel = raf.getChannel
                val fileLock = channel.tryLock()
                if (fileLock != null) {
                  state.exclusiveLock = Some(LockInfo(raf, channel, fileLock))
                  owned = true
                } else {
                  throw new LockNotAcquiredException(project)
                }
              } catch {
                case _: OverlappingFileLockException =>
                  throw new LockNotAcquiredException(project)
              } finally
                if (!owned) {
                  try raf.close()
                  catch { case _: Exception => () }
                }
          }
          notified
        } finally lock.unlock()
      }.handleErrorWith {
        case _: LockNotAcquiredException if remaining > 1 =>
          IO(if (!notified) onContention()) >>
            IO.sleep(retryDelay) >> attempt(remaining - 1, true)
        case _: LockNotAcquiredException =>
          IO.raiseError(new LockTimeoutException(project, timeout))
        case e: java.io.IOException if remaining > 1 && isWindowsSharingViolation(e) =>
          // On Windows, antivirus scanners and indexing services can briefly hold exclusive handles
          // on newly-created files. Retry the same way as for lock contention.
          IO.sleep(retryDelay) >> attempt(remaining - 1, notified)
        case e: java.io.IOException if isWindowsSharingViolation(e) =>
          IO.raiseError(new LockTimeoutException(project, timeout))
        case e =>
          IO.raiseError(e)
      }

    attempt(maxAttempts, false)
  }

  private def releaseLock(lockFile: Path, mode: LockMode): IO[Unit] =
    IO.blocking {
      val state = states.get(lockFile)
      if (state != null) {
        val lock = getLock(lockFile)
        // Snapshot the LockInfo that needs closing under the lock; do the actual file-handle
        // closure outside the lock. Closing a FileChannel under load can trip slow paths
        // (Windows file system filters, NFS) we don't want to hold the per-project lock for.
        var toClose: Option[LockInfo] = None
        lock.lock()
        try
          mode match {
            case LockMode.Shared =>
              state.sharedHolderCount -= 1
              if (state.sharedHolderCount <= 0) {
                toClose = state.sharedFileLock
                state.sharedFileLock = None
                state.sharedHolderCount = 0
              }
            case LockMode.Exclusive =>
              toClose = state.exclusiveLock
              state.exclusiveLock = None
          }
        finally lock.unlock()
        toClose.foreach(closeQuietly)
      }
    }

  private def closeQuietly(info: LockInfo): Unit = {
    try info.fileLock.release()
    catch { case _: Exception => () }
    try info.channel.close()
    catch { case _: Exception => () }
    try info.raf.close()
    catch { case _: Exception => () }
  }

  /** Tracks an open file lock plus the resources backing it. */
  private case class LockInfo(raf: RandomAccessFile, channel: FileChannel, fileLock: FileLock)

  /** Check if an IOException is a Windows sharing violation (ERROR_SHARING_VIOLATION).
    *
    * This occurs when antivirus scanners, Windows Search, or backup tools briefly hold exclusive handles on files. The error message is "The process cannot
    * access the file because it is being used by another process".
    */
  private def isWindowsSharingViolation(e: java.io.IOException): Boolean =
    e.getMessage != null && e.getMessage.contains("being used by another process")

  /** Exception when lock couldn't be acquired (internal, triggers retry) */
  private class LockNotAcquiredException(project: CrossProjectName) extends Exception(s"Could not acquire lock for ${project.value}")

  /** Exception when lock acquisition times out */
  class LockTimeoutException(project: CrossProjectName, timeout: scala.concurrent.duration.FiniteDuration)
      extends Exception(s"Timeout acquiring lock for ${project.value} after $timeout")

  /** Release every lock held by this JVM.
    *
    * DAEMON SHUTDOWN ONLY. This state is global to the process, shared by every BSP connection, so calling it from a per-connection finalizer would rip
    * exclusive locks out from under compiles still running on other connections — they would carry on writing to a directory they no longer own. Normal release
    * happens through the `Resource` returned by `acquire`, which runs on fiber cancellation too.
    */
  def releaseAllOnDaemonShutdown(): IO[Unit] =
    IO.blocking {
      import scala.jdk.CollectionConverters.*
      states.keys().asScala.toList.foreach { lockFile =>
        Option(states.remove(lockFile)).foreach { state =>
          state.sharedFileLock.foreach(closeQuietly)
          state.exclusiveLock.foreach(closeQuietly)
        }
      }
    }
}
