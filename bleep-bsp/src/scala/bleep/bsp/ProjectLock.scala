package bleep.bsp

import bleep.model.CrossProjectName
import cats.effect.{IO, Resource}

import java.io.RandomAccessFile
import java.nio.channels.{FileChannel, FileLock, OverlappingFileLockException}
import java.nio.file.{Files, Path}
import java.util.concurrent.ConcurrentHashMap

/** File-based locks for project write operations (compile, link).
  *
  * Prevents concurrent compilation/linking of the same project, both within this JVM and across processes. Uses Java NIO FileLock which provides:
  *   - Cross-process locking (multiple bleep instances)
  *   - Automatic release on JVM crash
  *   - Non-blocking tryLock with timeout support
  *
  * Lock files are created in the PARENT of the output directory (e.g. `target/.bleep-lock`, not `target/classes/.bleep-lock`). This is critical on Windows
  * where file locks are mandatory — placing the lock inside the classes directory causes Zinc to fail when scanning class files, since the locked file cannot
  * be accessed by other code in the same process.
  */
object ProjectLock {

  /** In-memory locks for efficiency within same JVM */
  private val jvmLocks = ConcurrentHashMap[CrossProjectName, AnyRef]()

  /** Get or create a JVM-level lock object for a project */
  private def getJvmLock(project: CrossProjectName): AnyRef =
    jvmLocks.computeIfAbsent(project, _ => new AnyRef)

  /** Acquire an exclusive lock for a project's write operations.
    *
    * @param project
    *   The project to lock
    * @param outputDir
    *   The project's output directory (lock file will be created here)
    * @param timeout
    *   Maximum time to wait for lock acquisition
    * @param onContention
    *   Called once on the first failed lock attempt (before retrying). Use to notify callers that a lock is contended.
    * @return
    *   Resource that holds the lock while in scope. The boolean indicates whether there was contention (true = had to wait).
    */
  def acquire(
      project: CrossProjectName,
      outputDir: Path,
      timeout: scala.concurrent.duration.FiniteDuration,
      onContention: () => Unit
  ): Resource[IO, Boolean] =
    Resource.make(acquireLock(project, outputDir, timeout, onContention))(_ => releaseLock(project, outputDir))

  /** Try to acquire lock, with retry and timeout.
    *
    * @return
    *   true if there was contention (had to wait), false if acquired immediately
    */
  private def acquireLock(
      project: CrossProjectName,
      outputDir: Path,
      timeout: scala.concurrent.duration.FiniteDuration,
      onContention: () => Unit
  ): IO[Boolean] = {
    // Place lock file in parent directory (e.g. target/.bleep-lock, not target/classes/.bleep-lock).
    // On Windows, mandatory file locks prevent ALL access to the locked file — including Zinc scanning
    // the classes directory for incremental compilation.
    val lockFile = outputDir.getParent.resolve(".bleep-lock")
    val retryDelay = scala.concurrent.duration.Duration(50, scala.concurrent.duration.MILLISECONDS)
    val maxAttempts = (timeout.toMillis / retryDelay.toMillis).toInt.max(1)

    def attempt(remaining: Int, notified: Boolean): IO[Boolean] =
      IO.blocking {
        // First acquire JVM-level lock (fast path for same-process)
        val jvmLock = getJvmLock(project)
        jvmLock.synchronized {
          // Ensure output directory exists.
          // Catch FileAlreadyExistsException: JDK's createDirectories has a known
          // TOCTOU race when parallel threads create overlapping directory trees.
          // The JDK's internal symlink check uses NOFOLLOW_LINKS, which can fail
          // when the final component is a symlink-to-directory created by another thread.
          // Our isDirectory check omits NOFOLLOW_LINKS so symlinks are fine.
          try Files.createDirectories(outputDir)
          catch {
            case _: java.nio.file.FileAlreadyExistsException if Files.isDirectory(outputDir) =>
              () // Race: another thread created it. It's a directory, which is what we wanted.
          }

          // Try to acquire file lock with proper resource management
          val raf = new RandomAccessFile(lockFile.toFile, "rw")
          var success = false
          try {
            val channel = raf.getChannel
            val fileLock = channel.tryLock()
            if (fileLock != null) {
              // Store lock info for later release
              activeLocks.put(project, LockInfo(raf, channel, fileLock))
              success = true
              notified // Return whether we had to wait
            } else {
              throw new LockNotAcquiredException(project)
            }
          } catch {
            case _: OverlappingFileLockException =>
              throw new LockNotAcquiredException(project)
          } finally
            // Only close if we didn't successfully acquire the lock
            // (on success, RAF ownership transfers to activeLocks)
            if (!success) {
              try raf.close()
              catch { case _: Exception => () }
            }
        }
      }.handleErrorWith {
        case _: LockNotAcquiredException if remaining > 1 =>
          IO(if (!notified) onContention()) >>
            IO.sleep(retryDelay) >> attempt(remaining - 1, true)
        case _: LockNotAcquiredException =>
          IO.raiseError(new LockTimeoutException(project, timeout))
        case e: java.io.IOException if remaining > 1 && isWindowsSharingViolation(e) =>
          // On Windows, antivirus scanners and indexing services can briefly hold
          // exclusive handles on newly-created files. Retry in the same way as for
          // lock contention.
          IO.sleep(retryDelay) >> attempt(remaining - 1, notified)
        case e: java.io.IOException if isWindowsSharingViolation(e) =>
          IO.raiseError(new LockTimeoutException(project, timeout))
        case e =>
          IO.raiseError(e)
      }

    attempt(maxAttempts, false)
  }

  /** Release a project lock */
  private def releaseLock(project: CrossProjectName, outputDir: Path): IO[Unit] =
    IO.blocking {
      Option(activeLocks.remove(project)).foreach { info =>
        try {
          info.fileLock.release()
          info.channel.close()
          info.raf.close()
        } catch {
          case _: Exception => () // Best effort cleanup
        }
      }
    }

  /** Track active locks for cleanup */
  private case class LockInfo(raf: RandomAccessFile, channel: FileChannel, fileLock: FileLock)
  private val activeLocks = ConcurrentHashMap[CrossProjectName, LockInfo]()

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

  /** Cleanup all locks (called on shutdown) */
  def releaseAll(): IO[Unit] =
    IO.blocking {
      import scala.jdk.CollectionConverters.*
      activeLocks.keys().asScala.toList.foreach { project =>
        Option(activeLocks.remove(project)).foreach { info =>
          try {
            info.fileLock.release()
            info.channel.close()
            info.raf.close()
          } catch {
            case _: Exception => ()
          }
        }
      }
    }
}
