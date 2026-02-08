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
  * Lock files are created in the project's output directory as `.bleep-lock`.
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
    * @return
    *   Resource that holds the lock while in scope
    */
  def acquire(
      project: CrossProjectName,
      outputDir: Path,
      timeout: scala.concurrent.duration.FiniteDuration
  ): Resource[IO, Unit] =
    Resource.make(acquireLock(project, outputDir, timeout))(_ => releaseLock(project, outputDir))

  /** Try to acquire lock, with retry and timeout */
  private def acquireLock(
      project: CrossProjectName,
      outputDir: Path,
      timeout: scala.concurrent.duration.FiniteDuration
  ): IO[Unit] = {
    val lockFile = outputDir.resolve(".bleep-lock")
    val retryDelay = scala.concurrent.duration.Duration(50, scala.concurrent.duration.MILLISECONDS)
    val maxAttempts = (timeout.toMillis / retryDelay.toMillis).toInt.max(1)

    def attempt(remaining: Int): IO[Unit] =
      IO.blocking {
        // First acquire JVM-level lock (fast path for same-process)
        val jvmLock = getJvmLock(project)
        jvmLock.synchronized {
          // Ensure output directory exists
          Files.createDirectories(outputDir)

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
              () // Success
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
          IO.sleep(retryDelay) >> attempt(remaining - 1)
        case _: LockNotAcquiredException =>
          IO.raiseError(new LockTimeoutException(project, timeout))
        case e =>
          IO.raiseError(e)
      }

    attempt(maxAttempts)
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
