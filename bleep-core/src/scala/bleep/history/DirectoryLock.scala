package bleep.history

import java.nio.channels.FileChannel
import java.nio.file.{Path, StandardOpenOption}
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.locks.ReentrantLock

/** Exclusive advisory lock on `<dir>/.lock`, for the writers of a store that assigns ids in that directory (transcripts, run logs). Readers never take it.
  *
  * A file lock belongs to the whole JVM, not to a thread: a second `lock()` on the same file from the same JVM does not wait, it throws
  * `OverlappingFileLockException`. The compile server writes a transcript per request, and two requests in one workspace can finish together, so writers in
  * this JVM first take a lock of their own for the directory. The file lock then only has other processes left to exclude.
  */
object DirectoryLock {
  def withLock[A](d: Path)(body: => A): A = {
    val inJvm = jvmLocks.computeIfAbsent(d.toAbsolutePath.normalize, _ => new ReentrantLock())
    inJvm.lock()
    try {
      val channel = FileChannel.open(d.resolve(".lock"), StandardOpenOption.CREATE, StandardOpenOption.WRITE)
      try {
        val lock = channel.lock()
        try body
        finally lock.release()
      } finally channel.close()
    } finally inJvm.unlock()
  }

  /** One lock per directory, for the writers in this JVM. It is JVM-wide because the file lock it complements is: it carries no data. */
  private val jvmLocks = new ConcurrentHashMap[Path, ReentrantLock]()
}
