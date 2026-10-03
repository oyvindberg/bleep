package bleep.machine

import ryddig.Logger

import java.nio.ByteBuffer
import java.nio.channels.{FileChannel, FileLock}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, StandardOpenOption}
import java.util.concurrent.locks.LockSupport

/** The machine-wide lock that serialises *claiming* memory across servers (design §6.3, §8). It protects the step from "observe the machine and everyone's
  * claims" to "publish my new claims" — not the files, which have one writer each, and not ticks in general.
  *
  * A trait so the tick runtime can be driven against a fake that records whether a tick asked for the lock at all.
  */
trait MachineLock {

  /** Runs `body` holding the lock if it can be had within `waitMs`, else with [[LockState.Unavailable]] naming who holds it and for how long. `body` runs
    * either way: an unavailable lock is a modelled state, not an error. The lock is released in a `finally`; `timer` measures the steps of the critical section
    * and the lock reports a hold over its threshold after release.
    */
  def locked[A](waitMs: Long)(body: (LockState, HoldTimer) => A): A
}

/** Per-step timing of a critical section, so a long hold says where the time went. Steps are named by the caller in the order they run. */
final class HoldTimer {
  private var steps: List[(String, Long)] = Nil

  def step[A](name: String)(f: => A): A = {
    val start = System.nanoTime()
    try f
    finally steps = steps :+ (name -> (System.nanoTime() - start))
  }

  def breakdown: List[(String, Long)] = steps

  def describe: String = steps.map { case (name, nanos) => s"$name ${nanos / 1_000_000}ms" }.mkString(", ")
}

object MachineLock {
  val FileName = "machine.lock"

  /** Hold time past which the holder reports itself (design §8 point 4). */
  val LongHoldMs: Long = 50L

  /** The announcement lives in the first [[ContentBytes]]; the lock is taken on one byte at [[LockOffset]], beyond it, so that Windows' mandatory locking of
    * the locked range leaves the announcement readable to everyone (design §8 point 3).
    */
  val ContentBytes: Int = 256
  val LockOffset: Long = 4096L

  def path(userPaths: bleep.UserPaths): Path = userPaths.cacheDir.resolve(FileName)

  /** What a holder writes into the file: `pid:startedAt acquiredAt=T`. */
  case class Announcement(pid: Long, startedAtEpochMs: Long, acquiredAtEpochMs: Long) {
    def render: String = s"$pid:$startedAtEpochMs acquiredAt=$acquiredAtEpochMs"
    def holder: String = s"pid $pid"
  }

  object Announcement {
    private val Pattern = """(\d+):(\d+) acquiredAt=(\d+)""".r

    /** `None` for a blank file: the holder has the lock but has not written yet, or nobody has ever held it. Anything else that does not parse throws. */
    def parse(content: String): Option[Announcement] =
      content.trim match {
        case ""                                  => None
        case Pattern(pid, startedAt, acquiredAt) => Some(Announcement(pid.toLong, startedAt.toLong, acquiredAt.toLong))
        case other                               => throw new IllegalStateException(s"machine.lock holds '$other', not an announcement")
      }
  }
}

/** The one opener of `machine.lock` in this process (design §8 point 2): `fcntl` locks are per process and closing *any* descriptor of the file releases them,
  * so the channel is opened once here and closed only with the instance. Construct exactly one per server.
  *
  * @param path
  *   normally `UserPaths.cacheDir / machine.lock`; a parameter so tests lock in a temp dir
  */
final class FileMachineLock(path: Path, self: ServerIdentity, logger: Logger) extends MachineLock with AutoCloseable {
  import MachineLock._

  Files.createDirectories(path.getParent)
  private val channel: FileChannel = FileChannel.open(path, StandardOpenOption.CREATE, StandardOpenOption.READ, StandardOpenOption.WRITE)

  // The tick thread is the only caller, so a second overlapping call from this process is a bug: let OverlappingFileLockException surface.
  private def tryLock(): Option[FileLock] = Option(channel.tryLock(LockOffset, 1L, false))

  override def locked[A](waitMs: Long)(body: (LockState, HoldTimer) => A): A = {
    val deadline = System.nanoTime() + waitMs * 1_000_000L
    var lock: Option[FileLock] = tryLock()
    while (lock.isEmpty && System.nanoTime() < deadline) {
      LockSupport.parkNanos(500_000L)
      lock = tryLock()
    }
    val timer = new HoldTimer
    lock match {
      case None =>
        body(unavailable(), timer)
      case Some(held) =>
        val acquiredAtNanos = System.nanoTime()
        try {
          announce(Announcement(self.pid, self.startedAtEpochMs, System.currentTimeMillis()))
          body(LockState.Held, timer)
        } finally {
          held.release()
          val heldMs = (System.nanoTime() - acquiredAtNanos) / 1_000_000L
          if (heldMs > LongHoldMs) logger.warn(s"machine.lock held for ${heldMs}ms (threshold ${LongHoldMs}ms): ${timer.describe}")
        }
    }
  }

  /** Who holds it, as read from the announcement; read after the deadline, never under the lock. */
  private def unavailable(): LockState.Unavailable =
    readAnnouncement() match {
      case Some(a) => LockState.Unavailable(a.holder, heldForMs = math.max(0L, System.currentTimeMillis() - a.acquiredAtEpochMs))
      case None    => LockState.Unavailable("unannounced holder", heldForMs = 0L)
    }

  private def announce(a: Announcement): Unit = {
    val bytes = a.render.getBytes(StandardCharsets.UTF_8)
    require(bytes.length <= ContentBytes, s"announcement '${a.render}' exceeds $ContentBytes bytes")
    val buffer = ByteBuffer.allocate(ContentBytes)
    buffer.put(bytes): Unit
    while (buffer.hasRemaining) buffer.put(' '.toByte): Unit
    buffer.flip(): Unit
    var position = 0L
    // No `force`: visibility to other processes on this machine comes from the shared page cache; durability is irrelevant for a lock file.
    while (buffer.hasRemaining) position += channel.write(buffer, position).toLong
  }

  def readAnnouncement(): Option[Announcement] = {
    val buffer = ByteBuffer.allocate(ContentBytes)
    var position = 0L
    var read = 0
    while (read >= 0 && buffer.hasRemaining) {
      read = channel.read(buffer, position)
      if (read > 0) position += read.toLong
    }
    buffer.flip(): Unit
    Announcement.parse(StandardCharsets.UTF_8.decode(buffer).toString)
  }

  override def close(): Unit = channel.close()
}
