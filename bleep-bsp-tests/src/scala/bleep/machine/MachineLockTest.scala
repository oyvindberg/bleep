package bleep.machine

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import ryddig.TypedLogger

import java.io.{BufferedReader, InputStreamReader}
import java.nio.channels.OverlappingFileLockException
import java.nio.file.{Files, Path}
import scala.jdk.StreamConverters.StreamHasToScala

/** `machine.lock` across processes (design §8): the kernel releases it when the holder dies, a stopped holder is reported by identity within the deadline, and
  * the holder's announcement is readable while the lock is held.
  */
class MachineLockTest extends AnyFunSuite with Matchers {
  private val isWindows = System.getProperty("os.name").toLowerCase.contains("win")
  private val self = StateFile.selfIdentity("test")

  private def withTempDir[A](f: Path => A): A = {
    val dir = Files.createTempDirectory("bleep-machine-lock-test")
    try f(dir)
    finally {
      val walk = Files.walk(dir)
      try walk.toScala(List).sortBy(p => -p.getNameCount).foreach(p => Files.delete(p))
      finally walk.close()
    }
  }

  /** A [[LockHolderMain]] process that has printed `LOCKED`. */
  private case class Holder(process: Process, stdout: BufferedReader) {
    def pid: Long = process.pid()
    def kill(): Unit = {
      process.destroyForcibly()
      process.waitFor(): Unit
    }
  }

  private def startHolder(lockPath: Path, holdMs: Long): Holder = {
    val javaBin = Path.of(System.getProperty("java.home"), "bin", "java").toString
    val process = new ProcessBuilder(javaBin, "-cp", System.getProperty("java.class.path"), "bleep.machine.LockHolderMain", lockPath.toString, holdMs.toString)
      .redirectErrorStream(true)
      .start()
    val stdout = new BufferedReader(new InputStreamReader(process.getInputStream))
    val deadline = System.nanoTime() + 60_000_000_000L
    var line = stdout.readLine()
    while (line != null && line != "LOCKED" && System.nanoTime() < deadline) line = stdout.readLine()
    if (line != "LOCKED") {
      process.destroyForcibly()
      fail(s"lock holder never reported LOCKED; last line: $line")
    }
    Holder(process, stdout)
  }

  private def lock(path: Path): FileMachineLock = new FileMachineLock(path, self, TypedLogger.DevNull)

  test("a lock nobody holds is taken at once, and the holder is announced in the file") {
    withTempDir { dir =>
      val l = lock(dir.resolve("machine.lock"))
      try {
        val seen = l.locked(waitMs = 1000L) { (state, timer) =>
          timer.step("probe")(())
          (state, l.readAnnouncement())
        }
        seen._1 shouldBe LockState.Held
        seen._2.map(a => (a.pid, a.startedAtEpochMs)) shouldBe Some((self.pid, self.startedAtEpochMs))
      } finally l.close()
    }
  }

  test("a second overlapping attempt from the same process is a bug and throws, not a wait") {
    withTempDir { dir =>
      val l = lock(dir.resolve("machine.lock"))
      try
        an[OverlappingFileLockException] should be thrownBy l.locked(waitMs = 10L)((_, _) => l.locked(waitMs = 10L)((_, _) => ()))
      finally l.close()
    }
  }

  test("the announcement is readable while another process holds the lock, and the wait is bounded with the holder named") {
    withTempDir { dir =>
      val path = dir.resolve("machine.lock")
      val holder = startHolder(path, holdMs = 30_000L)
      val l = lock(path)
      try {
        l.readAnnouncement().map(_.pid) shouldBe Some(holder.pid)
        val started = System.nanoTime()
        val state = l.locked(waitMs = 300L)((state, _) => state)
        val waitedMs = (System.nanoTime() - started) / 1_000_000L
        state match {
          case LockState.Unavailable(who, heldForMs) =>
            who shouldBe s"pid ${holder.pid}"
            heldForMs should be >= 0L
          case other => fail(s"expected Unavailable, got $other")
        }
        waitedMs should be >= 300L
        waitedMs should be < 5000L
      } finally {
        holder.kill()
        l.close()
      }
    }
  }

  test("SIGKILL of the holder releases the lock") {
    withTempDir { dir =>
      val path = dir.resolve("machine.lock")
      val holder = startHolder(path, holdMs = 30_000L)
      val l = lock(path)
      try {
        l.locked(waitMs = 100L)((state, _) => state) shouldBe a[LockState.Unavailable]
        holder.kill()
        l.locked(waitMs = 2000L)((state, _) => state) shouldBe LockState.Held
      } finally {
        holder.kill()
        l.close()
      }
    }
  }

  test("a stopped holder (SIGSTOP) is reported Unavailable with its identity within the deadline") {
    if (isWindows) cancel("SIGSTOP does not exist on Windows; a suspended process cannot be produced from here")
    withTempDir { dir =>
      val path = dir.resolve("machine.lock")
      val holder = startHolder(path, holdMs = 30_000L)
      val l = lock(path)
      try {
        new ProcessBuilder("kill", "-STOP", holder.pid.toString).inheritIO().start().waitFor() shouldBe 0
        val state = l.locked(waitMs = 300L)((state, _) => state)
        state match {
          case LockState.Unavailable(who, _) => who shouldBe s"pid ${holder.pid}"
          case other                         => fail(s"expected Unavailable, got $other")
        }
      } finally {
        new ProcessBuilder("kill", "-CONT", holder.pid.toString).inheritIO().start().waitFor(): Unit
        holder.kill()
        l.close()
      }
    }
  }

  test("the lock is released even when the critical section throws, and a long hold is reported with its breakdown") {
    withTempDir { dir =>
      val storing = ryddig.Loggers.storing()
      val l = new FileMachineLock(dir.resolve("machine.lock"), self, storing.map(_ => ()))
      try {
        an[IllegalStateException] should be thrownBy l.locked(waitMs = 100L) { (_, timer) =>
          timer.step("read")(Thread.sleep(MachineLock.LongHoldMs + 20L))
          throw new IllegalStateException("boom")
        }
        val warning = storing.underlying.map(_.message.plainText).find(_.startsWith("machine.lock held for"))
        warning.isDefined shouldBe true
        warning.get should include("read ")
        // Released: a second process gets it at once.
        val holder = startHolder(dir.resolve("machine.lock"), holdMs = 0L)
        holder.process.waitFor() shouldBe 0
      } finally l.close()
    }
  }

  test("an announcement that is not one is rejected") {
    MachineLock.Announcement.parse("") shouldBe None
    MachineLock.Announcement.parse("   ") shouldBe None
    MachineLock.Announcement.parse("12:34 acquiredAt=56").map(_.render) shouldBe Some("12:34 acquiredAt=56")
    an[IllegalStateException] should be thrownBy MachineLock.Announcement.parse("garbage")
  }
}
