package bleep.bsp

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import java.nio.file.{Files, Path}

/** What survives a crash, and what a socket directory is allowed to cost.
  *
  * The motivating failure: a client retries a crashed server once, so a systematic crash produces `crash → restart → crash` and two rotations within seconds.
  * With one generation of history the second rotation overwrote the first crash's log — and `cleanup` deleted the live one outright before the restart, so a
  * crash that was not an OutOfMemoryError left nothing at all. Reported from the field as "crashed twice … no OutOfMemoryError recorded", log already replaced.
  */
class ServerLogRetentionTest extends AnyFunSuite with Matchers {

  private def socketDir(): Path = {
    val d = Files.createTempDirectory("bleep-socket")
    d.toFile.deleteOnExit()
    d
  }

  private def write(p: Path, content: String): Unit = Files.write(p, content.getBytes("UTF-8")): Unit
  private def read(p: Path): String = new String(Files.readAllBytes(p), "UTF-8")

  /** startServer's rotation, reached the way the server reaches it. */
  private def rotate(dir: Path): Unit = {
    val m = BspServerOperations.getClass.getDeclaredMethod("rotateOutput", classOf[Path])
    m.setAccessible(true)
    m.invoke(BspServerOperations, dir): Unit
  }

  test("cleanup keeps the log — a crash must not delete its own evidence") {
    val dir = socketDir()
    write(dir.resolve("output"), "the crash trace")
    write(dir.resolve("pid"), "12345")
    write(dir.resolve("lock"), "")

    BspServerOperations.cleanup(dir).unsafeRunSyncCompat()

    // runtime state goes
    Files.exists(dir.resolve("pid")) shouldBe false
    Files.exists(dir.resolve("lock")) shouldBe false
    // evidence stays
    read(dir.resolve("output")) shouldBe "the crash trace"
  }

  test("the original crash survives the retry that was meant to diagnose it") {
    val dir = socketDir()
    // boot 1 crashes
    write(dir.resolve("output"), "CRASH ONE — the interesting one")
    // client restarts: rotation happens at spawn
    rotate(dir)
    write(dir.resolve("output"), "boot 2, short and uninformative")
    // boot 2 crashes too; client restarts again before giving up
    rotate(dir)

    val kept = (1 to BspServerOperations.OutputGenerationsKept).map(n => dir.resolve(s"output.$n")).filter(Files.exists(_)).map(read)
    kept should contain("CRASH ONE — the interesting one")
  }

  test("history is bounded: generations beyond the kept count are dropped") {
    val dir = socketDir()
    (1 to BspServerOperations.OutputGenerationsKept + 3).foreach { i =>
      write(dir.resolve("output"), s"boot $i")
      rotate(dir)
    }
    val present = (1 to 20).map(n => dir.resolve(s"output.$n")).count(Files.exists(_))
    present should be <= BspServerOperations.OutputGenerationsKept
  }

  test("the legacy single-generation name is cleaned up rather than left to rot") {
    val dir = socketDir()
    write(dir.resolve("output.prev"), "written by an older bleep")
    write(dir.resolve("output"), "current")
    rotate(dir)
    Files.exists(dir.resolve("output.prev")) shouldBe false
  }

  test("retained logs stay within the byte budget") {
    val dir = socketDir()
    val big = "x" * (BspServerOperations.MaxRetainedLogBytes.toInt / 2 + 1024)
    (1 to BspServerOperations.OutputGenerationsKept).foreach { _ =>
      write(dir.resolve("output"), big)
      rotate(dir)
    }
    val retained = (1 to BspServerOperations.OutputGenerationsKept).map(n => dir.resolve(s"output.$n")).filter(Files.exists(_)).map(Files.size).sum
    retained should be <= BspServerOperations.MaxRetainedLogBytes
  }

  extension (io: cats.effect.IO[Unit]) {
    def unsafeRunSyncCompat(): Unit = {
      import cats.effect.unsafe.implicits.global
      io.unsafeRunSync()
    }
  }
}
