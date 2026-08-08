package bleep.bsp

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import java.nio.file.attribute.PosixFilePermissions
import java.nio.file.{Files, Path}

/** The socket directory is 0700 or the daemon does not start — at all, and not once.
  *
  * libdaemonjvm's `LockFiles.under` throws on any group/other permission bit, and it is the daemon's first act. The client can only see the symptom: a spawn
  * that exits instantly, a retry, another instant exit, then "BSP server failed to start within 1 minute" with `socket file exists: false`. Since the bleep
  * version is part of the JVM key, a directory that starts out wrong is never revisited by a binary that would get it right — it stays broken until someone
  * chmods it by hand. Observed in the field with a default umask of 022, on a directory the client had created a moment earlier.
  */
class SocketDirPermissionsTest extends AnyFunSuite with Matchers {

  private val OwnerOnly = PosixFilePermissions.fromString("rwx------")

  private def tempDir(): Path = {
    val d = Files.createTempDirectory("bleep-socket-perms")
    d.toFile.deleteOnExit()
    d
  }

  private def isPosix(p: Path): Boolean = p.getFileSystem.supportedFileAttributeViews().contains("posix")

  test("a directory that does not exist yet is created 0700") {
    val dir = tempDir().resolve("socket-dir")
    BspServerOperations.ensureSocketDir(dir)

    Files.isDirectory(dir) shouldBe true
    if (isPosix(dir)) Files.getPosixFilePermissions(dir) shouldBe OwnerOnly
  }

  test("a directory left group/other readable by the umask is repaired, not accepted") {
    val dir = tempDir().resolve("socket-dir")
    Files.createDirectories(dir)
    if (isPosix(dir)) {
      // What `mkdir(2)` yields under the common umask of 022 — and what the client used to leave behind for the daemon.
      Files.setPosixFilePermissions(dir, PosixFilePermissions.fromString("rwxr-xr-x")): Unit

      BspServerOperations.ensureSocketDir(dir)

      Files.getPosixFilePermissions(dir) shouldBe OwnerOnly
    }
  }

  test("repairing keeps the directory's contents — logs and locks are not collateral") {
    val dir = tempDir().resolve("socket-dir")
    Files.createDirectories(dir)
    val log = dir.resolve("output")
    Files.write(log, "the crash trace".getBytes("UTF-8")): Unit
    if (isPosix(dir)) Files.setPosixFilePermissions(dir, PosixFilePermissions.fromString("rwxrwxrwx")): Unit

    BspServerOperations.ensureSocketDir(dir)

    new String(Files.readAllBytes(log), "UTF-8") shouldBe "the crash trace"
  }

  test("the parent is left alone — only the lock directory itself is constrained") {
    val parent = tempDir()
    val dir = parent.resolve("socket-dir")
    if (isPosix(parent)) {
      val parentBefore = Files.getPosixFilePermissions(parent)

      BspServerOperations.ensureSocketDir(dir)

      Files.getPosixFilePermissions(parent) shouldBe parentBefore
    }
  }
}
