package bleep.internal

import org.scalatest.funsuite.AnyFunSuite

import java.nio.file.{Files, Path}

/** `deleteDirectory` has to cope with read-only entries.
  *
  * Windows will not unlink a read-only file at all; POSIX only consults the containing directory, so the same tree deletes cleanly there. git marks every
  * object under `.git/objects` read-only, which is how this surfaced — `PublishVersionTest` builds a real repository in a temp directory, and its cleanup
  * failed on the Windows CI runner and nowhere else.
  *
  * This test asserts the outcome rather than the mechanism: the tree is gone. On Windows that exercises the clear-and-retry, on POSIX the plain path. Either
  * way a regression that reintroduces the refusal fails here, on the platform that has it.
  */
class FileUtilsDeleteTest extends AnyFunSuite {

  test("a directory containing read-only files is deleted") {
    val root = Files.createTempDirectory("bleep-delete-readonly-")
    val nested = Files.createDirectories(root.resolve("objects").resolve("07"))
    val file: Path = nested.resolve("abcdef")
    Files.writeString(file, "an object, the way git leaves it")
    assert(file.toFile.setReadOnly(), s"could not make $file read-only, so this test would prove nothing")

    FileUtils.deleteDirectory(root)

    assert(!FileUtils.exists(root), s"$root survived deletion")
  }

  test("deleting a directory that is not there is not an error") {
    val root = Files.createTempDirectory("bleep-delete-absent-")
    FileUtils.deleteDirectory(root)
    FileUtils.deleteDirectory(root)
    assert(!FileUtils.exists(root))
  }
}
