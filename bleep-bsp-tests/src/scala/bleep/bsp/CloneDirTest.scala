package bleep.bsp

import bleep.CloneDir
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import java.nio.charset.StandardCharsets
import java.nio.file.Files

/** The clone transport differs per OS — clonefile via `cp -Rc` on macOS, `cp -a --reflink=auto` on Linux, a JVM recursive copy elsewhere — so this suite
  * running in CI on every platform is what proves each lane produces a faithful copy.
  */
class CloneDirTest extends AnyFunSuite with Matchers {

  test(s"clones a tree byte-for-byte (strategy: ${CloneDir.Strategy.current})") {
    val from = Files.createTempDirectory("clone-src")
    Files.createDirectories(from.resolve("a/b/c"))
    Files.write(from.resolve("a/b/c/deep.txt"), "deep".getBytes(StandardCharsets.UTF_8))
    Files.write(from.resolve("a/top.bin"), Array[Byte](0, 1, 2, 127, -128))
    Files.write(from.resolve("root.txt"), "root".getBytes(StandardCharsets.UTF_8))
    Files.createDirectories(from.resolve("empty-dir"))

    val to = Files.createTempDirectory("clone-dst").resolve("target")
    CloneDir.clone(from, to)

    Files.readAllBytes(to.resolve("a/b/c/deep.txt")) shouldBe "deep".getBytes(StandardCharsets.UTF_8)
    Files.readAllBytes(to.resolve("a/top.bin")) shouldBe Array[Byte](0, 1, 2, 127, -128)
    Files.readAllBytes(to.resolve("root.txt")) shouldBe "root".getBytes(StandardCharsets.UTF_8)
    Files.isDirectory(to.resolve("empty-dir")) shouldBe true

    withClue("clones must be independent copies — writing the clone must not touch the source: ") {
      Files.write(to.resolve("root.txt"), "changed".getBytes(StandardCharsets.UTF_8))
      new String(Files.readAllBytes(from.resolve("root.txt")), StandardCharsets.UTF_8) shouldBe "root"
    }
  }

  test("refuses an existing target") {
    val from = Files.createTempDirectory("clone-src2")
    val to = Files.createTempDirectory("clone-dst2")
    the[Exception] thrownBy CloneDir.clone(from, to) should have message s"cannot clone to $to: already exists"
  }

  test("refuses a source that is not a directory") {
    val from = Files.createTempDirectory("clone-src3").resolve("nope")
    val to = Files.createTempDirectory("clone-dst3").resolve("target")
    the[Exception] thrownBy CloneDir.clone(from, to) should have message s"cannot clone $from: not a directory"
  }
}
