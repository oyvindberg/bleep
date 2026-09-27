package bleep.bsp

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import java.nio.file.Path

/** A test fork's process directory must be the `user.dir` it is told: file access resolves relative paths against the former, `getAbsolutePath` the latter. */
class ForkWorkingDirectoryTest extends AnyFunSuite with Matchers {
  private val projectDir = Some(Path.of("/build/compiler"))

  test("the stated user.dir is the process directory, the last one when stated twice") {
    TestRunner.forkWorkingDirectory(List("-Xmx1g", "-Duser.dir=/build"), projectDir) shouldBe Some(Path.of("/build"))
    TestRunner.forkWorkingDirectory(List("-Duser.dir=/a", "-Duser.dir=/b"), projectDir) shouldBe Some(Path.of("/b"))
  }

  test("without one, the project folder") {
    TestRunner.forkWorkingDirectory(List("-Xmx1g"), projectDir) shouldBe projectDir
    TestRunner.forkWorkingDirectory(Nil, None) shouldBe None
  }
}
