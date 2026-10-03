package bleep.machine

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import java.nio.file.Files

/** The native library is on the classpath for this platform, unpacks, loads and is the ABI version this code was written against. */
class MachineNativeTest extends AnyFunSuite with Matchers {

  test("the library for this platform loads, and loading it again is harmless") {
    ProbePlatform.current() match {
      case ProbePlatform.Linux =>
        intercept[IllegalArgumentException](MachineNative.resourceFor(ProbePlatform.Linux))
      case platform =>
        val dir = Files.createTempDirectory("bleep-machine-native")
        MachineNative.load(dir, platform).abiVersion() shouldBe MachineNative.AbiVersion
        // A second server, or a second probe in the same JVM, finds the file already unpacked.
        MachineNative.load(dir, platform).abiVersion() shouldBe MachineNative.AbiVersion
        Files.list(dir).count() shouldBe 1L
    }
  }
}
