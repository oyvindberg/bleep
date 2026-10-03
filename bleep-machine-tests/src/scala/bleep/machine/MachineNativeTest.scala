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

  test("every platform maps to a library; both macOS architectures share the universal one") {
    ProbePlatform.from("Mac OS X", "aarch64") shouldBe ProbePlatform.MacOsArm64
    ProbePlatform.from("Mac OS X", "x86_64") shouldBe ProbePlatform.MacOsX64
    ProbePlatform.from("Windows 11", "amd64") shouldBe ProbePlatform.WindowsX64
    ProbePlatform.from("Windows 11", "aarch64") shouldBe ProbePlatform.WindowsArm64
    ProbePlatform.from("Linux", "aarch64") shouldBe ProbePlatform.Linux
    intercept[UnsupportedOperationException](ProbePlatform.from("FreeBSD", "amd64"))
    MachineNative.resourceFor(ProbePlatform.MacOsX64) shouldBe MachineNative.resourceFor(ProbePlatform.MacOsArm64)
    MachineNative.resourceFor(ProbePlatform.WindowsArm64) should not be MachineNative.resourceFor(ProbePlatform.WindowsX64)
  }
}
