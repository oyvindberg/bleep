package bleep.machine

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import java.nio.file.Files

/** The Windows probe against the JDK's own reading of the same API: on Windows, OpenJDK's `OperatingSystemMXBean` answers total/free physical memory and
  * total/free "swap" from `GlobalMemoryStatusEx` (`ullTotalPhys`/`ullAvailPhys`, `ullTotalPageFile`/`ullAvailPageFile`). Agreement pins the field mapping.
  *
  * The live part is Windows only by nature. [[ProbesLiveTest]] covers every OS.
  */
class WindowsProbesTest extends AnyFunSuite with Matchers {
  private val MB = 1024L * 1024L
  private def onWindows: Boolean = ProbePlatform.current() == ProbePlatform.WindowsX64

  test("GlobalMemoryStatusEx fields map to used memory, commit and load") {
    val gb = 1024L * MB
    val out = Array(32 * gb, 20 * gb, 37L, 40 * gb, 15 * gb, 1L)
    WindowsProbes.fromStatus(out) shouldBe MachineSample(
      physicalMb = 32 * 1024,
      usedMb = 12 * 1024,
      roomFromUsed = true,
      pressure = RawPressure.Windows(memoryLoadPercent = 37, commitTotalMb = 25 * 1024, commitLimitMb = 40 * 1024, lowMemory = true)
    )
  }

  test("the probe reads what the JDK reads from GlobalMemoryStatusEx") {
    assume(onWindows)
    val probes = new WindowsProbes(MachineNative.load(Files.createTempDirectory("bleep-machine-native"), ProbePlatform.WindowsX64))
    val os = java.lang.management.ManagementFactory.getOperatingSystemMXBean.asInstanceOf[com.sun.management.OperatingSystemMXBean]
    val s = probes.sample()
    val jdkUsedMb = (os.getTotalMemorySize - os.getFreeMemorySize) / MB
    s.physicalMb shouldBe os.getTotalMemorySize / MB
    math.abs(s.usedMb - jdkUsedMb) should be <= 512L
    s.pressure match {
      case RawPressure.Windows(_, commitTotalMb, commitLimitMb, _) =>
        // The commit limit grows only when the page file does, so it can be compared exactly; committed memory moves.
        commitLimitMb shouldBe os.getTotalSwapSpaceSize / MB
        math.abs(commitTotalMb - (os.getTotalSwapSpaceSize - os.getFreeSwapSpaceSize) / MB) should be <= 512L
      case other => fail(s"not a Windows reading: $other")
    }
  }

  test("a pid that has never existed has no footprint") {
    assume(onWindows)
    val probes = new WindowsProbes(MachineNative.load(Files.createTempDirectory("bleep-machine-native"), ProbePlatform.WindowsX64))
    // Far above any process id Windows hands out (they are handle-table indices, in the tens of thousands). Not an odd number: the kernel ignores the
    // low two bits of a process id when looking it up.
    probes.footprintMb(2000000000L) shouldBe None
  }
}
