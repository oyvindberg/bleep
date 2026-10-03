package bleep.machine

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import java.nio.file.Files

/** The macOS probe reads the same numbers as the tools a person would check it against: `vm_stat`, `sysctl`, and the FFM reading of `proc_pid_rusage` the old
  * code used. This is what pins the `vm_statistics64` field mapping — a wrong field is off by gigabytes, not by the drift between two readings.
  *
  * macOS only, by nature: the tools it compares against exist nowhere else. [[ProbesLiveTest]] covers every OS.
  */
class MacOsProbesTest extends AnyFunSuite with Matchers {
  private def onMac: Boolean = ProbePlatform.current() == ProbePlatform.MacOsArm64

  private lazy val probes = new MacOsProbes(MachineNative.load(Files.createTempDirectory("bleep-machine-native"), ProbePlatform.MacOsArm64))

  private def run(cmd: String*): String = {
    val p = new ProcessBuilder(cmd*).redirectErrorStream(true).start()
    val out = new String(p.getInputStream.readAllBytes(), "UTF-8")
    p.waitFor() shouldBe 0
    out
  }

  test("used memory is vm_stat's anonymous + wired + occupied by compressor") {
    assume(onMac)
    val before = probes.sample()
    val stats = bleep.MachineMemory.MacOs.parse(run("/usr/bin/vm_stat"))
    val after = probes.sample()
    val pageSize = stats("page size")
    val vmStatUsedMb = (stats("Anonymous pages") + stats("Pages wired down") + stats("Pages occupied by compressor")) * pageSize / (1024 * 1024)
    // Memory moves between the readings; allow it to have moved by up to 512 MB in either direction.
    vmStatUsedMb should be >= (math.min(before.usedMb, after.usedMb) - 512)
    vmStatUsedMb should be <= (math.max(before.usedMb, after.usedMb) + 512)
  }

  test("physical memory and pressure level are what sysctl says") {
    assume(onMac)
    val s = probes.sample()
    s.physicalMb shouldBe run("/usr/sbin/sysctl", "-n", "hw.memsize").trim.toLong / (1024 * 1024)
    s.pressure shouldBe RawPressure.MacOs(run("/usr/sbin/sysctl", "-n", "kern.memorystatus_vm_pressure_level").trim.toInt)
  }

  test("footprint is the same phys_footprint the FFM reading saw") {
    assume(onMac)
    val pid = ProcessHandle.current().pid()
    val ffm = bleep.ProcessMemory.MacOs.footprintMb(pid).getOrElse(fail("FFM reading saw no footprint"))
    val jni = probes.footprintMb(pid).getOrElse(fail("JNI reading saw no footprint"))
    math.abs(jni - ffm) should be <= 64L
  }

  test("a pid that has never existed has no footprint") {
    assume(onMac)
    // Above kern.maxproc's pid range (99998 on macOS), so it cannot be a live process.
    probes.footprintMb(999999L) shouldBe None
  }
}
