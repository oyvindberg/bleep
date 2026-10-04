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

  /** `vm_stat`'s output as a map of page counts, with the page size under "page size". */
  private def parseVmStat(output: String): Map[String, Long] = {
    val pageSize = "page size of (\\d+) bytes".r.findFirstMatchIn(output).map(_.group(1).toLong)
    val counts = output.linesIterator.flatMap { line =>
      line.split(":", 2) match {
        case Array(k, v) => v.trim.stripSuffix(".").toLongOption.map(k.trim -> _)
        case _           => None
      }
    }.toMap
    pageSize.fold(counts)(ps => counts.updated("page size", ps))
  }

  private def run(cmd: String*): String = {
    val p = new ProcessBuilder(cmd*).redirectErrorStream(true).start()
    val out = new String(p.getInputStream.readAllBytes(), "UTF-8")
    p.waitFor() shouldBe 0
    out
  }

  test("used memory is vm_stat's anonymous - purgeable + wired + occupied by compressor") {
    assume(onMac)
    val before = probes.sample()
    val stats = parseVmStat(run("/usr/bin/vm_stat"))
    val after = probes.sample()
    val pageSize = stats("page size")
    val vmStatUsedMb =
      (stats("Anonymous pages") - stats("Pages purgeable") + stats("Pages wired down") + stats("Pages occupied by compressor")) * pageSize / (1024 * 1024)
    // Memory moves between the readings; allow it to have moved by up to 512 MB in either direction.
    vmStatUsedMb should be >= (math.min(before.usedMb, after.usedMb) - 512)
    vmStatUsedMb should be <= (math.max(before.usedMb, after.usedMb) + 512)
  }

  test("physical memory and pressure level are what sysctl says") {
    assume(onMac)
    val s = probes.sample()
    s.physicalMb shouldBe run("/usr/sbin/sysctl", "-n", "hw.memsize").trim.toLong / (1024 * 1024)
    s.pressure match {
      case RawPressure.MacOs(level, _, _, _, _) => level shouldBe run("/usr/sbin/sysctl", "-n", "kern.memorystatus_vm_pressure_level").trim.toInt
      case other                                => fail(s"not a macOS reading: $other")
    }
  }

  test("available memory is vm_stat's free + speculative + purgeable, and within physical") {
    assume(onMac)
    val before = probes.sample()
    val stats = parseVmStat(run("/usr/bin/vm_stat"))
    val after = probes.sample()
    val vmStatAvailableMb = (stats("Pages free") + stats("Pages speculative") + stats("Pages purgeable")) * stats("page size") / (1024 * 1024)
    // Free pages move fast; allow 512 MB either way around the two probes.
    vmStatAvailableMb should be >= (math.min(before.availableMb, after.availableMb) - 512)
    vmStatAvailableMb should be <= (math.max(before.availableMb, after.availableMb) + 512)
    after.availableMb should be <= after.physicalMb
  }

  test("the compressor and swap counters are vm_stat's, cumulative and monotonic") {
    assume(onMac)
    val RawPressure.MacOs(_, c1, d1, si1, so1) = probes.sample().pressure: @unchecked
    val stats = parseVmStat(run("/usr/bin/vm_stat"))
    val RawPressure.MacOs(_, c2, d2, si2, so2) = probes.sample().pressure: @unchecked
    // Counters since boot only ever grow.
    c2 should be >= c1
    d2 should be >= d1
    si2 should be >= si1
    so2 should be >= so1
    // vm_stat prints the same quantities, and a wrong field would be off by orders of magnitude; but the kernel does not serialise these counters against a
    // process that ran in between — vm_stat read right after a probe has been seen a few hundred pages ahead of the probe read right after it — so the check is
    // closeness (one percent), not an ordering.
    def close(name: String, mine: Long): Unit =
      withClue(s"$name: vm_stat ${stats(name)} vs probe $mine: ")(math.abs(stats(name) - mine) should be <= math.max(1000L, mine / 100))
    close("Compressions", c2)
    close("Decompressions", d2)
    close("Swapins", si2)
    close("Swapouts", so2)
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
