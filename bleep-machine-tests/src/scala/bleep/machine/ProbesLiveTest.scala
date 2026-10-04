package bleep.machine

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import java.nio.file.{Files, Path}
import java.util.concurrent.TimeUnit

/** The probes against the real machine this runs on — every OS bleep supports runs it in CI. Exact numbers are unknowable, so these are plausibility checks
  * that a wrong struct field, a wrong unit or a wrong page size cannot pass.
  */
class ProbesLiveTest extends AnyFunSuite with Matchers {
  private val MB = 1024L * 1024L

  private lazy val nativeLibDir: Path = Files.createTempDirectory("bleep-probes-native")
  private lazy val probes: Probes = Probes.forThisMachine(nativeLibDir)

  test("machine: physical memory is what the JVM sees, used memory is within it") {
    val s = probes.machine.sample()
    val jvmPhysicalMb = java.lang.management.ManagementFactory.getOperatingSystemMXBean match {
      case os: com.sun.management.OperatingSystemMXBean => os.getTotalMemorySize / MB
      case other                                        => fail(s"no com.sun.management.OperatingSystemMXBean: $other")
    }
    s.physicalMb should be > 0L
    // Same quantity from two sources. Allow 1%: Linux's MemTotal excludes kernel-reserved memory the JVM's figure may include, and rounding differs.
    math.abs(s.physicalMb - jvmPhysicalMb).toDouble should be <= (jvmPhysicalMb * 0.01 + 1)
    s.usedMb should be > 0L
    s.usedMb should be <= s.physicalMb
    // This JVM alone is using memory, so a machine that looks empty is a misread.
    s.usedMb should be >= ((Runtime.getRuntime.totalMemory() - Runtime.getRuntime.freeMemory()) / MB)
    s.availableMb should be >= 0L
    s.availableMb should be <= s.physicalMb
  }

  test("machine: pressure fields are in range") {
    probes.machine.sample().pressure match {
      case RawPressure.MacOs(level, compressions, decompressions, swapins, swapouts) =>
        Set(1, 2, 4) should contain(level)
        List(compressions, decompressions, swapins, swapouts).foreach(_ should be >= 0L)
      case RawPressure.LinuxPsi(some, full) =>
        some should (be >= 0.0 and be <= 100.0)
        full should (be >= 0.0 and be <= 100.0)
        full should be <= some // all tasks stalled implies some tasks stalled
      case RawPressure.Windows(load, commitTotalMb, commitLimitMb, _) =>
        load should (be >= 0 and be <= 100)
        commitTotalMb should be > 0L
        commitTotalMb should be <= commitLimitMb
      case RawPressure.Unavailable(reason) =>
        // Only a Linux kernel without PSI; the reason is what the user is told.
        ProbePlatform.current() shouldBe ProbePlatform.Linux
        reason should not be empty
    }
  }

  test("fork probe: this process costs at least the heap it has touched, and less than the machine") {
    val keep = Array.fill(8)(new Array[Byte](8 * MB.toInt))
    keep.foreach(a => java.util.Arrays.fill(a, 1.toByte)) // touched, so resident (or compressed/swapped, which counts too)
    val usedHeapMb = (Runtime.getRuntime.totalMemory() - Runtime.getRuntime.freeMemory()) / MB
    val footprint = probes.fork.footprintMb(ProcessHandle.current().pid()).getOrElse(fail("no footprint for this very process"))
    footprint should be >= 64L
    footprint should be >= usedHeapMb / 2
    footprint should be < probes.machine.sample().physicalMb
    keep.length shouldBe 8
  }

  private def startChildJvm(): Process = {
    val java = ProcessHandle.current().info().command().orElseThrow(() => new IllegalStateException("cannot tell which java runs this test"))
    new ProcessBuilder(java, "-Xmx32m", "-cp", System.getProperty("java.class.path"), "bleep.machine.SleepingChild")
      .redirectErrorStream(true)
      .redirectOutput(ProcessBuilder.Redirect.DISCARD)
      .start()
  }

  test("fork probe: a running child JVM has a footprint; once it has exited it has none") {
    val child = startChildJvm()
    try {
      // Give the child time to start its JVM; it is measured, not waited on, so a few hundred ms of startup is plenty.
      Thread.sleep(1000)
      child.isAlive shouldBe true
      val footprint = probes.fork.footprintMb(child.pid()).getOrElse(fail("no footprint for a live child"))
      footprint should be > 0L
      footprint should be < 1024L
    } finally {
      child.destroyForcibly()
      child.waitFor(30, TimeUnit.SECONDS) shouldBe true
    }
    // Reaped by the JDK, but on Windows the Process object still holds a handle, so the process object outlives the process. Still gone.
    probes.fork.footprintMb(child.pid()) shouldBe None
  }

  test("cost per call: microseconds, since the scheduler may call both every 10 ms") {
    val pid = ProcessHandle.current().pid()
    val warmup = 2000
    val n = 5000
    (1 to warmup).foreach { _ =>
      probes.machine.sample()
      probes.fork.footprintMb(pid)
    }
    val t0 = System.nanoTime()
    (1 to n).foreach(_ => probes.machine.sample())
    val t1 = System.nanoTime()
    (1 to n).foreach(_ => probes.fork.footprintMb(pid))
    val t2 = System.nanoTime()
    val sampleUs = (t1 - t0) / 1000.0 / n
    val footprintUs = (t2 - t1) / 1000.0 / n
    val report = f"sample(): $sampleUs%.1f µs/call, footprintMb(self): $footprintUs%.1f µs/call"
    info(report)
    // Generous, for noisy CI runners: a guard against a probe that forks or walks a process's memory, not a benchmark. Measured on GitHub runners,
    // sample() / footprintMb(): macOS 4-8 / 1-2 µs, Windows 1-2 / 3-4 µs, Linux 23 / 14 µs. Reading Linux footprints from smaps_rollup instead was
    // 1.7 ms for this very JVM, which is what this would catch.
    sampleUs should be < 300.0
    footprintUs should be < 300.0
  }
}

/** A child process to measure: a JVM that does nothing until it is killed. */
object SleepingChild {
  def main(args: Array[String]): Unit = Thread.sleep(120000)
}
