package bleep.machine

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import java.nio.file.{Files, Path}

/** The `/proc` parsers, against files captured from real kernels. Runs on every OS: it is only parsing. */
class LinuxProcParseTest extends AnyFunSuite with Matchers {

  // Ubuntu 22.04, kernel 6.5, 16 GB
  private val meminfo =
    """MemTotal:       16374460 kB
      |MemFree:          811668 kB
      |MemAvailable:   11126808 kB
      |Buffers:          532148 kB
      |Cached:          9535796 kB
      |SwapCached:          212 kB
      |Active:          5042604 kB
      |Inactive:        9440708 kB
      |HugePages_Total:       0
      |HugePages_Free:        0
      |Hugepagesize:       2048 kB
      |DirectMap1G:     4194304 kB
      |""".stripMargin

  // A forked test JVM: most of its RSS is file-backed (RssFile), which is not its cost.
  private val status =
    """Name:	java
      |Umask:	0022
      |State:	S (sleeping)
      |Tgid:	4242
      |Pid:	4242
      |PPid:	1
      |VmPeak:	 6203440 kB
      |VmSize:	 6137904 kB
      |VmHWM:	  612340 kB
      |VmRSS:	  612340 kB
      |RssAnon:	  402100 kB
      |RssFile:	  195220 kB
      |RssShmem:	   15020 kB
      |VmData:	 1203440 kB
      |VmSwap:	   20480 kB
      |Threads:	31
      |voluntary_ctxt_switches:	1503
      |""".stripMargin

  private val zombieStatus =
    """Name:	java
      |State:	Z (zombie)
      |Tgid:	4242
      |Pid:	4242
      |Threads:	1
      |""".stripMargin

  private val psi =
    """some avg10=12.34 avg60=3.10 avg300=0.80 total=123456789
      |full avg10=1.50 avg60=0.20 avg300=0.05 total=2345678
      |""".stripMargin

  test("meminfo: used is total minus available") {
    LinuxProc.parseMeminfo(meminfo) shouldBe LinuxProc.Meminfo(totalKb = 16374460L, availableKb = 11126808L)
  }

  test("meminfo without MemAvailable is an error, not a guess") {
    val e = intercept[IllegalStateException](LinuxProc.parseMeminfo(meminfo.linesIterator.filterNot(_.startsWith("MemAvailable")).mkString("\n")))
    e.getMessage should include("MemAvailable")
  }

  test("status: footprint is RssAnon plus VmSwap, not VmRSS") {
    LinuxProc.parseStatusFootprintKb(status) shouldBe Some(402100L + 20480L)
  }

  test("status of a zombie: no footprint, the process has exited") {
    LinuxProc.parseStatusFootprintKb(zombieStatus) shouldBe None
  }

  test("status of a live process without the memory lines is an error") {
    intercept[IllegalStateException](LinuxProc.parseStatusFootprintKb(zombieStatus.replace("Z (zombie)", "S (sleeping)"))).getMessage should include("RssAnon")
  }

  test("a field name that is a prefix of another is not confused with it") {
    LinuxProc.lineValue("VmSwapX:\t1 kB\nVmSwap:\t2 kB\n", "VmSwap") shouldBe Some("2 kB")
    LinuxProc.lineValue("VmSwap:\t2 kB", "VmSwap") shouldBe Some("2 kB")
    LinuxProc.lineValue("XVmSwap:\t2 kB", "VmSwap") shouldBe None
  }

  test("PSI: avg10 of some and full") {
    LinuxProc.parsePsi(psi) shouldBe RawPressure.LinuxPsi(someAvg10 = 12.34, fullAvg10 = 1.5)
  }

  test("PSI on a kernel too old to report `full` for memory is an error") {
    intercept[IllegalStateException](LinuxProc.parsePsi(psi.linesIterator.next())).getMessage should include("full avg10=")
  }

  private def withProc(files: (String, String)*)(f: Path => Unit): Unit = {
    val root = Files.createTempDirectory("bleep-fake-proc")
    try {
      files.foreach { case (rel, content) =>
        val p = root.resolve(rel)
        Files.createDirectories(p.getParent)
        Files.writeString(p, content)
      }
      f(root)
    } finally {
      val paths = Files.walk(root)
      try paths.sorted(java.util.Comparator.reverseOrder[Path]()).forEach(p => Files.delete(p))
      finally paths.close()
    }
  }

  test("machine sample from a /proc tree") {
    withProc("meminfo" -> meminfo, "pressure/memory" -> psi) { root =>
      new LinuxMachineProbe(root).sample() shouldBe MachineSample(
        physicalMb = 16374460L / 1024,
        usedMb = (16374460L - 11126808L) / 1024,
        pressure = RawPressure.LinuxPsi(12.34, 1.5)
      )
    }
  }

  test("a kernel without PSI fails with an error that says what is missing") {
    withProc("meminfo" -> meminfo) { root =>
      val e = intercept[IllegalStateException](new LinuxMachineProbe(root).sample())
      e.getMessage should include("CONFIG_PSI")
    }
  }

  test("fork footprint: a pid with no /proc entry has exited") {
    withProc("meminfo" -> meminfo) { root =>
      new LinuxForkProbe(root).footprintMb(4242L) shouldBe None
    }
  }

  test("fork footprint from status") {
    withProc("4242/status" -> status) { root =>
      new LinuxForkProbe(root).footprintMb(4242L) shouldBe Some((402100L + 20480L) / 1024)
    }
  }

  test("fork footprint of a zombie is none") {
    withProc("4242/status" -> zombieStatus) { root =>
      new LinuxForkProbe(root).footprintMb(4242L) shouldBe None
    }
  }
}
