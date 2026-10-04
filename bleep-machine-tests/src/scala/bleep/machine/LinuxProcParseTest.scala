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
    withProc("meminfo" -> meminfo, "pressure/memory" -> psi, "self/cgroup" -> "0::/\n") { root =>
      new LinuxMachineProbe(root, root.resolve("sys/fs/cgroup")).sample() shouldBe MachineSample(
        physicalMb = 16374460L / 1024,
        usedMb = (16374460L - 11126808L) / 1024,
        availableMb = 11126808L / 1024,
        pressure = RawPressure.LinuxPsi(12.34, 1.5)
      )
    }
  }

  test("a kernel without PSI has no pressure signal, and says what is missing; memory is still measured") {
    withProc("meminfo" -> meminfo, "self/cgroup" -> "0::/\n") { root =>
      val s = new LinuxMachineProbe(root, root.resolve("sys/fs/cgroup")).sample()
      s.usedMb shouldBe (16374460L - 11126808L) / 1024
      s.pressure match {
        case RawPressure.Unavailable(reason) =>
          reason should include("CONFIG_PSI")
          reason should include("psi=1")
        case other => fail(s"expected Unavailable, got $other")
      }
    }
  }

  test("the probes of a machine without PSI pass the startup check") {
    val self = ProcessHandle.current().pid()
    withProc("meminfo" -> meminfo, s"$self/status" -> status, "self/cgroup" -> "0::/\n") { root =>
      val probes = Probes.checked(Probes(new LinuxMachineProbe(root, root.resolve("sys/fs/cgroup")), new LinuxForkProbe(root)))
      probes.machine.sample().pressure shouldBe a[RawPressure.Unavailable]
    }
  }

  private val GiB = 1024L * 1024L * 1024L
  private val cgroupPsi = "some avg10=40.00 avg60=10.00 avg300=2.00 total=1\nfull avg10=5.00 avg60=1.00 avg300=0.20 total=1\n"
  private def containerFiles(maxBytes: String): List[(String, String)] = List(
    "meminfo" -> meminfo,
    "pressure/memory" -> psi,
    "self/cgroup" -> "0::/docker/abc\n",
    "sys/fs/cgroup/cgroup.controllers" -> "cpuset cpu io memory pids\n",
    "sys/fs/cgroup/docker/abc/memory.max" -> s"$maxBytes\n",
    "sys/fs/cgroup/docker/abc/memory.current" -> s"${3 * GiB}\n",
    "sys/fs/cgroup/docker/abc/memory.stat" -> s"anon ${2 * GiB}\nfile ${GiB}\nactive_file 0\ninactive_file ${GiB}\n",
    "sys/fs/cgroup/docker/abc/memory.pressure" -> cgroupPsi
  )
  private val hostSample = MachineSample(16374460L / 1024, (16374460L - 11126808L) / 1024, 11126808L / 1024, RawPressure.LinuxPsi(12.34, 1.5))

  test("cgroup v2 with a memory.max limit: the cgroup is the machine") {
    withProc(containerFiles((4 * GiB).toString)*) { root =>
      new LinuxMachineProbe(root, root.resolve("sys/fs/cgroup")).sample() shouldBe MachineSample(
        physicalMb = 4096,
        usedMb = 2048, // memory.current minus the inactive file cache it could drop
        availableMb = 2048, // memory.max less the working set
        pressure = RawPressure.LinuxPsi(40.0, 5.0)
      )
    }
  }

  test("cgroup v2: an ancestor's tighter limit binds") {
    withProc(
      (containerFiles("max") :+ ("sys/fs/cgroup/docker/memory.max" -> s"${8 * GiB}\n")) ++ List(
        "sys/fs/cgroup/docker/memory.current" -> s"${5 * GiB}\n",
        "sys/fs/cgroup/docker/memory.stat" -> s"inactive_file ${GiB}\n",
        "sys/fs/cgroup/docker/memory.pressure" -> cgroupPsi
      )*
    ) { root =>
      new LinuxMachineProbe(root, root.resolve("sys/fs/cgroup"))
        .sample() shouldBe MachineSample(8192, 4096, 4096, RawPressure.LinuxPsi(40.0, 5.0))
    }
  }

  test("cgroup v2 without a limit (`max`): the host's numbers") {
    withProc(containerFiles("max")*) { root =>
      new LinuxMachineProbe(root, root.resolve("sys/fs/cgroup")).sample() shouldBe hostSample
    }
  }

  test("cgroup v2 limit above the host's memory: the host's numbers") {
    withProc(containerFiles((64 * GiB).toString)*) { root =>
      new LinuxMachineProbe(root, root.resolve("sys/fs/cgroup")).sample() shouldBe hostSample
    }
  }

  test("cgroup v1 memory controller: the host's numbers, whatever v2 files exist") {
    withProc((containerFiles((4 * GiB).toString) :+ ("self/cgroup" -> "12:memory:/docker/abc\n11:cpu,cpuacct:/docker/abc\n0::/docker/abc\n"))*) { root =>
      new LinuxMachineProbe(root, root.resolve("sys/fs/cgroup")).sample() shouldBe hostSample
    }
  }

  test("a cgroup that /proc/self/cgroup names but the hierarchy lacks is an error") {
    withProc((containerFiles((4 * GiB).toString) :+ ("self/cgroup" -> "0::/elsewhere\n"))*) { root =>
      intercept[IllegalStateException](new LinuxMachineProbe(root, root.resolve("sys/fs/cgroup"))).getMessage should include("/elsewhere")
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
