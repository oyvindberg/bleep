package bleep.machine

import java.nio.file.Path

/** The probes for the machine this JVM runs on. */
case class Probes(machine: MachineProbe, fork: ForkProbe)

object Probes {

  /** Picks the implementation for this OS and architecture, and takes one reading with each so that a machine bleep cannot measure fails here, when the server
    * starts, rather than on the scheduler's first tick. Throws on a platform with no implementation. A machine without a pressure source (a Linux kernel
    * without PSI) is not one bleep cannot measure: its readings carry [[RawPressure.Unavailable]] and this does not throw.
    *
    * @param nativeLibDir
    *   where the JNI library is unpacked on macOS and Windows (a directory under bleep's cache dir); unused on Linux
    */
  def forThisMachine(nativeLibDir: Path): Probes = {
    val probes = ProbePlatform.current() match {
      case ProbePlatform.Linux =>
        val proc = Path.of("/proc")
        Probes(new LinuxMachineProbe(proc, Path.of("/sys/fs/cgroup")), new LinuxForkProbe(proc))
      case platform @ (ProbePlatform.MacOsArm64 | ProbePlatform.MacOsX64) =>
        val mac = new MacOsProbes(MachineNative.load(nativeLibDir, platform))
        Probes(mac, mac)
      case platform @ (ProbePlatform.WindowsX64 | ProbePlatform.WindowsArm64) =>
        val windows = new WindowsProbes(MachineNative.load(nativeLibDir, platform))
        Probes(windows, windows)
    }
    checked(probes)
  }

  /** `probes`, once each has taken a reading of this machine and this process. Throws if either cannot. */
  def checked(probes: Probes): Probes = {
    probes.machine.sample(): Unit
    probes.fork.footprintMb(ProcessHandle.current().pid()) match {
      case Some(_) => ()
      case None    => throw new IllegalStateException("The fork probe cannot see this very process")
    }
    probes
  }
}

/** The platforms bleep has probes for.
  *
  * [[MacOsX64]] and [[WindowsArm64]] are built but UNTESTED: their library is compiled (the x86_64 slice of the universal dylib, and a cross-compiled arm64
  * DLL), but bleep supports neither platform and CI has no runner to run them on. Nothing has ever checked that they load or read sensible numbers.
  */
sealed trait ProbePlatform
object ProbePlatform {
  case object Linux extends ProbePlatform
  case object MacOsArm64 extends ProbePlatform
  case object WindowsX64 extends ProbePlatform

  /** Built, never run. See [[ProbePlatform]]. */
  case object MacOsX64 extends ProbePlatform

  /** Built, never run. See [[ProbePlatform]]. */
  case object WindowsArm64 extends ProbePlatform

  def current(): ProbePlatform = from(System.getProperty("os.name"), System.getProperty("os.arch"))

  def from(osName: String, osArch: String): ProbePlatform = {
    val os = osName.toLowerCase(java.util.Locale.ROOT)
    val x64 = osArch == "amd64" || osArch == "x86_64"
    val arm64 = osArch == "aarch64"
    if (os.startsWith("linux")) Linux
    else if (os.startsWith("mac") && arm64) MacOsArm64
    else if (os.startsWith("mac") && x64) MacOsX64
    else if (os.startsWith("windows") && x64) WindowsX64
    else if (os.startsWith("windows") && arm64) WindowsArm64
    else throw new UnsupportedOperationException(s"bleep cannot measure memory on $osName/$osArch: it has probes for Linux, macOS and Windows on x64 and arm64")
  }
}
