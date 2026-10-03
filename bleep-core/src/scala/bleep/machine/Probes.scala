package bleep.machine

import java.nio.file.Path

/** The probes for the machine this JVM runs on. */
case class Probes(machine: MachineProbe, fork: ForkProbe)

object Probes {

  /** Picks the implementation for this OS and architecture, and takes one reading with each so that a machine bleep cannot measure fails here, when the server
    * starts, rather than on the scheduler's first tick. Throws on a platform with no implementation.
    *
    * @param nativeLibDir
    *   where the JNI library is unpacked on macOS and Windows (a directory under bleep's cache dir); unused on Linux
    */
  def forThisMachine(nativeLibDir: Path): Probes = {
    val probes = ProbePlatform.current() match {
      case ProbePlatform.Linux =>
        val proc = Path.of("/proc")
        Probes(new LinuxMachineProbe(proc), new LinuxForkProbe(proc))
      case ProbePlatform.MacOsArm64 =>
        val mac = new MacOsProbes(MachineNative.load(nativeLibDir, ProbePlatform.MacOsArm64))
        Probes(mac, mac)
      case other =>
        throw new UnsupportedOperationException(s"bleep has no memory probes for $other yet (native library dir: $nativeLibDir)")
    }
    probes.machine.sample(): Unit
    probes.fork.footprintMb(ProcessHandle.current().pid()) match {
      case Some(_) => ()
      case None    => throw new IllegalStateException("The fork probe cannot see this very process")
    }
    probes
  }
}

/** The platforms bleep can measure. Intel macOS is deliberately absent: bleep no longer supports it. */
sealed trait ProbePlatform
object ProbePlatform {
  case object Linux extends ProbePlatform
  case object MacOsArm64 extends ProbePlatform
  case object WindowsX64 extends ProbePlatform

  def current(): ProbePlatform = from(System.getProperty("os.name"), System.getProperty("os.arch"))

  def from(osName: String, osArch: String): ProbePlatform = {
    val os = osName.toLowerCase(java.util.Locale.ROOT)
    if (os.startsWith("linux")) Linux
    else if (os.startsWith("mac") && osArch == "aarch64") MacOsArm64
    else if (os.startsWith("windows") && (osArch == "amd64" || osArch == "x86_64")) WindowsX64
    else throw new UnsupportedOperationException(s"bleep cannot measure memory on $osName/$osArch: supported are Linux, macOS on arm64 and Windows on x64")
  }
}
