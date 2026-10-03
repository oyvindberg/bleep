package bleep.machine

/** The macOS probes, through the JNI library (`bleep_machine.c`). Apple silicon only.
  *
  * '''Used memory''' is what `vm_stat` calls anonymous − purgeable + wired + occupied by compressor, read from `host_statistics64` (`internal_page_count −
  * purgeable_count + wire_count + compressor_page_count`) times the kernel page size. That is the memory nobody gets back without paying for it: anonymous
  * pages can only go to the compressor or swap, wired pages cannot move, and the compressor's own pages are the anonymous memory already squeezed out — leaving
  * them out is what made the old budget believe a machine with 7 GB in the compressor was nearly empty.
  *
  * Purgeable pages are anonymous but not a cost: an app marks them as a discardable cache (`VM_PURGABLE`, `NSPurgeableData`), and under pressure the kernel
  * simply drops them, with no compression and no swap. Activity Monitor subtracts them for "App Memory" for the same reason. File-backed pages are not counted
  * either; the kernel drops them for free.
  *
  * '''Pressure''' is `kern.memorystatus_vm_pressure_level`, the kernel's own verdict (what Activity Monitor's pressure graph shows).
  *
  * '''A fork's footprint''' is `ri_phys_footprint` from `proc_pid_rusage(RUSAGE_INFO_V4)` — what `footprint(1)` and Activity Monitor's "Memory" column report:
  * the process's dirty private memory including what has been compressed, excluding shared and clean file-backed pages.
  *
  * The host port is taken once per probe and held for its life; a server makes one probe.
  */
final class MacOsProbes(native: MachineNative) extends MachineProbe with ForkProbe {
  import MacOsProbes.*

  private val hostPort: Long = native.macHostPort()

  def sample(): MachineSample = {
    val out = new Array[Long](7)
    native.macSample(hostPort, out) match {
      case 0 => ()
      case 1 => throw new IllegalStateException(s"host_statistics64(HOST_VM_INFO64) failed: kern_return_t ${out(0)}")
      case 2 => throw new IllegalStateException(s"sysctlbyname(kern.memorystatus_vm_pressure_level) failed: errno ${out(0)}")
      case 3 => throw new IllegalStateException(s"sysctlbyname(hw.memsize) failed: errno ${out(0)}")
      case s => throw new IllegalStateException(s"macSample returned unknown status $s")
    }
    fromCounts(out)
  }

  def footprintMb(pid: Long): Option[Long] = {
    val r = native.macFootprint(Math.toIntExact(pid))
    if (r >= 0) Some(r / MB)
    else if (-r == ESRCH) None
    else throw new IllegalStateException(s"proc_pid_rusage($pid) failed: errno ${-r}")
  }
}

object MacOsProbes {
  private final val MB = 1024L * 1024L
  private final val ESRCH = 3L

  /** `out` as `macSample` fills it. */
  def fromCounts(out: Array[Long]): MachineSample = {
    val pageSize = out(1)
    MachineSample(
      physicalMb = out(0) / MB,
      usedMb = (out(2) - out(6) + out(3) + out(4)) * pageSize / MB,
      pressure = RawPressure.MacOs(out(5).toInt)
    )
  }
}
