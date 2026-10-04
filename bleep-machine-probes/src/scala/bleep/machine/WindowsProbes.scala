package bleep.machine

/** The Windows probes, through the JNI library (`bleep_machine.c`). x64 only, like bleep's Windows release.
  *
  * '''Used memory''' is `ullTotalPhys − ullAvailPhys` from `GlobalMemoryStatusEx`. Windows' "available" already includes the standby list (cache it can
  * repurpose at once), so this is the counterpart of Linux's `MemTotal − MemAvailable`. The same numbers are what
  * `com.sun.management.OperatingSystemMXBean.getTotalMemorySize`/`getFreeMemorySize` return on Windows (OpenJDK calls `GlobalMemoryStatusEx` too), but the
  * low-memory notification and per-process private bytes need the library anyway, so one native call reads them all.
  *
  * '''Pressure''' is the rest of `GlobalMemoryStatusEx` — `dwMemoryLoad`, and committed memory against the commit limit, since Windows fails allocations when
  * commit runs out even with physical memory to spare — plus the system's own `LowMemoryResourceNotification`.
  *
  * '''A fork's footprint''' is `PrivateUsage` from `GetProcessMemoryInfo`: its private committed memory, resident or paged out. Shared and image-backed pages
  * (the JDK's DLLs, mapped jars) are excluded, which is what `phys_footprint` excludes on macOS.
  *
  * The notification handle is created once per probe and held for its life; a server makes one probe.
  */
final class WindowsProbes(native: MachineNative) extends MachineProbe with ForkProbe {
  import WindowsProbes.*

  private val lowMemoryNotification: Long = {
    val h = native.winLowMemoryNotification()
    if (h <= 0) throw new IllegalStateException(s"CreateMemoryResourceNotification(LowMemoryResourceNotification) failed: GetLastError ${-h}")
    h
  }

  def sample(): MachineSample = {
    val out = new Array[Long](6)
    native.winSample(lowMemoryNotification, out) match {
      case 0 => ()
      case 1 => throw new IllegalStateException(s"GlobalMemoryStatusEx failed: GetLastError ${out(0)}")
      case 2 => throw new IllegalStateException(s"QueryMemoryResourceNotification failed: GetLastError ${out(0)}")
      case s => throw new IllegalStateException(s"winSample returned unknown status $s")
    }
    fromStatus(out)
  }

  def footprintMb(pid: Long): Option[Long] = {
    val r = native.winFootprint(Math.toIntExact(pid))
    if (r >= 0) Some(r / MB)
    else if (r == Gone) None
    else throw new IllegalStateException(s"Reading the memory of process $pid failed: GetLastError ${-r}")
  }
}

object WindowsProbes {
  private final val MB = 1024L * 1024L

  /** `WIN_GONE` in the C source. */
  private final val Gone = Long.MinValue

  /** `out` as `winSample` fills it. */
  def fromStatus(out: Array[Long]): MachineSample = {
    val totalPhys = out(0)
    val availPhys = out(1)
    val totalPageFile = out(3)
    val availPageFile = out(4)
    MachineSample(
      physicalMb = totalPhys / MB,
      usedMb = (totalPhys - availPhys) / MB,
      roomFromUsed = true,
      pressure = RawPressure.Windows(
        memoryLoadPercent = out(2).toInt,
        commitTotalMb = (totalPageFile - availPageFile) / MB,
        commitLimitMb = totalPageFile / MB,
        lowMemory = out(5) == 1L
      )
    )
  }
}
