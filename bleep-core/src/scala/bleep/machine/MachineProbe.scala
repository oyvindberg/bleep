package bleep.machine

/** What the machine-wide scheduler needs to know about the machine, read in-process. See `machine-scheduler-design.md` §9.
  *
  * One implementation per OS: Linux reads `/proc`, macOS and Windows call a small JNI library shipped with bleep. Every implementation works on any JDK the
  * build runs on, and none forks a process. A reading that cannot be taken throws — there is no "unavailable" answer, because a scheduler deciding on a guess
  * is how the machine gets overcommitted.
  *
  * Both probes are called on the scheduler's tick thread, up to every 10 ms, so a call must cost microseconds, not milliseconds.
  */
trait MachineProbe {

  /** One reading of the whole machine. Throws if the platform call fails. */
  def sample(): MachineSample
}

/** @param physicalMb
  *   installed physical memory
  * @param usedMb
  *   memory that cannot be reclaimed without someone paying for it. macOS: anonymous + wired + compressor-occupied pages. Linux: `MemTotal − MemAvailable`.
  *   Windows: total − available physical (`GlobalMemoryStatusEx`).
  * @param pressure
  *   the platform's own pressure signal, unnormalised. The scheduler maps it to Normal/Elevated/Critical.
  */
/** @param usedMb
  *   memory in use as the platform counts it — for display and metrics, never for room (design §5 rule 1): on macOS it does not move between a calm machine and
  *   an overloaded one (anonymous pages become compressor pages, §9)
  * @param availableMb
  *   what a new process can take now without the OS reclaiming anything: Linux `MemAvailable` (or a cgroup's limit less its working set), Windows available
  *   physical, macOS free + speculative + purgeable pages. Room for forks is this, less a reserve, less what starting forks are charged.
  */
case class MachineSample(physicalMb: Long, usedMb: Long, availableMb: Long, pressure: RawPressure)

/** The OS's own judgement of memory trouble, as the OS reports it. Each variant carries exactly what its platform exposes. */
sealed trait RawPressure
object RawPressure {

  /** macOS: the kernel's `kern.memorystatus_vm_pressure_level` (1 normal, 2 warning, 4 critical) and the cumulative page counters of `vm_statistics64` —
    * compressions, decompressions, swapins, swapouts — since boot. The counters are reported raw; [[Churn]] rates them, since a rate needs two samples and the
    * probe has one. The level alone lags an overload by tens of seconds (design §9); the churn is what moves first.
    */
  case class MacOs(level: Int, compressions: Long, decompressions: Long, swapins: Long, swapouts: Long) extends RawPressure {

    /** Pages the compressor moved, in or out: the one counter that climbs with kernel_task's CPU on an overloaded Mac. */
    def churnPages: Long = compressions + decompressions
  }

  /** `/proc/pressure/memory` (PSI): share of the last 10 s in which some / all non-idle tasks were stalled on memory, in percent. */
  case class LinuxPsi(someAvg10: Double, fullAvg10: Double) extends RawPressure

  /** `GlobalMemoryStatusEx` and `QueryMemoryResourceNotification(LowMemoryResourceNotification)`.
    *
    * @param memoryLoadPercent
    *   `dwMemoryLoad`, 0–100
    * @param commitTotalMb
    *   committed memory (`ullTotalPageFile − ullAvailPageFile`)
    * @param commitLimitMb
    *   the commit limit (`ullTotalPageFile`)
    * @param lowMemory
    *   the system's low-memory resource notification is signalled
    */
  case class Windows(memoryLoadPercent: Int, commitTotalMb: Long, commitLimitMb: Long, lowMemory: Boolean) extends RawPressure

  /** The platform has no pressure source here — e.g. a Linux kernel built without PSI, or booted with `psi=0`. Not a failure: the scheduler still has used
    * memory against the ceiling and only loses its pressure brake. Reported once at startup and in `bleep server top`.
    */
  case class Unavailable(reason: String) extends RawPressure
}

/** What one forked process currently costs the machine, read in-process. macOS: `phys_footprint` (`proc_pid_rusage`). Linux: from `/proc/<pid>/smaps_rollup`.
  * Windows: private bytes (`GetProcessMemoryInfo`).
  */
trait ForkProbe {

  /** Current footprint of `pid` in MB. `None` only when the process no longer exists — a fork that exited between the scheduler choosing to measure it and the
    * measurement is a normal event. Any other failure throws.
    */
  def footprintMb(pid: Long): Option[Long]
}
