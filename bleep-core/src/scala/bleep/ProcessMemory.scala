package bleep

import scala.jdk.CollectionConverters._
import scala.util.Properties

/** What a process actually costs the machine, measured rather than predicted.
  *
  * The metric is deliberately NOT resident set size. RSS counts every page a process has mapped, including pages it shares with other processes — and a fleet
  * of forked test JVMs shares a great deal: the JDK itself, and every jar on a classpath that is identical across forks. Summing RSS over ten such forks counts
  * the same mapped jars ten times. Measured on a real build, forks reported a median RSS of 610MB while the JVM's own view of its committed heap was 66MB;
  * almost all of the difference was mapped classpath.
  *
  * Every platform has a name for the metric that excludes that double-count, and they mean the same thing:
  *   - macOS: `phys_footprint` (and its lifetime peak), via the `proc_pid_rusage` syscall
  *   - Linux: `Pss` — proportional set size, shared pages divided by the number of sharers — via `/proc/<pid>/smaps_rollup`
  *   - Windows: private working set
  *
  * Only the first two are implemented. Windows returns [[Unavailable]] and callers fall back to the declared bound, which is what they used before any of this
  * existed — less adaptive, never wrong.
  *
  * This deliberately does not measure the *machine*. "How much memory is free" is unanswerable on a modern OS, which keeps as much as it can in cache and frees
  * on demand, so free-page counts stay near zero whether the machine is idle or desperate. What we can answer exactly is how much our own processes cost, and
  * that is the number that had been guesswork.
  */
trait ProcessMemory {

  /** Current proportional cost of `pid` in MB. `None` if the process is gone or the platform can't say. */
  def footprintMb(pid: Long): Option[Long]

  /** Highest proportional cost `pid` has reached since it started, where the platform tracks it for us.
    *
    * Worth having separately: a peak is what a cost estimate wants (a suite is as expensive as its worst moment, not the moment we happened to look), and a
    * platform that records it saves us polling to catch the spike.
    */
  def peakFootprintMb(pid: Long): Option[Long]
}

object ProcessMemory {

  /** Never answers. Windows, and anything unrecognised. */
  object Unavailable extends ProcessMemory {
    def footprintMb(pid: Long): Option[Long] = None
    def peakFootprintMb(pid: Long): Option[Long] = None
  }

  /** `phys_footprint`, read from the kernel via `proc_pid_rusage` — the same source `/usr/bin/footprint` reads.
    *
    * This used to shell out to `/usr/bin/footprint -p <pid>`, one process per pid. [[ourTreeFootprintMb]] sweeps the daemon's whole process tree every
    * [[MachineResources.BudgetRetuneInterval]], so on a busy server that was tens of spawns per sweep, each costing ~35ms of address-space survey (measured
    * against a 6GB-heap JVM), plus parsing the output — which showed up as 2% of the daemon's total heap allocation in a profile. The syscall answers in
    * microseconds and touches one 296-byte buffer.
    *
    * Resident set size is NOT a cheaper substitute, tempting as `ps` looks. Measured against that same 6GB-heap JVM, `phys_footprint` reported 7190MB while RSS
    * reported 2933MB: macOS compresses pages, and RSS stops counting them once compressed. Subtracting a number 2.5x too small from the machine total is
    * exactly the class of error that produced a 63GB fork budget on a 48GB machine.
    */
  object MacOs extends ProcessMemory {

    /** `struct rusage_info_v4` (`sys/resource.h`): 16 bytes of uuid followed by 35 `uint64_t`, 296 bytes in total. We want two of those fields, so we index
      * them directly rather than describing the whole struct. Offsets are `offsetof` on macOS 15 and derive as `16 + n*8`: `ri_phys_footprint` is the 8th
      * `uint64_t` (72) and `ri_lifetime_max_phys_footprint` the 29th (240). This is a stable public ABI, but it IS an ABI assumption — the live test in
      * ForkCostModelTest reads this JVM's own footprint and asserts the value is plausible, which is what would catch a layout that moved.
      */
    private final val StructBytes = 296L
    private final val PhysFootprintOffset = 72L
    private final val LifetimeMaxPhysFootprintOffset = 240L
    private final val RusageInfoV4 = 4

    /** `int proc_pid_rusage(int pid, int flavor, rusage_info_t *buffer)`, from libSystem. Resolved once; a macOS without it is not a macOS we can run on, so
      * failing to find it throws rather than quietly degrading to "cannot measure".
      */
    private lazy val procPidRusage: java.lang.invoke.MethodHandle = {
      val linker = java.lang.foreign.Linker.nativeLinker()
      val symbol = linker
        .defaultLookup()
        .find("proc_pid_rusage")
        .orElseThrow(() => new RuntimeException("proc_pid_rusage is missing from libSystem — cannot measure process memory on this macOS"))
      linker.downcallHandle(
        symbol,
        java.lang.foreign.FunctionDescriptor.of(
          java.lang.foreign.ValueLayout.JAVA_INT,
          java.lang.foreign.ValueLayout.JAVA_INT,
          java.lang.foreign.ValueLayout.JAVA_INT,
          java.lang.foreign.ValueLayout.ADDRESS
        )
      )
    }

    def footprintMb(pid: Long): Option[Long] = read(pid, PhysFootprintOffset)
    def peakFootprintMb(pid: Long): Option[Long] = read(pid, LifetimeMaxPhysFootprintOffset)

    private def read(pid: Long, offset: Long): Option[Long] = {
      val arena = java.lang.foreign.Arena.ofConfined()
      try {
        val buffer = arena.allocate(StructBytes)
        // invokeWithArguments rather than invokeExact: it boxes, but this runs a handful of times per
        // retune and the alternative depends on Scala's handling of signature-polymorphic calls.
        val rc = procPidRusage.invokeWithArguments(Integer.valueOf(pid.toInt), Integer.valueOf(RusageInfoV4), buffer).asInstanceOf[Integer]
        // Non-zero is ESRCH: the process exited between being listed and being measured. That is the
        // expected race in a tree sweep, not an error worth surfacing.
        if (rc.intValue() != 0) None
        else Some(buffer.get(java.lang.foreign.ValueLayout.JAVA_LONG, offset) / (1024L * 1024L))
      } finally arena.close()
    }
  }

  /** `/proc/<pid>/smaps_rollup` carries a single `Pss:` line covering the whole address space — one cheap file read, no process spawn. The kernel does not
    * track a high-water Pss, so [[peakFootprintMb]] has nothing to return and callers sample instead.
    */
  object Linux extends ProcessMemory {
    def footprintMb(pid: Long): Option[Long] =
      try {
        val path = java.nio.file.Path.of(s"/proc/$pid/smaps_rollup")
        if (!java.nio.file.Files.exists(path)) None
        else
          java.nio.file.Files
            .readAllLines(path)
            .toArray(Array.empty[String])
            .collectFirst { case l if l.startsWith("Pss:") => l }
            .flatMap(l => l.split("\\s+").lift(1).flatMap(_.toLongOption))
            .map(_ / 1024) // reported in kB
      } catch { case _: java.io.IOException => None }

    def peakFootprintMb(pid: Long): Option[Long] = None
  }

  lazy val system: ProcessMemory =
    if (Properties.isMac) MacOs
    else if (Properties.isLinux) Linux
    else Unavailable

  /** What this process and everything it has spawned currently cost the machine, in MB.
    *
    * The tree, not just this process: the forks are the expensive part, and they are exactly the descendants of the daemon. `ProcessHandle.descendants` gives
    * them without the daemon having to track pids, and it covers test runners, sourcegen and KSP alike.
    *
    * Summed with the proportional metric, so pages shared between forks — the identical mapped classpath, above all — are not counted once per fork. `None` if
    * the platform can't measure at all; a single unreadable pid (one that exited mid-sweep) is skipped rather than failing the sweep.
    */
  def ourTreeFootprintMb(self: ProcessHandle): Option[Long] =
    if (system eq Unavailable) None
    else {
      val descendants = self.descendants().iterator().asScala.map(_.pid()).toList
      val measured = (self.pid() :: descendants).flatMap(system.footprintMb)
      if (measured.isEmpty) None else Some(measured.sum)
    }
}
