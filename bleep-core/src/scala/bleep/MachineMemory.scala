package bleep

import scala.util.Properties

/** How much more memory the machine could give us right now, without going to swap.
  *
  * The naive question — "how much memory is free" — has no useful answer on a modern OS, which keeps as much as it can in page cache and reclaims on demand.
  * Free-page counts sit near zero whether the machine is idle or desperate; measured on the machine this was developed against, `free` was 263MB while the
  * kernel's own pressure level said "normal".
  *
  * The answerable question is how much is held in memory that CANNOT simply be dropped:
  *   - '''anonymous''' memory — heap, stacks, native allocation — which has nowhere to go but swap or the compressor. This is what actually causes pressure.
  *   - '''wired''' memory, which cannot be moved at all.
  *
  * File-backed pages are deliberately NOT counted. A mapped jar is clean and disk-backed, so the kernel drops it for free; treating a fleet of forks sharing a
  * large classpath as if that classpath were a cost is what made an earlier version of this budget wrong in the opposite direction.
  *
  * Linux is asked directly rather than derived: `MemAvailable` is the kernel's own estimate of what a new allocation could get without swapping, which is
  * exactly this question and better answered by the kernel than by us. Windows has an equally good answer (`GlobalMemoryStatusEx`, whose "available" includes
  * repurposable standby cache) but no way to read it without native access, so it returns `None` and the caller keeps its static budget.
  */
trait MachineMemory {

  /** MB currently held in memory that cannot simply be dropped — anonymous plus wired, machine-wide, ours included. `None` when this platform can't say.
    *
    * Reported as a total rather than as "what's left" so the caller can subtract its OWN share (see [[budgetFor]]); a bare "available" figure cannot be
    * corrected that way, and treating it as if it could is what made the budget run away.
    */
  def unreclaimableMb: Option[Long]
}

object MachineMemory {

  object Unavailable extends MachineMemory {
    def unreclaimableMb: Option[Long] = None
  }

  /** `vm_stat` reports page counts; anonymous + wired is what can't be dropped. */
  object MacOs extends MachineMemory {
    def unreclaimableMb: Option[Long] =
      readVmStat().flatMap { stats =>
        for {
          pageSize <- stats.get("page size")
          anonymous <- stats.get("Anonymous pages")
          wired <- stats.get("Pages wired down")
        } yield (anonymous + wired) * pageSize / (1024L * 1024L)
      }

    private[bleep] def parse(output: String): Map[String, Long] = {
      val pageSize = "page size of (\\d+) bytes".r.findFirstMatchIn(output).map(_.group(1).toLong)
      val counts = output.linesIterator.flatMap { line =>
        line.split(":", 2) match {
          case Array(k, v) =>
            v.trim.stripSuffix(".").toLongOption.map(k.trim -> _)
          case _ => None
        }
      }.toMap
      pageSize.fold(counts)(ps => counts.updated("page size", ps))
    }

    private def readVmStat(): Option[Map[String, Long]] =
      try {
        val p = new ProcessBuilder("/usr/bin/vm_stat").start()
        val out = new String(p.getInputStream.readAllBytes(), "UTF-8")
        if (p.waitFor() == 0) Some(parse(out)) else None
      } catch { case _: java.io.IOException => None }
  }

  /** Derived from `MemAvailable`, the kernel's own estimate of what a new allocation could get without swapping. Expressed as its complement — total minus
    * available — so it means the same thing as the macOS figure and can have our own share subtracted from it.
    */
  object Linux extends MachineMemory {
    def unreclaimableMb: Option[Long] =
      readMeminfo().flatMap { info =>
        for {
          total <- info.get("MemTotal")
          available <- info.get("MemAvailable")
        } yield math.max(0L, (total - available) / 1024) // kB
      }

    private def readMeminfo(): Option[Map[String, Long]] =
      try {
        val path = java.nio.file.Path.of("/proc/meminfo")
        if (!java.nio.file.Files.exists(path)) None
        else
          Some(
            java.nio.file.Files
              .readAllLines(path)
              .toArray(Array.empty[String])
              .flatMap { line =>
                line.split(":", 2) match {
                  case Array(k, v) => v.trim.split("\\s+").headOption.flatMap(_.toLongOption).map(k.trim -> _)
                  case _           => None
                }
              }
              .toMap
          )
      } catch { case _: java.io.IOException => None }
  }

  lazy val system: MachineMemory =
    if (Properties.isMac) MacOs
    else if (Properties.isLinux) Linux
    else Unavailable

  /** Leave this much of the machine untouched even when it looks free. Reclaim is not instantaneous and our own estimate of a not-yet-started fork is only an
    * estimate, so running right up to the line is how a build discovers the OOM killer.
    */
  def slackMb(physicalMb: Long): Long = math.max(2048L, physicalMb / 16)

  /** The fork-memory budget: everything except what OTHER processes have made unreclaimable.
    *
    * `ourFootprintMb` is what our own process tree currently costs, MEASURED (see [[ProcessMemory]]) — not what the governor has reserved. Subtracting it from
    * the machine's unreclaimable total leaves what everything-that-isn't-bleep is holding, and the budget is whatever remains after that and slack.
    *
    * The subtraction is the entire point, and getting it wrong once produced a runaway. The previous formulation was relative — `holding + available - slack` —
    * on the reasoning that the OS's "available" already accounts for forks we are running. It does not account for forks we have just ADMITTED: their
    * reservation inflates `holding` immediately, while their memory only appears in `available` seconds later, so each admission raised the budget, which
    * admitted more. Observed on a 48GB machine: 19GB -> 34GB -> 48GB -> 63GB while forks were being spawned, and 78GB at peak.
    *
    * Stated absolutely there is no such loop. As our forks materialize, `ourFootprintMb` and the machine's unreclaimable total rise together and cancel, so
    * admitting work cannot manufacture evidence that there is room for more of it. Only genuine movement by other processes shifts the budget.
    */
  def budgetFor(ourFootprintMb: Long, machineUnreclaimableMb: Long, physicalMb: Long, floorMb: Long): Long = {
    val heldByOthers = math.max(0L, machineUnreclaimableMb - ourFootprintMb)
    math.max(floorMb, physicalMb - heldByOthers - slackMb(physicalMb))
  }
}
