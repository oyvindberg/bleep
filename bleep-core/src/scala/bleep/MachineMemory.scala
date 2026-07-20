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

  /** Additional MB the machine could hand out before it would have to swap. `None` when this platform can't say. */
  def availableForMoreMb: Option[Long]
}

object MachineMemory {

  object Unavailable extends MachineMemory {
    def availableForMoreMb: Option[Long] = None
  }

  /** `vm_stat` reports page counts; anonymous + wired is what can't be dropped. */
  object MacOs extends MachineMemory {
    def availableForMoreMb: Option[Long] =
      readVmStat().flatMap { stats =>
        for {
          pageSize <- stats.get("page size")
          anonymous <- stats.get("Anonymous pages")
          wired <- stats.get("Pages wired down")
        } yield {
          val physicalMb = MachineResources.physicalMemoryMb(fallbackMb = 4096)
          val unreclaimableMb = (anonymous + wired) * pageSize / (1024L * 1024L)
          math.max(0L, physicalMb - unreclaimableMb)
        }
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

  /** `MemAvailable` in `/proc/meminfo` — the kernel's own answer, already accounting for reclaimable cache. */
  object Linux extends MachineMemory {
    def availableForMoreMb: Option[Long] =
      try {
        val path = java.nio.file.Path.of("/proc/meminfo")
        if (!java.nio.file.Files.exists(path)) None
        else
          java.nio.file.Files
            .readAllLines(path)
            .toArray(Array.empty[String])
            .collectFirst { case l if l.startsWith("MemAvailable:") => l }
            .flatMap(_.split("\\s+").lift(1).flatMap(_.toLongOption))
            .map(_ / 1024) // kB
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

  /** The fork-memory budget to run with, given what we are already holding.
    *
    * Expressed relative to current usage rather than as an absolute: the OS tells us what is still available, and whatever we already hold is by definition
    * already accounted for in that figure. So the total we may hold is "what we hold now, plus what is left, minus slack" — which self-corrects as other
    * processes come and go, without us having to model them.
    */
  def budgetFor(currentlyUsedMb: Long, availableForMoreMb: Long, physicalMb: Long, floorMb: Long): Long =
    math.max(floorMb, currentlyUsedMb + availableForMoreMb - slackMb(physicalMb))
}
