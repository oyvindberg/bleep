package bleep

import scala.util.Properties

/** What a process actually costs the machine, measured rather than predicted.
  *
  * The metric is deliberately NOT resident set size. RSS counts every page a process has mapped, including pages it shares with other processes — and a fleet
  * of forked test JVMs shares a great deal: the JDK itself, and every jar on a classpath that is identical across forks. Summing RSS over ten such forks counts
  * the same mapped jars ten times. Measured on a real build, forks reported a median RSS of 610MB while the JVM's own view of its committed heap was 66MB;
  * almost all of the difference was mapped classpath.
  *
  * Every platform has a name for the metric that excludes that double-count, and they mean the same thing:
  *   - macOS: `phys_footprint` (and `phys_footprint_peak`), via `/usr/bin/footprint`
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

  /** `/usr/bin/footprint -p <pid>` prints both current and peak:
    * {{{
    * phys_footprint: 542 MB
    * phys_footprint_peak: 1645 MB
    * }}}
    * ~40ms per call, and it accepts several `-p` at once, so a whole fleet costs one process spawn. `vmmap --summary` reports the same number but walks the
    * entire address space and takes ~800ms, which is too slow to do repeatedly.
    */
  object MacOs extends ProcessMemory {
    def footprintMb(pid: Long): Option[Long] = read(pid, "phys_footprint")
    def peakFootprintMb(pid: Long): Option[Long] = read(pid, "phys_footprint_peak")

    private def read(pid: Long, field: String): Option[Long] =
      runFootprint(pid).flatMap { out =>
        out.linesIterator
          .map(_.trim)
          .collectFirst { case l if l.startsWith(s"$field:") => l }
          .flatMap(parseSize)
      }

    private def runFootprint(pid: Long): Option[String] =
      try {
        val p = new ProcessBuilder("/usr/bin/footprint", "-p", pid.toString).redirectErrorStream(false).start()
        val out = new String(p.getInputStream.readAllBytes(), "UTF-8")
        // A dead pid exits non-zero; that is an expected race (the fork finished between listing and
        // measuring), not an error worth surfacing.
        if (p.waitFor() == 0) Some(out) else None
      } catch { case _: java.io.IOException => None }

    /** `phys_footprint: 542 MB` / `... 1.6 GB` / `... 813 KB` */
    private[bleep] def parseSize(line: String): Option[Long] = {
      val value = line.dropWhile(_ != ':').drop(1).trim
      val (num, unit) = value.span(c => c.isDigit || c == '.')
      num.trim.toDoubleOption.map { n =>
        unit.trim.toUpperCase match {
          case u if u.startsWith("GB") || u == "G"     => math.round(n * 1024)
          case u if u.startsWith("KB") || u == "K"     => math.round(n / 1024)
          case u if u.startsWith("B") && u.length == 1 => math.round(n / (1024 * 1024))
          case _                                       => math.round(n) // MB, the usual case
        }
      }
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
}
