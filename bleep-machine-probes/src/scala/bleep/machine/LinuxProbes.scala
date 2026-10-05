package bleep.machine

import java.io.IOException
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, NoSuchFileException, Path}

/** The Linux machine probe: plain reads of `/proc` and `/sys/fs/cgroup`, so it works on every JDK with no native code.
  *
  * '''Containers.''' Host-wide `/proc/meminfo` cannot see a cgroup memory limit, and in a container with one, that limit — not the host's RAM — is what the OOM
  * killer enforces. So when this process is in a cgroup v2 hierarchy whose tightest `memory.max` (its own cgroup's or an ancestor's) is below the host's
  * memory, that cgroup is the machine: physical is its `memory.max`, used is its `memory.current` minus the file cache it could drop (`inactive_file` from
  * `memory.stat`, the same subtraction `docker stats` and the kubelet's working set make), and pressure is its own `memory.pressure`. Otherwise — no limit
  * (`max`), a limit above the host's memory, or a cgroup v1 memory controller — the host's `/proc` numbers apply. cgroup v1 is deliberately not read: its
  * accounting files differ and it is on its way out, so a v1 container is measured as the host it runs on.
  *
  * Which cgroups to look at is resolved once, from `/proc/self/cgroup`, when the probe is made; their limits are read on every sample, since `memory.max` can
  * be changed at runtime.
  *
  * @param procRoot
  *   `/proc` on a real machine; tests point it at fixtures
  * @param cgroupRoot
  *   `/sys/fs/cgroup` on a real machine; tests point it at fixtures
  */
final class LinuxMachineProbe(procRoot: Path, cgroupRoot: Path) extends MachineProbe {
  private val meminfo = procRoot.resolve("meminfo")
  private val psi = procRoot.resolve("pressure").resolve("memory")
  private val cgroups: List[Path] = LinuxCgroup.memoryCgroups(LinuxProc.read(procRoot.resolve("self").resolve("cgroup")), cgroupRoot)

  def sample(): MachineSample = {
    val mem = LinuxProc.parseMeminfo(LinuxProc.read(meminfo))
    LinuxCgroup.tightestLimit(cgroups, mem.totalKb * 1024) match {
      case Some(LinuxCgroup.Limit(dir, maxBytes)) =>
        val currentFile = dir.resolve("memory.current")
        val statFile = dir.resolve("memory.stat")
        val current = LinuxCgroup.parseBytes(LinuxProc.read(currentFile), currentFile)
        val inactiveFile = LinuxCgroup.statField(LinuxProc.read(statFile), "inactive_file", statFile)
        val workingSet = math.max(0L, current - inactiveFile)
        MachineSample(
          physicalMb = maxBytes / LinuxCgroup.MB,
          usedMb = workingSet / LinuxCgroup.MB,
          availableMb = math.max(0L, maxBytes - workingSet) / LinuxCgroup.MB,
          roomBasis = RoomBasis.AvailableMemory,
          pressure = LinuxProc.readPsi(dir.resolve("memory.pressure"))
        )
      case None =>
        MachineSample(
          physicalMb = mem.totalKb / 1024,
          usedMb = (mem.totalKb - mem.availableKb) / 1024,
          availableMb = mem.availableKb / 1024,
          roomBasis = RoomBasis.AvailableMemory,
          pressure = LinuxProc.readPsi(psi)
        )
    }
  }
}

object LinuxCgroup {
  final val MB = 1024L * 1024L

  /** The cgroup whose limit binds, and that limit in bytes. */
  case class Limit(dir: Path, maxBytes: Long)

  /** The cgroup v2 directories whose memory limits apply to this process — its own cgroup first, then each ancestor up to and including `cgroupRoot` — or none
    * when cgroup v2 does not govern memory here.
    *
    * `/proc/self/cgroup` has one `hierarchy:controllers:path` line per hierarchy. A line naming the `memory` controller means cgroup v1 governs memory (a v1 or
    * hybrid host). Otherwise the `0::<path>` line is this process's v2 cgroup, relative to the v2 mount — which is `cgroupRoot` only if that holds a
    * `cgroup.controllers` listing `memory`. Inside a container with its own cgroup namespace the path is `/` and `cgroupRoot` is the container's cgroup.
    */
  def memoryCgroups(selfCgroup: String, cgroupRoot: Path): List[Path] = {
    val lines = selfCgroup.linesIterator.filter(_.nonEmpty).map(_.split(":", 3)).toList
    val v1Memory = lines.exists(parts => parts.length == 3 && parts(1).split(',').contains("memory"))
    val v2Path = lines.collectFirst { case Array("0", "", path) => path }
    val controllers = cgroupRoot.resolve("cgroup.controllers")
    val v2MemoryMounted = Files.isRegularFile(controllers) && LinuxProc.read(controllers).trim.split("\\s+").contains("memory")
    (v1Memory, v2Path, v2MemoryMounted) match {
      case (false, Some(path), true) =>
        val leaf = path.split('/').filter(_.nonEmpty).foldLeft(cgroupRoot)(_.resolve(_))
        if (!Files.isDirectory(leaf))
          throw new IllegalStateException(
            s"/proc/self/cgroup puts this process in cgroup $path, but $leaf does not exist: $cgroupRoot is not the cgroup v2 hierarchy this process is in"
          )
        Iterator.iterate(leaf)(_.getParent).takeWhile(_.startsWith(cgroupRoot)).toList
      case _ => Nil
    }
  }

  /** The tightest `memory.max` among `cgroups` that is below the host's memory, if any. The root cgroup has no `memory.max`; `max` means no limit. */
  def tightestLimit(cgroups: List[Path], hostBytes: Long): Option[Limit] =
    cgroups
      .flatMap { dir =>
        val file = dir.resolve("memory.max")
        if (!Files.exists(file)) None
        else {
          val content = LinuxProc.read(file)
          if (content.trim == "max") None else Some(Limit(dir, parseBytes(content, file)))
        }
      }
      .filter(_.maxBytes < hostBytes)
      .minByOption(_.maxBytes)

  def parseBytes(content: String, file: Path): Long =
    content.trim.toLongOption.getOrElse(throw new IllegalStateException(s"$file does not hold a byte count: '${content.trim}'"))

  /** A `name value` line of `memory.stat`, in bytes. */
  def statField(content: String, name: String, file: Path): Long =
    content.linesIterator
      .collectFirst { case line if line.startsWith(name + " ") => line.substring(name.length + 1).trim }
      .flatMap(_.toLongOption)
      .getOrElse(throw new IllegalStateException(s"$file has no `$name` line. Content:\n$content"))
}

/** What a fork costs, from `/proc/<pid>/status`: its `RssAnon` plus its `VmSwap`.
  *
  * Chosen to mean what `phys_footprint` means on macOS and private bytes on Windows — memory that belongs to the process and has nowhere to go but RAM or swap:
  *   - '''not `VmRSS` or `Pss`''': both count file-backed pages — the JDK's own `lib/modules`, every mapped jar on the classpath. Those are clean and
  *     disk-backed, the kernel drops them for free, and `MemAvailable` already counts them as available, so charging them to the fork would count reclaimable
  *     cache twice. Measured on forked test JVMs, mapped classpath was most of their RSS.
  *   - '''`RssAnon`''' is the heap, metaspace, thread stacks, code cache and malloc'd memory that is resident now. A forked JVM is `exec`ed, not a
  *     copy-on-write child, so none of it is shared with another process and no proportional (`Pss_Anon`) correction is needed.
  *   - '''`VmSwap`''' is anonymous memory the kernel has already pushed out. It is still the process's cost — macOS counts compressed pages in `phys_footprint`
  *     and Windows counts paged-out private memory in private bytes — and a fork that has been swapped out must not look cheap to the scheduler.
  *
  * '''Not `smaps_rollup`''', although its `Anonymous + Swap` is the same quantity: the kernel computes `smaps_rollup` by walking the process's page tables, so
  * a read costs time in proportion to the process's size — measured at 1.7 ms for a 300 MB JVM on a GitHub runner, and a multi-GB test fork is several times
  * that. `status` prints the memory manager's running counters, so a read costs the same whatever the size of the process. Both fields exist since Linux 4.5.
  */
final class LinuxForkProbe(procRoot: Path) extends ForkProbe {
  def footprintMb(pid: Long): Option[Long] = {
    val status = procRoot.resolve(pid.toString).resolve("status")
    try LinuxProc.parseStatusFootprintKb(LinuxProc.read(status)).map(_ / 1024)
    catch {
      case _: NoSuchFileException => None
      // The process was reaped between opening the file and reading it: the read fails with ESRCH, which Java reports as a bare IOException. Whether
      // `/proc/<pid>` is still there tells that apart from a genuine failure.
      case e: IOException =>
        if (LinuxProc.isGone(procRoot, pid)) None
        else throw new IOException(s"Could not read $status for live process $pid", e)
    }
  }
}

object LinuxProc {
  case class Meminfo(totalKb: Long, availableKb: Long)

  def read(path: Path): String = new String(Files.readAllBytes(path), StandardCharsets.US_ASCII)

  /** The value of the `name:` line in a `/proc` file of `key: value` lines, trimmed; `None` if there is no such line. A direct search rather than a parse of
    * the whole file: `status` has some 60 lines and two are wanted, on a path the scheduler runs often.
    */
  def lineValue(content: String, name: String): Option[String] = {
    val key = name + ":"
    val at =
      if (content.startsWith(key)) 0
      else {
        val i = content.indexOf("\n" + key)
        if (i < 0) -1 else i + 1
      }
    if (at < 0) None
    else {
      val from = at + key.length
      val nl = content.indexOf('\n', from)
      Some(content.substring(from, if (nl < 0) content.length else nl).trim)
    }
  }

  /** A `name:   1234 kB` line, as kB. Missing or malformed throws, quoting the file. */
  def kbField(content: String, name: String, file: String): Long =
    lineValue(content, name)
      .filter(_.endsWith(" kB"))
      .flatMap(_.stripSuffix(" kB").trim.toLongOption)
      .getOrElse(throw new IllegalStateException(s"$file has no `$name: <n> kB` line. Content:\n$content"))

  /** `MemAvailable` is the kernel's own estimate of what a new allocation can get without swapping; it exists since Linux 3.14. */
  def parseMeminfo(content: String): Meminfo =
    Meminfo(kbField(content, "MemTotal", "/proc/meminfo"), kbField(content, "MemAvailable", "/proc/meminfo"))

  /** `RssAnon + VmSwap` from `/proc/<pid>/status`, or `None` for a zombie (`Z`) or dead (`X`) task: it has exited and its memory is gone, though it has not
    * been reaped yet. A zombie's `status` has no memory lines at all.
    */
  def parseStatusFootprintKb(content: String): Option[Long] = {
    val state = lineValue(content, "State").getOrElse(throw new IllegalStateException(s"/proc/<pid>/status has no `State:` line. Content:\n$content"))
    if (state.startsWith("Z") || state.startsWith("X")) None
    else Some(kbField(content, "RssAnon", "/proc/<pid>/status") + kbField(content, "VmSwap", "/proc/<pid>/status"))
  }

  /** `/proc/pressure/memory`:
    * {{{
    * some avg10=0.00 avg60=0.00 avg300=0.00 total=0
    * full avg10=0.00 avg60=0.00 avg300=0.00 total=0
    * }}}
    */
  def parsePsi(content: String): RawPressure.LinuxPsi = {
    def avg10(kind: String): Double =
      content.linesIterator
        .find(_.startsWith(kind + " "))
        .flatMap(_.split(' ').collectFirst { case kv if kv.startsWith("avg10=") => kv.stripPrefix("avg10=") })
        .flatMap(_.toDoubleOption)
        .getOrElse(throw new IllegalStateException(s"/proc/pressure/memory has no `$kind avg10=` value. Content:\n$content"))
    RawPressure.LinuxPsi(someAvg10 = avg10("some"), fullAvg10 = avg10("full"))
  }

  /** PSI from `path`, or [[RawPressure.Unavailable]] where the kernel has none.
    *
    * A kernel built without `CONFIG_PSI` has no such file; one built with `CONFIG_PSI_DEFAULT_DISABLED` (RHEL 8, for one), or booted with `psi=0`, has the file
    * but refuses to read it with EOPNOTSUPP, which Java reports only as an IOException whose message is the errno text. Neither is a failure: the scheduler
    * still has used memory against the ceiling and only loses its pressure brake. Any other read failure is a failure, and throws.
    */
  def readPsi(path: Path): RawPressure = {
    def unavailable(why: String): RawPressure.Unavailable =
      RawPressure.Unavailable(
        s"$path $why: this Linux kernel reports no pressure stall information (PSI). It needs CONFIG_PSI, and a kernel built with " +
          "CONFIG_PSI_DEFAULT_DISABLED must be booted with `psi=1`."
      )
    try parsePsi(read(path))
    catch {
      case _: NoSuchFileException                                      => unavailable("does not exist")
      case e: IOException if e.getMessage == "Operation not supported" => unavailable("cannot be read (PSI is disabled)")
    }
  }

  /** Whether `pid` has exited, once a read of one of its files has failed: there is no `/proc/<pid>` any more. */
  def isGone(procRoot: Path, pid: Long): Boolean = !Files.exists(procRoot.resolve(pid.toString))
}
