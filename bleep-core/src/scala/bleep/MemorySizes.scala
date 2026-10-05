package bleep

/** Memory sizes bleep states, parses and derives: the heap a fork is started with, what a bound fork costs the machine, and the machine's own RAM.
  *
  * Pure arithmetic and parsing, shared by the client (`bleep server top`, the daemon launcher), the compile server and the scheduler. Nothing here decides
  * anything; it only keeps the two halves of every deal — what we tell a JVM it may use and what we charge for it — derived from one number.
  */
object MemorySizes {

  /** Parse a heap-size string (`"512m"`, `"2g"`, `"1500m"`, optionally `-Xmx`-prefixed) into MB.
    *
    * `None` means "no size stated here". It does NOT mean "unparseable": a string that looks like a size but isn't one we understand throws, because silently
    * mis-weighting a fork is exactly how a memory bound stops bounding anything. An unknown suffix is the dangerous case — reading `2t` as 2MB under-counts a
    * fork a millionfold, and a scheduler would happily admit hundreds of them.
    */
  def parseMemoryMb(raw0: String): Option[Long] = {
    val raw = raw0.trim.stripPrefix("-Xmx").trim
    if (raw.isEmpty) None
    else {
      val (num, unit) = raw.span(_.isDigit)
      if (num.isEmpty) throw new IllegalArgumentException(s"cannot parse memory size '$raw0': expected digits, optionally suffixed with k/m/g")
      val n =
        try num.toLong
        catch { case _: NumberFormatException => throw new IllegalArgumentException(s"cannot parse memory size '$raw0': '$num' is not a valid number") }
      Some(unit.trim.toLowerCase match {
        case "g" | "gb" => n * 1024L
        case "m" | "mb" => n
        case "k" | "kb" => math.max(1L, n / 1024L)
        case ""         => math.max(1L, n / (1024L * 1024L)) // bare number is bytes, per -Xmx semantics
        case other      => throw new IllegalArgumentException(s"cannot parse memory size '$raw0': unknown unit '$other' (expected k/m/g)")
      })
    }
  }

  /** Total physical RAM in MB, as the JDK reports it.
    *
    * `fallbackMb` covers the one genuinely-expected miss: a JVM whose OperatingSystemMXBean isn't the `com.sun` one that exposes total memory. An *exception*
    * from the platform bean is not that case and is not swallowed — it would mean a figure is being computed from a number we never actually read. The compile
    * server's scheduler does not use this: it reads the machine through its probes, which also see a container's limit.
    */
  def physicalMemoryMb(fallbackMb: Long): Long =
    java.lang.management.ManagementFactory.getOperatingSystemMXBean match {
      case os: com.sun.management.OperatingSystemMXBean => os.getTotalMemorySize / (1024L * 1024L)
      case _                                            => fallbackMb
    }

  /** Heap ceiling imposed on any forked JVM whose build states none — test runners, sourcegen scripts, KSP.
    *
    * A fork with no `-Xmx` is not "unlimited": HotSpot silently gives it `MaxRAMPercentage=25`, a quarter of the machine. That default assumes it is the only
    * thing running, which is exactly wrong for a build tool that starts one per core — on an 18-core / 48GB machine bleep was requesting 18 × 12GB. OOM was
    * arithmetic, not bad luck, and scheduling cannot fix it, because a scheduler gets no say in what a process allocates.
    *
    * So bleep states a bound rather than inheriting one. Every comparable tool does: Gradle defaults `Test.maxHeapSize` to 512m, Maven Surefire runs a single
    * fork, sbt runs tests in-process. 2GB is comfortable for ordinary JVM test suites and small enough that CPU, not memory, decides how wide a build runs.
    *
    * A fork that genuinely needs more says so — `testRunnerHeap` / `sourcegenMaxMemory` / `kspRunnerMaxMemory`, or the project's own `jvmOptions` — and if it
    * then exceeds that, it gets an `OutOfMemoryError` naming the limit: attributable to the code that caused it, instead of a SIGKILL landing on whichever
    * process the kernel happened to pick.
    */
  val DefaultForkHeapMb: Long = 2048L

  /** The heap a fork will actually run with: what the build configured, else [[DefaultForkHeapMb]]. The single source of truth for both halves of the deal —
    * what we tell the JVM it may use, and what we tell the scheduler it costs. Deriving those separately is how they drift apart.
    */
  def forkHeapMb(configured: Option[String]): Long =
    configured.flatMap(parseMemoryMb).getOrElse(DefaultForkHeapMb)

  /** The options a fork is actually started with: whatever the build asked for, plus `defaultHeapMb` if it stated no `-Xmx`.
    *
    * A fork ends up with exactly one `-Xmx`, which is the point. Configured heaps used to be prepended to the build's own options and left to JVM last-one-wins
    * to resolve, so `java -Xmx1g … -Xmx3g` was a normal argv and the number a fork ran with could not be read off either source alone. It also made the config
    * knob look like a ceiling while behaving as a default. Now the choice is made here, once, and the argv states the answer.
    *
    * Returned rather than applied in place because for pooled forks these options are also the pool key — a JVM started with an imposed bound must not be
    * handed to a caller who asked for a different one.
    */
  def withHeapBound(jvmOptions: List[String], defaultHeapMb: Long): List[String] =
    if (jvmOptions.exists(_.startsWith("-Xmx"))) jvmOptions
    else jvmOptions :+ s"-Xmx${defaultHeapMb}m"

  /** The `-Xmx` among `jvmOptions`, the last one if several, in MB; `None` when none is stated. */
  def xmxMb(jvmOptions: List[String]): Option[Long] =
    jvmOptions.reverse.collectFirst { case o if o.startsWith("-Xmx") => o }.flatMap(parseMemoryMb)

  /** What a forked JVM whose heap is capped at `heapMb` costs the machine: its footprint ceiling, the `boundMb` the scheduler charges it until it is measured.
    *
    * `-Xmx` bounds the heap, not the process: on top of it a JVM commits metaspace, the code cache, a stack per thread, direct/mapped byte buffers and the GC's
    * own bookkeeping.
    *
    * This is honest accounting of a bound we already know — NOT a safety mechanism. Containment comes from the `-Xmx` every fork now carries (see
    * [[DefaultForkHeapMb]]). It used to be load-bearing, back when forks ran unbounded and this multiplier was the only thing between a fork storm and the OOM
    * killer, which was always the wrong job for an estimate: an estimate cannot stop a process from allocating.
    */
  def forkFootprintMb(heapMb: Long): Long =
    heapMb + math.max(256L, heapMb / 4)
}
