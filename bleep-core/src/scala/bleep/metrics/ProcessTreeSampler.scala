package bleep.metrics

import java.io.{BufferedReader, InputStreamReader}
import java.nio.file.{Files, Path}
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicLong
import scala.collection.mutable
import scala.jdk.CollectionConverters.*
import scala.jdk.OptionConverters.*
import scala.util.control.NonFatal

/** Observes the OS process tree rooted at the current process. Emits one `subprocess_start` event the first time a descendant PID is seen, updates the peak RSS
  * for each known PID on every `sample()`, and emits one `subprocess_end` when a PID disappears from the descendant set.
  *
  * Designed to be driven by a single-thread sampler at coarse (~5s) cadence. The `writeEvent` callback is the only side-effect — typically wired to
  * `BspMetrics.writeEvent` or `InvocationMetrics.writeEvent`. The class owns its registry; sample() must not be called concurrently from multiple threads.
  *
  * Memory readings:
  *   - **Linux**: reads `/proc/<pid>/status` `VmHWM:` line. The kernel tracks high-water-mark monotonically; sampling cadence doesn't affect peak accuracy even
  *     for short-lived processes (as long as we sample at least once during their life).
  *   - **macOS**: one batched `ps -o pid=,rss= -p p1 -p p2 …` per sample. RSS is current (not peak); we keep our own running max across samples. Spikes between
  *     samples may be missed, but for processes alive >5s the approximation is solid.
  *   - **Windows**: skipped (no `/proc`, JDK doesn't expose). Subprocesses still appear in start/end events with `peak_rss_mb: 0` so the tree is visible.
  *
  * Non-bleep / non-JVM descendants (`native-image`, `cc`, `tar`, `node`) get tracked the same way as JVMs — we don't need cooperation from the child.
  */
final class ProcessTreeSampler(writeEvent: String => Unit) {
  import ProcessTreeSampler.*

  private val registry = new ConcurrentHashMap[Long, Track]()

  /** Walk descendants once, emit start/end events, update peaks. Idempotent across calls. */
  def sample(): Unit = {
    val nowMs = System.currentTimeMillis()
    val seen = new java.util.HashMap[Long, ProcessHandle]()
    ProcessHandle.current().descendants().forEach(ph => seen.put(ph.pid, ph))

    // Emit `subprocess_start` for any PID we haven't seen before.
    seen.forEach { (pid, ph) =>
      if (!registry.containsKey(pid)) {
        val ppid = ph.parent.toScala.map(_.pid).getOrElse(-1L)
        val info = ph.info
        // `info.commandLine` inlines the classpath verbatim (multi-KB for JVM children) — useless and noisy. Instead reconstruct from `info.command` +
        // `info.arguments` and strip the classpath flag-pairs. Truncate to 500 chars so a pathological main-class argv still doesn't dominate the JSONL.
        val rawCmd = info.command.toScala.orElse(info.commandLine.toScala.map(_.takeWhile(_ != ' '))).getOrElse("")
        val argList = info.arguments.toScala.map(_.toList).getOrElse(Nil)
        val keptArgs = stripClasspath(argList)
        val full = if (keptArgs.isEmpty) rawCmd else s"$rawCmd ${keptArgs.mkString(" ")}"
        val cmd = if (full.length > 500) full.take(497) + "..." else full
        val startMs = info.startInstant.toScala.map(_.toEpochMilli).getOrElse(nowMs)
        registry.put(pid, new Track(pid, ppid, cmd, startMs))
        writeEvent(
          s"""{"type":"subprocess_start","ts":$nowMs,"pid":$pid,"ppid":$ppid,"start_time":$startMs,"cmd":"${esc(cmd)}","kind":"observed"}"""
        )
      }
    }

    // Update peak RSS for everyone currently visible. Linux: one /proc read per PID (cheap). macOS: one batched `ps`. Windows: no-op.
    val rssByPid = readRssBatch(seen.values.iterator.asScala.map(_.pid).toList)
    rssByPid.foreachEntry { (pid, kb) =>
      val t = registry.get(pid)
      if (t != null) t.peakRssKb.updateAndGet(prev => math.max(prev, kb))
    }

    // Emit `subprocess_end` for PIDs that disappeared since the last sample. The exit code is unknown from outside (we're not waitpid-ing), so just omit.
    val toRemove = mutable.ArrayBuffer.empty[Long]
    registry.forEach { (pid, t) =>
      if (!seen.containsKey(pid)) {
        val durationMs = nowMs - t.startMs
        val peakMb = t.peakRssKb.get / 1024
        writeEvent(s"""{"type":"subprocess_end","ts":$nowMs,"pid":$pid,"peak_rss_mb":$peakMb,"duration_ms":$durationMs}""")
        toRemove += pid
      }
    }
    toRemove.foreach(registry.remove)
  }

  /** Final shutdown — emit `subprocess_end` for everyone in the registry, so a clean process exit doesn't leave dangling spans in the JSONL. */
  def flushOnShutdown(): Unit = {
    val nowMs = System.currentTimeMillis()
    registry.forEach { (pid, t) =>
      val durationMs = nowMs - t.startMs
      val peakMb = t.peakRssKb.get / 1024
      writeEvent(s"""{"type":"subprocess_end","ts":$nowMs,"pid":$pid,"peak_rss_mb":$peakMb,"duration_ms":$durationMs,"reason":"sampler_shutdown"}""")
    }
    registry.clear()
  }

  private def readRssBatch(pids: List[Long]): Map[Long, Long] =
    if (pids.isEmpty) Map.empty
    else if (isLinux) readRssFromProc(pids)
    else if (isMacOs) readRssFromPs(pids)
    else Map.empty
}

object ProcessTreeSampler {

  private final class Track(val pid: Long, val ppid: Long, val cmd: String, val startMs: Long) {
    val peakRssKb: AtomicLong = new AtomicLong(0)
  }

  private lazy val isLinux: Boolean = System.getProperty("os.name", "").toLowerCase.contains("linux")
  private lazy val isMacOs: Boolean = System.getProperty("os.name", "").toLowerCase.contains("mac")

  /** Linux path: read each `/proc/<pid>/status` and pull the `VmHWM:` line (monotonic high-water-mark in KB). Catches the all-time peak even if we sample late
    * in the process's life. Costs one file read per PID, no fork.
    */
  private def readRssFromProc(pids: List[Long]): Map[Long, Long] = {
    val b = Map.newBuilder[Long, Long]
    pids.foreach { pid =>
      try {
        val p = Path.of(s"/proc/$pid/status")
        if (Files.isReadable(p)) {
          // status is short; readString is fine. We could line-scan for an early-exit but the win is negligible.
          val content = Files.readString(p)
          val idx = content.indexOf("VmHWM:")
          if (idx >= 0) {
            val eol = content.indexOf('\n', idx)
            val line = if (eol > 0) content.substring(idx + 6, eol) else content.substring(idx + 6)
            val kb = line.trim.split("\\s+").headOption.flatMap(_.toLongOption).getOrElse(0L)
            if (kb > 0) b += (pid -> kb)
          }
        }
      } catch { case NonFatal(_) => () /* race: pid exited between descendants() and now */ }
    }
    b.result()
  }

  /** macOS path: one `ps -o pid=,rss= -p p1 -p p2 …` per sample. Output: lines of `PID RSS-KB`. No peak from the kernel; we maintain it ourselves in
    * `Track.peakRssKb`. One fork per sample regardless of subprocess count.
    */
  private def readRssFromPs(pids: List[Long]): Map[Long, Long] = {
    if (pids.isEmpty) return Map.empty
    val cmd = "ps" :: "-o" :: "pid=,rss=" :: pids.flatMap(p => List("-p", p.toString))
    try {
      val pb = new ProcessBuilder(cmd*).redirectErrorStream(true)
      val proc = pb.start()
      val r = new BufferedReader(new InputStreamReader(proc.getInputStream))
      val b = Map.newBuilder[Long, Long]
      var line = r.readLine()
      while (line != null) {
        val parts = line.trim.split("\\s+")
        if (parts.length >= 2) {
          for {
            pid <- parts(0).toLongOption
            rss <- parts(1).toLongOption
          } b += (pid -> rss)
        }
        line = r.readLine()
      }
      proc.waitFor()
      b.result()
    } catch { case NonFatal(_) => Map.empty }
  }

  /** Minimal JSON escape — same shape `BspMetrics.esc` uses, kept duplicated here so bleep-core has no dep on bleep-bsp. */
  private def esc(s: String): String =
    s.replace("\\", "\\\\").replace("\"", "\\\"").replace("\n", "\\n").replace("\r", "\\r").replace("\t", "\\t")

  /** Drop classpath-style flag pairs from a JVM argv. We strip these because the values are routinely several KB of jar paths — useless for identifying the
    * process and ruinous for JSONL line size. The flags removed are:
    *   - `-classpath` / `-cp` / `--class-path` / `--classpath` — the classic JVM classpath flag and its long forms
    *   - `--module-path` / `-p` — the JPMS module path, same multi-KB problem
    *   - `-Xbootclasspath/a:` etc. (joined flag, no separate value) — handled by dropping the single token outright
    *
    * Everything else (`-Xmx`, `-Dfoo=bar`, `--add-opens`, main class, program args) is kept verbatim.
    */
  def stripClasspath(args: List[String]): List[String] = {
    val pairFlags = Set("-classpath", "-cp", "--class-path", "--classpath", "--module-path", "-p")
    def loop(remaining: List[String], acc: List[String]): List[String] = remaining match {
      case head :: _ :: rest if pairFlags.contains(head)      => loop(rest, acc)
      case head :: rest if head.startsWith("-Xbootclasspath") => loop(rest, acc)
      case head :: rest if head.startsWith("--patch-module")  => loop(rest, acc)
      case head :: rest                                       => loop(rest, head :: acc)
      case Nil                                                => acc.reverse
    }
    loop(args, Nil)
  }
}
