package bleep.metrics

import java.io.{BufferedWriter, FileWriter}
import java.nio.file.{Files, Path}
import java.time.{Instant, ZoneOffset}
import java.time.format.DateTimeFormatter

/** Per-invocation metrics for the bleep CLI process. Counterpart to `BspMetrics` but scoped to one `bleep …` run, not the long-running BSP server daemon.
  *
  * Each invocation creates a fresh directory under `BuildPaths.metricsRoot`:
  *
  * {{{
  *   <workspace>/.bleep/metrics/<utc-timestamp>-<pid>/
  *     ├── metrics.jsonl      # this invocation's events
  *     └── pid                # this CLI process's pid + start_time (lets later tools tie subprocesses to the invocation)
  * }}}
  *
  * Hosts a `ProcessTreeSampler` so every subprocess the CLI forks (script JVMs, `native-image`, etc.) shows up as `subprocess_start`/`subprocess_end` events
  * with their peak RSS. No coordination with children needed; the observer reads the OS-maintained descendant tree.
  *
  * Independent of `BspMetrics`: when the CLI talks to a BSP daemon over the socket, the daemon's events go to *its* own scope (`<bspSocketDir>/<hash>/`). The
  * dashboard joins the two scopes at read time. Keep them write-disjoint here so OOM resilience, file ownership and lifecycle stay simple.
  */
object InvocationMetrics {

  @volatile private var writer: BufferedWriter = scala.compiletime.uninitialized
  @volatile private var samplerThread: Thread = scala.compiletime.uninitialized
  @volatile private var processTree: ProcessTreeSampler = scala.compiletime.uninitialized
  @volatile private var invocationDir: Path = scala.compiletime.uninitialized
  @volatile private var startMs: Long = 0L

  private val SampleIntervalMs = 5000L

  /** Create the per-invocation directory and start the sampler. Idempotent: a second call is a no-op (in case command dispatching reaches us twice). */
  def initialize(metricsRoot: Path): Unit = synchronized {
    if (writer != null) return // already running
    val ts = DateTimeFormatter.ofPattern("yyyyMMdd'T'HHmmss").withZone(ZoneOffset.UTC).format(Instant.now())
    val pid = ProcessHandle.current.pid
    invocationDir = metricsRoot.resolve(s"$ts-$pid")
    Files.createDirectories(invocationDir)
    startMs = System.currentTimeMillis()

    val metricsPath = invocationDir.resolve("metrics.jsonl")
    writer = new BufferedWriter(new FileWriter(metricsPath.toFile, true))

    // PID file in the same format as bleep-bsp uses (pid on first line, start_time on second) so a future cross-process tool can correlate descendants
    // back to the owning invocation by walking the OS PID chain. Not consumed today; here so the contract is stable.
    val pidFile = invocationDir.resolve("pid")
    Files.writeString(pidFile, s"$pid\n$startMs\n")

    writeEvent(
      s"""{"type":"process","ts":${now()},"pid":$pid,"start_time":$startMs,"kind":"bleep-cli","cmd":"${esc(processCmd())}"}"""
    )

    processTree = new ProcessTreeSampler(writeEvent)

    val t = new Thread("invocation-metrics-sampler") {
      override def run(): Unit =
        try
          while (!Thread.currentThread.isInterrupted) {
            try processTree.sample()
            catch { case _: Throwable => () /* never let the sampler die on a bad probe */ }
            Thread.sleep(SampleIntervalMs)
          }
        catch { case _: InterruptedException => () }
    }
    t.setDaemon(true)
    t.start()
    samplerThread = t
  }

  /** Stop the sampler, drain the registry one last time, close the file. Safe to call multiple times. Called from a JVM shutdown hook in bleep-cli Main. */
  def shutdown(): Unit = synchronized {
    val t = samplerThread
    if (t != null) {
      t.interrupt()
      t.join(2000)
      samplerThread = null
    }
    val tree = processTree
    if (tree != null) {
      tree.flushOnShutdown()
      processTree = null
    }
    val w = writer
    if (w != null) {
      writeEvent(s"""{"type":"invocation_end","ts":${now()},"duration_ms":${now() - startMs}}""")
      w.synchronized {
        w.flush()
        w.close()
      }
      writer = null
    }
  }

  /** Path to this invocation's metrics directory, if initialized. */
  def dir: Option[Path] = Option(invocationDir)

  // --------------- internals ---------------

  private def writeEvent(json: String): Unit = {
    val w = writer
    if (w != null) {
      try
        w.synchronized {
          w.write(json)
          w.newLine()
          w.flush()
        }
      catch { case _: Throwable => () /* if the metrics writer fails mid-event we don't want it to kill the host CLI */ }
    }
  }

  private def now(): Long = System.currentTimeMillis()

  private def processCmd(): String =
    ProcessHandle.current.info.commandLine
      .map[String](identity)
      .orElse(ProcessHandle.current.info.command.orElse(""))

  private def esc(s: String): String =
    s.replace("\\", "\\\\").replace("\"", "\\\"").replace("\n", "\\n").replace("\r", "\\r").replace("\t", "\\t")
}
