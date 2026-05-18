package bleep.bsp

import bleep.metrics.ProcessTreeSampler

import java.io.{BufferedWriter, FileWriter}
import java.lang.management.ManagementFactory
import java.nio.file.{Files, Path}
import java.util.Locale
import java.util.concurrent.atomic.{AtomicInteger, AtomicLong}
import scala.jdk.CollectionConverters._

/** Lightweight metrics collector for the BSP server.
  *
  * Writes append-only JSONL to `<metricsDir>/metrics.jsonl`. Crash-safe: flushes after every write. Intended to always be on — the overhead is negligible (one
  * sync write per event + one background sample every few seconds).
  */
object BspMetrics {

  @volatile private var writer: BufferedWriter = scala.compiletime.uninitialized
  @volatile private var samplerThread: Thread = scala.compiletime.uninitialized
  @volatile private var metricsPath: Path = scala.compiletime.uninitialized
  @volatile private var processTree: ProcessTreeSampler = scala.compiletime.uninitialized
  @volatile private var processTreeSampleErrorReported: Boolean = false

  // High watermarks
  private val maxConcurrentCompiles = AtomicInteger(0)
  private val maxActiveConnections = AtomicInteger(0)
  private val maxHeapUsedBytes = AtomicLong(0)

  // Counters
  private val concurrentCompiles = AtomicInteger(0)
  private val activeConnections = AtomicInteger(0)

  // OOM tracking
  private val OomThreshold = 0.95
  @volatile private var oomPressureStarted = false

  private val SampleIntervalMs = 5000L

  /** Maximum metrics file size before rotation (100MB) */
  private val MaxMetricsFileBytes = 100L * 1024 * 1024

  def initialize(metricsDir: Path): Unit = {
    metricsPath = metricsDir.resolve("metrics.jsonl")
    rotateIfNeeded()
    writer = new BufferedWriter(new FileWriter(metricsPath.toFile, true)) // append mode

    // Install OOM crash handler — records event before JVM dies
    val previousHandler = Thread.getDefaultUncaughtExceptionHandler
    Thread.setDefaultUncaughtExceptionHandler { (thread: Thread, throwable: Throwable) =>
      throwable match {
        case oom: OutOfMemoryError =>
          recordOomCrash(thread.getName, oom.getMessage)
        case _ => ()
      }
      if (previousHandler != null) previousHandler.uncaughtException(thread, throwable)
    }

    // Observer for every transitive descendant of this process (test runner JVMs, KSP runner, sourcegen forks, native-image-tool, LLVM linkers, …). Shares
    // the same writer + sampler thread as the JVM metrics — one extra ProcessHandle.descendants() walk + RSS read per 5s tick. See ProcessTreeSampler.
    processTree = new ProcessTreeSampler(writeEvent)

    val t = new Thread("bsp-metrics-sampler") {
      override def run(): Unit =
        try
          while (!Thread.currentThread().isInterrupted) {
            sampleJvm()
            try processTree.sample()
            catch {
              case t: Throwable =>
                // Print once to stderr but don't kill the thread — observer failures shouldn't take down BSP metrics. Once per JVM (volatile flag) so we don't
                // spam if descendants() throws on every tick.
                if (!processTreeSampleErrorReported) {
                  processTreeSampleErrorReported = true
                  System.err.println(s"[BspMetrics] ProcessTreeSampler.sample() failed (will not be reported again): ${t.getClass.getName}: ${t.getMessage}")
                }
            }
            Thread.sleep(SampleIntervalMs)
          }
        catch {
          case _: InterruptedException => () // shutdown
        }
    }
    t.setDaemon(true)
    t.start()
    samplerThread = t
  }

  def shutdown(): Unit = {
    val t = samplerThread
    if (t != null) {
      t.interrupt()
      t.join(2000)
    }
    val tree = processTree
    if (tree != null) tree.flushOnShutdown()
    writeSummary()
    val w = writer
    if (w != null) {
      w.synchronized {
        w.close()
      }
    }
  }

  /** Path to the metrics file, if initialized. */
  def path: Option[Path] = Option(metricsPath)

  // --------------- event recording ---------------

  def recordCompileStart(project: String, workspace: String): Unit = {
    val current = concurrentCompiles.incrementAndGet()
    updateMax(maxConcurrentCompiles, current)
    writeEvent(s"""{"type":"compile_start","ts":${now()},"project":"${esc(project)}","workspace":"${esc(workspace)}","concurrent":$current}""")
  }

  def recordCompileEnd(project: String, workspace: String, durationMs: Long, success: Boolean): Unit = {
    val current = concurrentCompiles.decrementAndGet()
    writeEvent(
      s"""{"type":"compile_end","ts":${now()},"project":"${esc(project)}","workspace":"${esc(
          workspace
        )}","duration_ms":$durationMs,"success":$success,"concurrent":$current}"""
    )
  }

  def recordBuildStart(workspace: String, projectCount: Int): Unit =
    writeEvent(s"""{"type":"build_start","ts":${now()},"workspace":"${esc(workspace)}","projects":$projectCount}""")

  def recordBuildEnd(workspace: String, durationMs: Long, success: Boolean): Unit =
    writeEvent(s"""{"type":"build_end","ts":${now()},"workspace":"${esc(workspace)}","duration_ms":$durationMs,"success":$success}""")

  /** Project-scoped test span. Emitted once per (project, BSP test request) pair to give the dashboard a wrapping span around the per-suite events for that
    * project. `originId` is the BSP `originId` field — the same value the bleep CLI may carry as a trace anchor (the request-id propagation in a later phase
    * threads this through to InvocationMetrics). For now it's just a correlation token.
    */
  def recordProjectTestStart(project: String, workspace: String, originId: String): Unit =
    writeEvent(
      s"""{"type":"project_test_start","ts":${now()},"project":"${esc(project)}","workspace":"${esc(workspace)}","origin_id":"${esc(originId)}"}"""
    )

  def recordProjectTestEnd(project: String, workspace: String, originId: String, durationMs: Long, success: Boolean): Unit =
    writeEvent(
      s"""{"type":"project_test_end","ts":${now()},"project":"${esc(
          project
        )}","workspace":"${esc(workspace)}","origin_id":"${esc(originId)}","duration_ms":$durationMs,"success":$success}"""
    )

  /** BSP-request boundary events. Emitted at handler entry/exit so the dashboard / Perfetto / Jaeger can show one cohesive trace per `bleep` command: a root
    * `request` span containing the compile/sourcegen/test work plus any subprocesses (test JVMs) the daemon spawned for it. `originId` is the same field BSP4j
    * propagates from the CLI; it becomes the OTLP `traceId` on the renderer side.
    */
  def recordRequestStart(originId: String, method: String, workspace: String, projects: Iterable[String]): Unit = {
    val projectsJson = projects.iterator.map(p => s""""${esc(p)}"""").mkString("[", ",", "]")
    writeEvent(
      s"""{"type":"request_start","ts":${now()},"origin_id":"${esc(originId)}","method":"${esc(method)}","workspace":"${esc(
          workspace
        )}","projects":$projectsJson}"""
    )
  }

  def recordRequestEnd(originId: String, durationMs: Long, success: Boolean): Unit =
    writeEvent(s"""{"type":"request_end","ts":${now()},"origin_id":"${esc(originId)}","duration_ms":$durationMs,"success":$success}""")

  def recordCacheEvict(cache: String, workspace: String): Unit =
    writeEvent(s"""{"type":"cache_evict","ts":${now()},"cache":"${esc(cache)}","workspace":"${esc(workspace)}"}""")

  def recordCleanCache(project: String): Unit =
    writeEvent(s"""{"type":"clean_cache","ts":${now()},"project":"${esc(project)}"}""")

  def recordConnectionOpen(connId: Int): Unit = {
    val current = activeConnections.incrementAndGet()
    updateMax(maxActiveConnections, current)
    writeEvent(s"""{"type":"connection_open","ts":${now()},"conn_id":$connId,"active_connections":$current}""")
  }

  def recordConnectionClose(connId: Int): Unit = {
    val current = activeConnections.decrementAndGet()
    writeEvent(s"""{"type":"connection_close","ts":${now()},"conn_id":$connId,"active_connections":$current}""")
  }

  def recordSourcegenStart(scriptName: String): Unit =
    writeEvent(s"""{"type":"sourcegen_start","ts":${now()},"script":"${esc(scriptName)}"}""")

  def recordSourcegenEnd(scriptName: String, durationMs: Long, success: Boolean): Unit =
    writeEvent(s"""{"type":"sourcegen_end","ts":${now()},"script":"${esc(scriptName)}","duration_ms":$durationMs,"success":$success}""")

  /** Suite-level start: emitted when a test suite begins execution in a forked JVM. Pairs with `recordSuiteEnd` for total wall-clock duration including JVM
    * acquisition.
    */
  def recordSuiteStart(project: String, workspace: String, suite: String, framework: String): Unit =
    writeEvent(
      s"""{"type":"suite_start","ts":${now()},"project":"${esc(project)}","workspace":"${esc(workspace)}","suite":"${esc(suite)}","framework":"${esc(
          framework
        )}"}"""
    )

  /** Suite-level end: pairs with `recordSuiteStart`. `durationMs` is wall-clock from suite start to the SuiteFinished event from the runner. Counts are the
    * final values reported by the runner (or reconciled from individual TestFinished events).
    */
  def recordSuiteEnd(project: String, workspace: String, suite: String, passed: Int, failed: Int, skipped: Int, durationMs: Long, success: Boolean): Unit =
    writeEvent(
      s"""{"type":"suite_end","ts":${now()},"project":"${esc(project)}","workspace":"${esc(workspace)}","suite":"${esc(
          suite
        )}","passed":$passed,"failed":$failed,"skipped":$skipped,"duration_ms":$durationMs,"success":$success}"""
    )

  /** Individual test end: emitted on each TestFinished from the runner. `durationMs` is what the test framework reports for that single test. `status` is one
    * of: "passed", "failed", "skipped", "canceled", "error", "ignored" — whatever the framework emitted.
    */
  def recordTestEnd(project: String, workspace: String, suite: String, test: String, status: String, durationMs: Long): Unit =
    writeEvent(
      s"""{"type":"test_end","ts":${now()},"project":"${esc(project)}","workspace":"${esc(workspace)}","suite":"${esc(suite)}","test":"${esc(
          test
        )}","status":"${esc(status)}","duration_ms":$durationMs}"""
    )

  def recordCompilePhase(project: String, phase: String, trackedApis: Int): Unit =
    writeEvent(
      s"""{"type":"compile_phase","ts":${now()},"project":"${esc(project)}","phase":"${esc(phase)}","tracked_apis":$trackedApis}"""
    )

  def recordHeapPressureStall(project: String, heapUsedMb: Long, heapMaxMb: Long): Unit =
    writeEvent(
      s"""{"type":"heap_pressure_stall","ts":${now()},"project":"${esc(
          project
        )}","heap_used_mb":$heapUsedMb,"heap_max_mb":$heapMaxMb,"concurrent_compiles":${concurrentCompiles.get()}}"""
    )

  def recordOomCrash(threadName: String, message: String): Unit = {
    val heap = ManagementFactory.getMemoryMXBean.getHeapMemoryUsage
    val usedMb = heap.getUsed / (1024 * 1024)
    val maxMb = heap.getMax / (1024 * 1024)
    // Use pre-allocated strings to minimize allocation during OOM
    writeEvent(
      s"""{"type":"oom_crash","ts":${now()},"thread":"${esc(threadName)}","message":"${esc(
          if (message != null) message else "null"
        )}","heap_used_mb":$usedMb,"heap_max_mb":$maxMb,"concurrent_compiles":${concurrentCompiles.get()},"active_connections":${activeConnections.get()}}"""
    )
    System.err.println(s"[BspMetrics] FATAL: OutOfMemoryError on thread $threadName: $message (heap: $usedMb/$maxMb MB)")
  }

  // --------------- JVM sampling ---------------

  private def sampleJvm(): Unit = {
    val memBean = ManagementFactory.getMemoryMXBean
    val heap = memBean.getHeapMemoryUsage
    val nonHeap = memBean.getNonHeapMemoryUsage
    val threadBean = ManagementFactory.getThreadMXBean
    val gcBeans = ManagementFactory.getGarbageCollectorMXBeans.asScala

    val heapUsed = heap.getUsed
    updateMaxLong(maxHeapUsedBytes, heapUsed)

    val heapUsedMb = heapUsed / (1024 * 1024)
    val heapCommittedMb = heap.getCommitted / (1024 * 1024)
    val heapMaxMb = heap.getMax / (1024 * 1024)
    val nonHeapUsedMb = nonHeap.getUsed / (1024 * 1024)

    val gcJson = gcBeans
      .map { gc =>
        s"""{"name":"${esc(gc.getName)}","count":${gc.getCollectionCount},"time_ms":${gc.getCollectionTime}}"""
      }
      .mkString("[", ",", "]")

    val threads = threadBean.getThreadCount
    val peakThreads = threadBean.getPeakThreadCount
    val daemonThreads = threadBean.getDaemonThreadCount

    val (cpuProcess, cpuSystem) =
      try {
        val osBean = ManagementFactory.getOperatingSystemMXBean.asInstanceOf[com.sun.management.OperatingSystemMXBean]
        (osBean.getProcessCpuLoad, osBean.getCpuLoad)
      } catch {
        case _: ClassCastException => (-1.0, -1.0)
      }

    val currentCompiles = concurrentCompiles.get()
    val loadedClasses = ManagementFactory.getClassLoadingMXBean.getLoadedClassCount

    val cpuProcessStr = String.format(Locale.US, "%.4f", cpuProcess: java.lang.Double)
    val cpuSystemStr = String.format(Locale.US, "%.4f", cpuSystem: java.lang.Double)

    writeEvent(
      s"""{"type":"jvm","ts":${now()},"heap_used_mb":$heapUsedMb,"heap_committed_mb":$heapCommittedMb,"heap_max_mb":$heapMaxMb,"non_heap_used_mb":$nonHeapUsedMb,"gc":$gcJson,"threads":$threads,"peak_threads":$peakThreads,"daemon_threads":$daemonThreads,"cpu_process":$cpuProcessStr,"cpu_system":$cpuSystemStr,"concurrent_compiles":$currentCompiles,"loaded_classes":$loadedClasses}"""
    )

    // OOM pressure detection: heap used >= 95% of max
    val heapMaxBytes = heap.getMax
    if (heapMaxBytes > 0) {
      val usedPct = heapUsed.toDouble / heapMaxBytes
      if (usedPct >= OomThreshold) {
        if (!oomPressureStarted) {
          oomPressureStarted = true
          writeEvent(
            s"""{"type":"oom_pressure","ts":${now()},"heap_used_mb":$heapUsedMb,"heap_max_mb":$heapMaxMb,"pct":${String.format(
                Locale.US,
                "%.1f",
                (usedPct * 100): java.lang.Double
              )},"concurrent_compiles":$currentCompiles,"active_connections":${activeConnections.get()}}"""
          )
          System.err.println(s"[BspMetrics] WARNING: Heap at ${(usedPct * 100).toInt}% ($heapUsedMb/$heapMaxMb MB) with $currentCompiles concurrent compiles")
        }
      } else {
        oomPressureStarted = false
      }
    }
  }

  private def writeSummary(): Unit = {
    val threadBean = ManagementFactory.getThreadMXBean
    writeEvent(
      s"""{"type":"summary","ts":${now()},"max_concurrent_compiles":${maxConcurrentCompiles.get()},"max_active_connections":${maxActiveConnections
          .get()},"peak_threads":${threadBean.getPeakThreadCount},"max_heap_used_mb":${maxHeapUsedBytes.get() / (1024 * 1024)}}"""
    )
  }

  // --------------- helpers ---------------

  private def now(): Long = System.currentTimeMillis()

  private def writeEvent(json: String): Unit = {
    val w = writer
    if (w != null) {
      w.synchronized {
        w.write(json)
        w.newLine()
        w.flush()
      }
    }
  }

  private def updateMax(atom: AtomicInteger, value: Int): Unit = {
    var current = atom.get()
    while (value > current) {
      if (atom.compareAndSet(current, value)) return
      current = atom.get()
    }
  }

  private def updateMaxLong(atom: AtomicLong, value: Long): Unit = {
    var current = atom.get()
    while (value > current) {
      if (atom.compareAndSet(current, value)) return
      current = atom.get()
    }
  }

  /** Rotate metrics file if it exceeds the size limit. Renames current file to metrics.prev.jsonl (overwriting any existing). */
  private def rotateIfNeeded(): Unit =
    try
      if (Files.exists(metricsPath) && Files.size(metricsPath) > MaxMetricsFileBytes) {
        val prev = metricsPath.resolveSibling("metrics.prev.jsonl")
        Files.move(metricsPath, prev, java.nio.file.StandardCopyOption.REPLACE_EXISTING): Unit
      }
    catch { case _: Exception => () }

  /** Escape a string for safe JSON embedding (minimal: backslash + double quote + newlines). */
  private def esc(s: String): String =
    s.replace("\\", "\\\\").replace("\"", "\\\"").replace("\n", "\\n").replace("\r", "\\r")
}
