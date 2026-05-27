package bleep.bsp

import java.io.{BufferedWriter, FileWriter}
import java.lang.management.ManagementFactory
import java.nio.file.{Files, Path}
import java.util.Locale
import java.util.concurrent.LinkedBlockingQueue
import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger, AtomicLong}
import scala.jdk.CollectionConverters._

/** Lightweight metrics collector for the BSP server.
  *
  * Writes append-only JSONL to `<metricsDir>/metrics.jsonl`. Crash-safe: flushes after every write. Intended to always be on — the overhead is negligible (one
  * sync write per event + one background sample every few seconds).
  */
object BspMetrics {

  @volatile private var writer: BufferedWriter = scala.compiletime.uninitialized
  @volatile private var samplerThread: Thread = scala.compiletime.uninitialized
  @volatile private var writerThread: Thread = scala.compiletime.uninitialized
  @volatile private var metricsPath: Path = scala.compiletime.uninitialized

  /** Bounded queue of pending event JSON lines. A dedicated writer thread drains this into the BufferedWriter so event-producing threads (every compile/test
    * phase) don't serialize on BufferedWriter.flush — which under high event volume turned `recordCompilePhase` into a synchronous fsync per call. Cap is
    * generous: tens of thousands of events queue up to ~16MB before backpressure kicks in.
    */
  private val pendingEvents = new LinkedBlockingQueue[String](200000)

  /** Counter of dropped events (queue saturated). Surfaced in the shutdown summary. */
  private val droppedEvents = new AtomicInteger(0)

  /** Flipped by `shutdown()`. The writer thread polls this so it can drain and exit. */
  private val shuttingDown = new AtomicBoolean(false)

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

    val t = new Thread("bsp-metrics-sampler") {
      override def run(): Unit =
        try
          while (!Thread.currentThread().isInterrupted) {
            sampleJvm()
            Thread.sleep(SampleIntervalMs)
          }
        catch {
          case _: InterruptedException => () // shutdown
        }
    }
    t.setDaemon(true)
    t.start()
    samplerThread = t

    val writerT = new Thread("bsp-metrics-writer") {
      override def run(): Unit = {
        val batch = new java.util.ArrayList[String](256)
        try
          while (!(shuttingDown.get() && pendingEvents.isEmpty)) {
            // Block for the first event; then drain whatever else is queued in one batch.
            val first = pendingEvents.poll(250, java.util.concurrent.TimeUnit.MILLISECONDS)
            if (first != null) {
              batch.clear()
              batch.add(first)
              pendingEvents.drainTo(batch, 1024)
              val w = writer
              if (w != null) {
                // Single synchronized region per batch, single flush at the end — far less
                // contention with shutdown.close than per-event sync+flush.
                w.synchronized {
                  try {
                    val it = batch.iterator()
                    while (it.hasNext) {
                      w.write(it.next())
                      w.newLine()
                    }
                    w.flush()
                  } catch { case _: Exception => () }
                }
              }
            }
          }
        catch {
          case _: InterruptedException => ()
        }
      }
    }
    writerT.setDaemon(true)
    writerT.start()
    writerThread = writerT
  }

  def shutdown(): Unit = {
    val t = samplerThread
    if (t != null) {
      t.interrupt()
      t.join(2000)
    }
    // Enqueue the summary BEFORE flipping shuttingDown so the writer drains it.
    writeSummary()
    shuttingDown.set(true)
    val wt = writerThread
    if (wt != null) {
      // Give the writer up to 2s to drain. If it can't, we lose some tail events —
      // acceptable at shutdown.
      wt.join(2000)
    }
    val w = writer
    if (w != null) {
      w.synchronized {
        try w.close()
        catch { case _: Exception => () }
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
    writeEventSync(
      s"""{"type":"summary","ts":${now()},"max_concurrent_compiles":${maxConcurrentCompiles.get()},"max_active_connections":${maxActiveConnections
          .get()},"peak_threads":${threadBean.getPeakThreadCount},"max_heap_used_mb":${maxHeapUsedBytes.get() / (1024 * 1024)},"dropped_events":${droppedEvents
          .get()}}"""
    )
  }

  // --------------- helpers ---------------

  private def now(): Long = System.currentTimeMillis()

  /** Enqueue an event for the writer thread. Non-blocking — if the queue is full (extreme load or writer thread stuck), the event is dropped and counted in
    * `droppedEvents`. This is the right tradeoff for metrics: never block a compile to record a metric, surface the loss in the summary instead.
    */
  private def writeEvent(json: String): Unit =
    if (writer != null) {
      if (!pendingEvents.offer(json)) droppedEvents.incrementAndGet(): Unit
    }

  /** Synchronously write — used only at shutdown for the summary, when the writer thread is about to be joined and we want the summary to land on disk
    * regardless of queue draining.
    */
  private def writeEventSync(json: String): Unit = {
    val w = writer
    if (w != null) {
      w.synchronized {
        try {
          w.write(json)
          w.newLine()
          w.flush()
        } catch { case _: Exception => () }
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
