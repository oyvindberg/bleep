package bleep.bsp

import java.io.{BufferedWriter, FileWriter}
import java.lang.management.ManagementFactory
import java.nio.file.{Files, Path}
import java.util.Locale
import java.util.concurrent.atomic.{AtomicInteger, AtomicLong}
import scala.jdk.CollectionConverters._

/** Lightweight metrics collector for the BSP server.
  *
  * Writes append-only JSONL to `<metricsDir>/metrics.jsonl`. Crash-safe: flushes after every write. Intended to always be on — the overhead is negligible
  * (one sync write per event + one background sample every few seconds).
  */
object BspMetrics {

  @volatile private var writer: BufferedWriter = _
  @volatile private var samplerThread: Thread = _
  @volatile private var metricsPath: Path = _

  // High watermarks
  private val maxConcurrentCompiles = AtomicInteger(0)
  private val maxActiveConnections = AtomicInteger(0)
  private val maxHeapUsedBytes = AtomicLong(0)

  // Counters
  private val concurrentCompiles = AtomicInteger(0)
  private val activeConnections = AtomicInteger(0)

  private val SampleIntervalMs = 5000L

  def initialize(metricsDir: Path): Unit = {
    metricsPath = metricsDir.resolve("metrics.jsonl")
    writer = new BufferedWriter(new FileWriter(metricsPath.toFile, true)) // append mode

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
  }

  def shutdown(): Unit = {
    val t = samplerThread
    if (t != null) {
      t.interrupt()
      t.join(2000)
    }
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
      s"""{"type":"compile_end","ts":${now()},"project":"${esc(project)}","workspace":"${esc(workspace)}","duration_ms":$durationMs,"success":$success,"concurrent":$current}"""
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

    val (cpuProcess, cpuSystem) = try {
      val osBean = ManagementFactory.getOperatingSystemMXBean.asInstanceOf[com.sun.management.OperatingSystemMXBean]
      (osBean.getProcessCpuLoad, osBean.getSystemCpuLoad)
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
  }

  private def writeSummary(): Unit = {
    val threadBean = ManagementFactory.getThreadMXBean
    writeEvent(
      s"""{"type":"summary","ts":${now()},"max_concurrent_compiles":${maxConcurrentCompiles.get()},"max_active_connections":${maxActiveConnections.get()},"peak_threads":${threadBean.getPeakThreadCount},"max_heap_used_mb":${maxHeapUsedBytes.get() / (1024 * 1024)}}"""
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

  /** Escape a string for safe JSON embedding (minimal: backslash + double quote + newlines). */
  private def esc(s: String): String =
    s.replace("\\", "\\\\").replace("\"", "\\\"").replace("\n", "\\n").replace("\r", "\\r")
}
