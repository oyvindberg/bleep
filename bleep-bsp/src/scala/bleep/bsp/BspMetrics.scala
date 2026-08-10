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

  /** What is compiling right now, so a heap event can name it instead of merely counting it.
    *
    * A count answers "how loaded was the server"; only the names answer "loaded with what, from which workspace" — which is the question you have when the heap
    * is at its ceiling. Reconstructing this after the fact means replaying every start/end pair in the file and hoping none were dropped.
    *
    * Keyed by workspace+project, valued by the allocation reading taken at start (see [[ThreadAllocation]]).
    */
  private val inFlightCompiles = new java.util.concurrent.ConcurrentHashMap[String, ThreadAllocation]()

  /** A thread's cumulative allocation at a point in time, for attributing bytes to whatever ran in between.
    *
    * `getThreadAllocatedBytes` is per THREAD, and a cats-effect fiber may finish on a different thread than it started on. So the reading records which thread
    * it came from, and the delta is only reported when the same thread closes it out — an unattributable compile reports nothing rather than a number that
    * silently means something else.
    */
  private case class ThreadAllocation(threadId: Long, bytes: Long, startedMs: Long)

  private val threadBeanForAllocation: Option[com.sun.management.ThreadMXBean] =
    ManagementFactory.getThreadMXBean match {
      case tb: com.sun.management.ThreadMXBean if tb.isThreadAllocatedMemorySupported =>
        tb.setThreadAllocatedMemoryEnabled(true)
        Some(tb)
      case _ => None
    }

  private def readThreadAllocation(): ThreadAllocation = {
    val t = Thread.currentThread()
    ThreadAllocation(t.threadId(), threadAllocatedBytes(), System.currentTimeMillis())
  }

  /** Cumulative bytes allocated by the CALLING thread, or -1 where the JVM does not report it.
    *
    * Public because the only place a compile is reliably pinned to one thread is inside ZincBridge's `IO.interruptible` block, so that is where the two
    * readings have to be taken. See [[recordCompileAllocation]].
    */
  def threadAllocatedBytes(): Long =
    threadBeanForAllocation.map(_.getThreadAllocatedBytes(Thread.currentThread().threadId())).getOrElse(-1L)

  /** Bytes a single compile allocated, measured across the blocking section that does the work.
    *
    * Churn, not retention: a compile that allocates 40GB and keeps none of it is a very different problem from one that keeps 4GB, and the two are
    * indistinguishable in a heap-usage graph. This is the number that says which module actually needs the headroom.
    */
  def recordCompileAllocation(project: String, allocatedBytes: Long, durationMs: Long): Unit =
    writeEvent(
      s"""{"type":"compile_allocation","ts":${now()},"project":"${esc(project)}","allocated_mb":${allocatedBytes / (1024 * 1024)},"duration_ms":$durationMs}"""
    )

  /** A compile where the noop fast path declined but zinc then recompiled nothing.
    *
    * Pure waste: the analysis was loaded and invalidation computed to reach a conclusion the manifest check could have reached from stats alone. `reason` names
    * the stat that tripped, so the aggregate answers which check is too conservative rather than just how often.
    */
  def recordNoopDisagreement(project: String, reason: String): Unit =
    writeEvent(
      s"""{"type":"noop_disagreement","ts":${now()},"project":"${esc(project)}","reason":"${esc(reason)}"}"""
    )

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
          // An OOM on a worker thread (CE pool, io-compute) is otherwise only logged, and the
          // server keeps serving with poisoned static initializers — every later compile then
          // fails with NoClassDefFoundError. Force-exit so BspRifle restarts a clean server.
          // (-XX:+ExitOnOutOfMemoryError usually beats us to it; this is the belt-and-suspenders.)
          System.err.println(s"[FATAL] OutOfMemoryError on thread ${thread.getName} — halting for restart")
          Runtime.getRuntime.halt(1)
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
    inFlightCompiles.put(inFlightKey(project, workspace), readThreadAllocation()): Unit
    writeEvent(s"""{"type":"compile_start","ts":${now()},"project":"${esc(project)}","workspace":"${esc(workspace)}","concurrent":$current}""")
  }

  def recordCompileEnd(project: String, workspace: String, durationMs: Long, success: Boolean): Unit = {
    val current = concurrentCompiles.decrementAndGet()
    inFlightCompiles.remove(inFlightKey(project, workspace)): Unit
    // No allocation figure here on purpose — see recordCompileAllocation, which is emitted from
    // inside the compile's own blocking section where the thread is stable.
    writeEvent(
      s"""{"type":"compile_end","ts":${now()},"project":"${esc(project)}","workspace":"${esc(
          workspace
        )}","duration_ms":$durationMs,"success":$success,"concurrent":$current}"""
    )
  }

  private def inFlightKey(project: String, workspace: String): String = s"$workspace::$project"

  /** The in-flight compiles as a JSON array, newest last. Attached to heap events so they name what was running. */
  private def inFlightJson(): String = {
    val now = System.currentTimeMillis()
    inFlightCompiles
      .entrySet()
      .asScala
      .toVector
      .sortBy(_.getValue.startedMs)
      .map(e => s"""{"key":"${esc(e.getKey)}","age_ms":${now - e.getValue.startedMs}}""")
      .mkString("[", ",", "]")
  }

  /** Machine-governor state: what the daemon-wide resource governor is admitting and what is queued behind it. Emitted periodically by the daemon's reporter,
    * which is the only thing holding the governor.
    *
    * `total_memory_mb` is the *fork* budget, not the machine's RAM — [[bleep.MachineResources.forkMemoryBudgetMb]] subtracts the server's own footprint and an
    * OS reserve, and [[bleep.MachineResources.retuneMemoryBudget]] then moves it up and down with what the machine can actually spare. Two consequences that
    * repeatedly misled readers of this data: `used_memory_mb` can legitimately exceed it (retuning down while reservations are held), and the machine's RAM
    * cannot be recovered from it by inverting the formula, because the value sampled is rarely the initial one.
    *
    * So `physical_memory_mb` and `server_heap_mb` are recorded outright. They are constant per process and mildly redundant on every 15s sample, which is the
    * price of each sample being self-describing rather than something to reconstruct.
    */
  def recordMachine(
      usedCpu: Int,
      totalCpu: Int,
      usedMemoryMb: Long,
      totalMemoryMb: Long,
      physicalMemoryMb: Long,
      serverHeapMb: Long,
      activeCompiles: Int,
      running: Int,
      waiting: Int
  ): Unit =
    writeEvent(
      s"""{"type":"machine","ts":${now()},"used_cpu":$usedCpu,"total_cpu":$totalCpu,"used_memory_mb":$usedMemoryMb,"total_memory_mb":$totalMemoryMb,"physical_memory_mb":$physicalMemoryMb,"server_heap_mb":$serverHeapMb,"active_compiles":$activeCompiles,"running":$running,"waiting":$waiting}"""
    )

  /** What the Zinc analysis cache is holding after each sweep. The largest single retainer in the server heap, so its size is the first number to look at when
    * the live set is climbing.
    */
  def recordAnalysisCache(stats: bleep.analysis.AnalysisCache.Stats): Unit = {
    // Per workspace, because the total alone was what the old telemetry gave and it left the actual
    // question — which build is holding the heap — to be reconstructed from a class histogram.
    val perWorkspace = stats.perWorkspace
      .map(w =>
        s"""{"workspace":"${esc(w.key.workspace.toString)}","variant":"${esc(w.key.variant.toString)}","entries":${w.entries},"file_bytes":${w.fileBytes}}"""
      )
      .mkString("[", ",", "]")
    writeEvent(
      s"""{"type":"analysis_cache","ts":${now()},"entries":${stats.entries},"file_bytes":${stats.fileBytes},"workspaces":${stats.perWorkspace.size},"interned_classes":${stats.internedClasses},"intern_hits":${stats.internHits},"intern_misses":${stats.internMisses},"shared_analyses":${stats.sharedAnalyses},"content_hits":${stats.contentHits},"sharing_factor":${String
          .format(Locale.US, "%.2f", stats.sharingFactor: java.lang.Double)},"per_workspace":$perWorkspace}"""
    )
  }

  /** Which workspaces the daemon is holding resolved builds for. The retained-heap floor tracks this number, so recording it is what makes the floor
    * attributable instead of merely visible.
    */
  def recordWorkspaceState(cached: List[String], maxCached: Int): Unit =
    writeEvent(
      s"""{"type":"workspace_state","ts":${now()},"cached_count":${cached.size},"max_cached":$maxCached,"cached":${cached
          .map(w => s""""${esc(w)}"""")
          .mkString("[", ",", "]")}}"""
    )

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

  /** Linking a non-JVM target: Scala.js, Scala Native, Kotlin/JS, Kotlin/Native.
    *
    * Recorded because it was the one expensive thing the server did and never mentioned. A compile has `compile_start` / `compile_end`, a forked JVM has five
    * events, and a link — which forks a whole linker toolchain, and for Scala Native means NIR -> LLVM IR -> clang -> executable — had none. It was charged a
    * cost by the governor and then vanished, so "why was this build slow" could not be answered for any build that links.
    *
    * `release_mode` is here because it is the field that predicts the number: the same project linked Debug and linked ReleaseFast are different work by a
    * large factor, and without it a slow link and a fast one are one population. `platform` for the same reason across toolchains.
    */
  def recordLinkStart(project: String, workspace: String, platform: String, releaseMode: Boolean, isTest: Boolean): Unit =
    writeEvent(
      s"""{"type":"link_start","ts":${now()},"project":"${esc(project)}","workspace":"${esc(workspace)}","platform":"${esc(
          platform
        )}","release_mode":$releaseMode,"is_test":$isTest}"""
    )

  def recordLinkEnd(
      project: String,
      workspace: String,
      platform: String,
      releaseMode: Boolean,
      isTest: Boolean,
      durationMs: Long,
      success: Boolean
  ): Unit =
    writeEvent(
      s"""{"type":"link_end","ts":${now()},"project":"${esc(project)}","workspace":"${esc(workspace)}","platform":"${esc(
          platform
        )}","release_mode":$releaseMode,"is_test":$isTest,"duration_ms":$durationMs,"success":$success}"""
    )

  def recordCompilePhase(project: String, phase: String, trackedApis: Int): Unit =
    writeEvent(
      s"""{"type":"compile_phase","ts":${now()},"project":"${esc(project)}","phase":"${esc(phase)}","tracked_apis":$trackedApis}"""
    )

  /** A compile the admission gate declined to start, and why.
    *
    * Was `heap_pressure_stall`, which is what it is called when it fires and what it almost never is. Across two full CI runs, 74 of 74 events were recorded
    * below the pressure threshold — the server never crossed it at all, peaking at 65-69% of a 4GB heap. Every one was the gate's deliberate stagger, which
    * defers a project's first admission attempt whenever anything else is compiling, at 5% heap as readily as at 79%. Named and shaped as it was, the data said
    * "this build spent its time starved of memory" when it meant "these compiles were spread out on purpose".
    *
    * `delay_ms` because the previous event carried none: [[HeapPressureGate.decide]] computes the stagger and hands it to the listener, and it stopped there.
    * The event whose job is explaining a slow build could not say whether 36 defers cost 7 seconds or 70.
    *
    * `others_compiling` is what the gate actually consulted (`machine.activeCompiles`), not [[concurrentCompiles]], which counts something else and disagreed —
    * events recorded `concurrent_compiles: 0` while the gate was deferring precisely because others were compiling.
    */
  def recordAdmissionDefer(project: String, reason: String, heapUsedMb: Long, heapMaxMb: Long, delayMs: Long, othersCompiling: Int): Unit =
    writeEvent(
      s"""{"type":"admission_defer","ts":${now()},"project":"${esc(project)}","reason":"${esc(
          reason
        )}","heap_used_mb":$heapUsedMb,"heap_max_mb":$heapMaxMb,"delay_ms":$delayMs,"others_compiling":$othersCompiling}"""
    )

  /** Forked test JVMs, by pid, so a test run can be reconstructed after the fact.
    *
    * The compile server already recorded what it compiled; what it *ran* was invisible. These three plus [[recordSuiteScheduled]] answer the questions a slow
    * or flaky test run actually raises: how many JVMs were started, how many were reused rather than respawned, how long each lived, which were killed rather
    * than finishing, and which suite ran on which one. The pid is the join key between all of them.
    */
  /** The pool announces; this forwards to the same jsonl everything else lands in. Lives here rather than at the call site so there is one place that knows how
    * a fork becomes an event.
    */
  val jvmPoolListener: bleep.testing.JvmPoolListener = new bleep.testing.JvmPoolListener {
    def onForkStart(pid: Long, label: String, xmxMb: Option[Long]): Unit = recordForkStart(pid, label, xmxMb)
    def onForkEnd(pid: Long, lifetimeMs: Long, exit: String, killedByUs: Option[String]): Unit = recordForkEnd(pid, lifetimeMs, exit, killedByUs)
    def onForkReused(pid: Long, label: String): Unit = recordForkReused(pid, label)
  }

  def recordForkStart(pid: Long, label: String, xmxMb: Option[Long]): Unit =
    writeEvent(
      s"""{"type":"fork_start","ts":${now()},"pid":$pid,"label":"${esc(label)}","xmx_mb":${xmxMb.getOrElse(-1L)}}"""
    )

  /** `killed_by` is the whole point of recording this separately from the exit summary: `destroyForcibly` sends SIGKILL, so a fork bleep terminated is
    * indistinguishable from one the OS killed unless we say which it was.
    */
  def recordForkEnd(pid: Long, lifetimeMs: Long, exit: String, killedByUs: Option[String]): Unit =
    writeEvent(
      s"""{"type":"fork_end","ts":${now()},"pid":$pid,"lifetime_ms":$lifetimeMs,"exit":"${esc(exit)}","killed_by":${killedByUs
          .map(r => s""""${esc(r)}"""")
          .getOrElse("null")}}"""
    )

  /** A pooled JVM handed out again instead of a new one being forked. The ratio against `fork_start` is how much the pool actually saves. */
  def recordForkReused(pid: Long, label: String): Unit =
    writeEvent(s"""{"type":"fork_reused","ts":${now()},"pid":$pid,"label":"${esc(label)}"}""")

  /** Which suite was placed on which JVM, and how it went. Keyed on pid, so it joins to the fork events above. */
  def recordSuiteScheduled(pid: Long, project: String, suite: String, framework: String): Unit =
    writeEvent(
      s"""{"type":"suite_scheduled","ts":${now()},"pid":$pid,"project":"${esc(project)}","suite":"${esc(suite)}","framework":"${esc(framework)}"}"""
    )

  def recordSuiteFinished(pid: Long, project: String, suite: String, durationMs: Long, outcome: String): Unit =
    writeEvent(
      s"""{"type":"suite_finished","ts":${now()},"pid":$pid,"project":"${esc(project)}","suite":"${esc(
          suite
        )}","duration_ms":$durationMs,"outcome":"${esc(outcome)}"}"""
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
    val stats = JvmSampler.sample()

    val heapUsed = JvmSampler.heapUsedBytes()
    updateMaxLong(maxHeapUsedBytes, heapUsed)

    val heapUsedMb = stats.heapUsedMb
    val heapMaxMb = stats.heapMaxMb
    val heapLiveMb = stats.heapLiveMb

    val gcJson = stats.gc
      .map(gc => s"""{"name":"${esc(gc.name)}","count":${gc.count},"time_ms":${gc.timeMs}}""")
      .mkString("[", ",", "]")

    val currentCompiles = concurrentCompiles.get()

    val cpuProcessStr = String.format(Locale.US, "%.4f", stats.cpuProcess: java.lang.Double)
    val cpuSystemStr = String.format(Locale.US, "%.4f", stats.cpuSystem: java.lang.Double)

    writeEvent(
      s"""{"type":"jvm","ts":${now()},"heap_used_mb":$heapUsedMb,"heap_committed_mb":${stats.heapCommittedMb},"heap_max_mb":$heapMaxMb,"non_heap_used_mb":${stats.nonHeapUsedMb},"gc":$gcJson,"threads":${stats.threads},"peak_threads":${stats.peakThreads},"daemon_threads":${stats.daemonThreads},"cpu_process":$cpuProcessStr,"cpu_system":$cpuSystemStr,"concurrent_compiles":$currentCompiles,"loaded_classes":${stats.loadedClasses},"heap_live_mb":$heapLiveMb}"""
    )

    // OOM pressure detection: heap used >= 95% of max
    val heapMaxBytes = JvmSampler.heapMaxBytes()
    if (heapMaxBytes > 0) {
      val usedPct = heapUsed.toDouble / heapMaxBytes
      if (usedPct >= OomThreshold) {
        if (!oomPressureStarted) {
          oomPressureStarted = true
          writeEvent(
            s"""{"type":"oom_pressure","ts":${now()},"heap_used_mb":$heapUsedMb,"heap_live_mb":$heapLiveMb,"heap_max_mb":$heapMaxMb,"pct":${String.format(
                Locale.US,
                "%.1f",
                (usedPct * 100): java.lang.Double
              )},"concurrent_compiles":$currentCompiles,"active_connections":${activeConnections.get()},"in_flight":${inFlightJson()}}"""
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
