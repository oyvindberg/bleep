package bleep.bsp

import bleep.bsp.protocol.{GcStat, JvmStats}

import java.lang.management.ManagementFactory
import scala.jdk.CollectionConverters._

/** Reads the daemon JVM's own vitals off the platform MXBeans.
  *
  * One sampler, two consumers: [[BspMetrics]] serialises the result into `metrics.jsonl` every few seconds for the historical dashboard, and the `bleep/status`
  * endpoint returns it for the live view. Before this existed the numbers were computed inline in `BspMetrics.sampleJvm` and could only ever reach a file,
  * which is why `bleep server top` needed them lifted out rather than duplicated.
  */
object JvmSampler {

  /** The live set: heap still occupied immediately AFTER the last collection of each heap pool, which is the number that says whether the server is retaining
    * or merely churning.
    *
    * `heapUsedMb` cannot answer that. Sampled at an arbitrary moment it includes whatever garbage has accumulated since the last GC, so a healthy server
    * churning hard and a server whose floor is creeping up towards its ceiling produce the same sawtooth. Deriving the floor by taking minima over a window (as
    * one has to do without this) needs a long window and still only approximates it.
    *
    * `-1` when the JVM does not report collection usage for its heap pools, which is a real answer, not a zero to be averaged in. Distinct from `0`, which is
    * itself a real measurement: no collection has happened yet, so nothing is known to be retained. Renderers must not collapse the two.
    */
  def liveSetMb(): Long = {
    val pools = ManagementFactory.getMemoryPoolMXBeans.asScala.filter(_.getType == java.lang.management.MemoryType.HEAP)
    val usages = pools.flatMap(p => Option(p.getCollectionUsage))
    if (usages.isEmpty) -1L else usages.map(_.getUsed).sum / (1024 * 1024)
  }

  def sample(): JvmStats = {
    val memBean = ManagementFactory.getMemoryMXBean
    val heap = memBean.getHeapMemoryUsage
    val nonHeap = memBean.getNonHeapMemoryUsage
    val threadBean = ManagementFactory.getThreadMXBean
    val gcBeans = ManagementFactory.getGarbageCollectorMXBeans.asScala

    // getProcessCpuLoad / getCpuLoad / getOpenFileDescriptorCount are all com.sun extensions. A JVM without them is a real possibility, so each degrades to its
    // own documented "unknown" rather than to a number that would look like a measurement.
    val osBean = ManagementFactory.getOperatingSystemMXBean
    val (cpuProcess, cpuSystem) = osBean match {
      case sunBean: com.sun.management.OperatingSystemMXBean => (sunBean.getProcessCpuLoad, sunBean.getCpuLoad)
      case _                                                 => (-1.0, -1.0)
    }
    val openFileDescriptors = osBean match {
      case unixBean: com.sun.management.UnixOperatingSystemMXBean => Some(unixBean.getOpenFileDescriptorCount)
      case _                                                      => None
    }

    JvmStats(
      heapUsedMb = heap.getUsed / (1024 * 1024),
      heapCommittedMb = heap.getCommitted / (1024 * 1024),
      heapMaxMb = heap.getMax / (1024 * 1024),
      heapLiveMb = liveSetMb(),
      nonHeapUsedMb = nonHeap.getUsed / (1024 * 1024),
      gc = gcBeans.map(gc => GcStat(gc.getName, gc.getCollectionCount, gc.getCollectionTime)).toList,
      threads = threadBean.getThreadCount,
      peakThreads = threadBean.getPeakThreadCount,
      daemonThreads = threadBean.getDaemonThreadCount,
      cpuProcess = cpuProcess,
      cpuSystem = cpuSystem,
      loadedClasses = ManagementFactory.getClassLoadingMXBean.getLoadedClassCount,
      openFileDescriptors = openFileDescriptors
    )
  }

  /** Raw heap-used in bytes, for the OOM-pressure comparison that needs the un-rounded value. */
  def heapUsedBytes(): Long = ManagementFactory.getMemoryMXBean.getHeapMemoryUsage.getUsed

  /** Raw heap-max in bytes; `0` or negative when the JVM reports no ceiling. */
  def heapMaxBytes(): Long = ManagementFactory.getMemoryMXBean.getHeapMemoryUsage.getMax
}
