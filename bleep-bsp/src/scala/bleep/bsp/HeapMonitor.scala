package bleep.bsp

/** Heap memory in megabytes */
case class HeapMb(value: Long) extends AnyVal

/** Millisecond timestamp */
case class EpochMs(value: Long) extends AnyVal

/** Duration in milliseconds */
case class DurationMs(value: Long) extends AnyVal

/** Abstraction for reading heap memory usage, injectable for testing. */
trait HeapMonitor {
  def heapUsage(): HeapMonitor.Usage
}

object HeapMonitor {
  case class Usage(usedBytes: Long, maxBytes: Long) {
    def usedMb: HeapMb = HeapMb(usedBytes / (1024 * 1024))
    def maxMb: HeapMb = HeapMb(maxBytes / (1024 * 1024))
    def fraction: Double = if (maxBytes > 0) usedBytes.toDouble / maxBytes else 0.0
  }

  val system: HeapMonitor = new HeapMonitor {
    private val memBean = java.lang.management.ManagementFactory.getMemoryMXBean
    def heapUsage(): Usage = {
      val heap = memBean.getHeapMemoryUsage
      Usage(heap.getUsed, heap.getMax)
    }
  }
}
