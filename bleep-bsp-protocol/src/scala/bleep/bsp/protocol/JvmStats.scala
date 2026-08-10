package bleep.bsp.protocol

import io.circe.Codec
import io.circe.generic.semiauto.deriveCodec

/** One garbage collector's cumulative counters. */
case class GcStat(name: String, count: Long, timeMs: Long)

object GcStat {
  implicit val codec: Codec[GcStat] = deriveCodec
}

/** A point-in-time reading of the daemon JVM.
  *
  * Lives here rather than in bleep-bsp because it travels: the same shape is written to `metrics.jsonl` for the historical dashboard and returned by
  * `bleep/status` for the live view. Measurement is [[bleep.bsp.JvmSampler]]; this module stays free of MXBeans.
  *
  * Two fields carry a deliberate "unknown", because averaging a fabricated zero into these is worse than showing nothing:
  *   - `heapLiveMb` is `-1` when the JVM does not report collection usage for its heap pools
  *   - `openFileDescriptors` is `None` where the platform has no such notion
  */
case class JvmStats(
    heapUsedMb: Long,
    heapCommittedMb: Long,
    heapMaxMb: Long,
    heapLiveMb: Long,
    nonHeapUsedMb: Long,
    gc: List[GcStat],
    threads: Int,
    peakThreads: Int,
    daemonThreads: Int,
    cpuProcess: Double,
    cpuSystem: Double,
    loadedClasses: Int,
    openFileDescriptors: Option[Long]
)

object JvmStats {
  implicit val codec: Codec[JvmStats] = deriveCodec
}
