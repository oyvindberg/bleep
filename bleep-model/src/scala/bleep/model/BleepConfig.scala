package bleep.model

import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.{Decoder, Encoder}

case class BleepConfig(
    compileServerMode: Option[CompileServerMode],
    authentications: Option[Authentications],
    logTiming: Option[Boolean],
    bspServerConfig: Option[BspServerConfig],
    remoteCacheCredentials: Option[RemoteCacheCredentials]
) {
  def compileServerModeOrDefault: CompileServerMode = compileServerMode.getOrElse(CompileServerMode.Shared)
  def bspServerConfigOrDefault: BspServerConfig = bspServerConfig.getOrElse(BspServerConfig.default)
}

object BleepConfig {
  val default = BleepConfig(
    compileServerMode = None,
    authentications = None,
    logTiming = None,
    bspServerConfig = None,
    remoteCacheCredentials = None
  )

  implicit val decoder: Decoder[BleepConfig] = deriveDecoder
  implicit val encoder: Encoder[BleepConfig] = deriveEncoder
}

/** BSP server configuration - applies to compile and test operations */
case class BspServerConfig(
    /** Maximum parallelism. If None, uses parallelismRatio or defaults to all cores */
    parallelism: Option[Int],
    /** Parallelism as ratio of available cores (e.g., 0.5 = half). Used if parallelism is None */
    parallelismRatio: Option[Double],
    /** Idle timeout for test suites in minutes — resets each time a test completes */
    testIdleTimeoutMinutes: Option[Int],
    /** Max heap for forked test runner JVMs, e.g. "512m", "2g". None = JVM default */
    testRunnerMaxMemory: Option[String],
    /** Max heap for the compile server JVM, e.g. "4g". None = JVM default */
    compileServerMaxMemory: Option[String],
    /** Heap usage fraction (0.0–1.0) above which new compilations wait for memory. When other compilations are running and heap exceeds this threshold, the
      * server delays starting new compilations until memory is freed. Default: 0.80
      */
    heapPressureThreshold: Option[Double]
) {
  def effectiveParallelism: Int = {
    val cores = Runtime.getRuntime.availableProcessors
    parallelism
      .orElse(parallelismRatio.map(r => math.max(1, (cores * r).toInt)))
      .getOrElse(cores)
  }

  def effectiveTestIdleTimeoutMinutes: Int =
    testIdleTimeoutMinutes.getOrElse(BspServerConfig.DefaultTestIdleTimeoutMinutes)

  def effectiveHeapPressureThreshold: Double =
    heapPressureThreshold.getOrElse(BspServerConfig.DefaultHeapPressureThreshold)
}

object BspServerConfig {
  val DefaultTestIdleTimeoutMinutes: Int = 5
  val DefaultHeapPressureThreshold: Double = 0.80

  val default: BspServerConfig = BspServerConfig(
    parallelism = None,
    parallelismRatio = None,
    testIdleTimeoutMinutes = None,
    testRunnerMaxMemory = None,
    compileServerMaxMemory = None,
    heapPressureThreshold = None
  )

  implicit val decoder: Decoder[BspServerConfig] = deriveDecoder
  implicit val encoder: Encoder[BspServerConfig] = deriveEncoder
}
