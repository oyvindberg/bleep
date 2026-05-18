package bleep.metrics

import io.circe.*
import io.circe.generic.semiauto.*
import io.circe.syntax.*

/** Typed model of one line in a `metrics.jsonl` stream. The wire format is flat — `type` lives at the top level alongside the other fields, no `data:` wrapping
  * (different from the BSP protocol's events). Decode discriminates on the `type` field; encode injects it back.
  *
  * Writers (`BspMetrics`, `InvocationMetrics`, `ProcessTreeSampler`) emit via raw string interpolation rather than going through these encoders — that path is
  * hot and needs to be allocation-light during OOM scenarios. The encoders below are kept so:
  *   - The dashboard parses into typed values instead of poking JsonObjects with stringly-typed field names.
  *   - Tests can round-trip writer output through the decoder, catching any drift between the string templates and the codec.
  *
  * If you add a new event type, add a case class + codec + decoder branch and also a writer in the appropriate Metrics object. The round-trip test in
  * `MetricEventTest` will fail loudly if the writer's JSON doesn't match the codec.
  */
sealed trait MetricEvent {
  def ts: Long
}

object MetricEvent {

  case class GcStat(name: String, count: Long, time_ms: Long)

  case class JvmSample(
      ts: Long,
      heap_used_mb: Long,
      heap_committed_mb: Long,
      heap_max_mb: Long,
      non_heap_used_mb: Long,
      gc: List[GcStat],
      threads: Long,
      peak_threads: Long,
      daemon_threads: Long,
      cpu_process: Double,
      cpu_system: Double,
      concurrent_compiles: Long,
      loaded_classes: Long
  ) extends MetricEvent

  case class CompileStart(ts: Long, project: String, workspace: String, concurrent: Long) extends MetricEvent
  case class CompileEnd(ts: Long, project: String, workspace: String, duration_ms: Long, success: Boolean, concurrent: Long) extends MetricEvent
  case class BuildStart(ts: Long, workspace: String, projects: Long) extends MetricEvent
  case class BuildEnd(ts: Long, workspace: String, duration_ms: Long, success: Boolean) extends MetricEvent
  case class CacheEvict(ts: Long, cache: String, workspace: String) extends MetricEvent
  case class CleanCache(ts: Long, project: String) extends MetricEvent
  case class ConnectionOpen(ts: Long, conn_id: Long, active_connections: Long) extends MetricEvent
  case class ConnectionClose(ts: Long, conn_id: Long, active_connections: Long) extends MetricEvent
  case class SourcegenStart(ts: Long, script: String) extends MetricEvent
  case class SourcegenEnd(ts: Long, script: String, duration_ms: Long, success: Boolean) extends MetricEvent
  case class CompilePhase(ts: Long, project: String, phase: String, tracked_apis: Long) extends MetricEvent
  case class HeapPressureStall(ts: Long, project: String, heap_used_mb: Long, heap_max_mb: Long, concurrent_compiles: Long) extends MetricEvent

  case class Summary(
      ts: Long,
      max_concurrent_compiles: Long,
      max_active_connections: Long,
      max_heap_used_mb: Long,
      duration_ms: Long
  ) extends MetricEvent

  case class OomPressure(
      ts: Long,
      heap_used_mb: Long,
      heap_max_mb: Long,
      pct: Double,
      concurrent_compiles: Long,
      active_connections: Long
  ) extends MetricEvent

  case class OomCrash(
      ts: Long,
      thread: String,
      message: String,
      heap_used_mb: Long,
      heap_max_mb: Long,
      concurrent_compiles: Long,
      active_connections: Long
  ) extends MetricEvent

  case class ProjectTestStart(ts: Long, project: String, workspace: String, origin_id: String) extends MetricEvent
  case class ProjectTestEnd(ts: Long, project: String, workspace: String, origin_id: String, duration_ms: Long, success: Boolean) extends MetricEvent

  /** BSP request boundary — emitted at handler entry/exit for `buildTarget/compile`, `buildTarget/test`, etc. The `originId` is the BSP-protocol request id
    * supplied by the CLI; it ties together every event the daemon emits while serving the request, and ultimately becomes the OTLP `traceId` so the whole thing
    * renders as one trace in Jaeger / Tempo / otel-desktop-viewer.
    */
  case class RequestStart(ts: Long, origin_id: String, method: String, workspace: String, projects: List[String]) extends MetricEvent
  case class RequestEnd(ts: Long, origin_id: String, duration_ms: Long, success: Boolean) extends MetricEvent

  case class SuiteStart(ts: Long, project: String, workspace: String, suite: String, framework: String) extends MetricEvent
  case class SuiteEnd(
      ts: Long,
      project: String,
      workspace: String,
      suite: String,
      passed: Long,
      failed: Long,
      skipped: Long,
      duration_ms: Long,
      success: Boolean
  ) extends MetricEvent
  case class TestEnd(ts: Long, project: String, workspace: String, suite: String, test: String, status: String, duration_ms: Long) extends MetricEvent

  case class SubprocessStart(ts: Long, pid: Long, ppid: Long, start_time: Long, cmd: String, kind: String) extends MetricEvent
  case class SubprocessEnd(ts: Long, pid: Long, peak_rss_mb: Long, duration_ms: Long, reason: Option[String]) extends MetricEvent

  case class Process(ts: Long, pid: Long, start_time: Long, kind: String, cmd: String) extends MetricEvent
  case class InvocationEnd(ts: Long, duration_ms: Long) extends MetricEvent

  /** Catch-all for forward-compatibility: an unknown `type` value decodes into this with the raw JSON preserved, so the dashboard doesn't crash on metrics
    * files produced by a newer/older bleep. Carries the timestamp when available so it still sorts into the timeline.
    */
  case class Unknown(ts: Long, eventType: String, raw: Json) extends MetricEvent

  // ----- per-variant codecs -----

  given Codec[GcStat] = deriveCodec
  given Codec[JvmSample] = deriveCodec
  given Codec[CompileStart] = deriveCodec
  given Codec[CompileEnd] = deriveCodec
  given Codec[BuildStart] = deriveCodec
  given Codec[BuildEnd] = deriveCodec
  given Codec[CacheEvict] = deriveCodec
  given Codec[CleanCache] = deriveCodec
  given Codec[ConnectionOpen] = deriveCodec
  given Codec[ConnectionClose] = deriveCodec
  given Codec[SourcegenStart] = deriveCodec
  given Codec[SourcegenEnd] = deriveCodec
  given Codec[CompilePhase] = deriveCodec
  given Codec[HeapPressureStall] = deriveCodec
  given Codec[Summary] = deriveCodec
  given Codec[OomPressure] = deriveCodec
  given Codec[OomCrash] = deriveCodec
  given Codec[ProjectTestStart] = deriveCodec
  given Codec[ProjectTestEnd] = deriveCodec
  given Codec[RequestStart] = deriveCodec
  given Codec[RequestEnd] = deriveCodec
  given Codec[SuiteStart] = deriveCodec
  given Codec[SuiteEnd] = deriveCodec
  given Codec[TestEnd] = deriveCodec
  given Codec[SubprocessStart] = deriveCodec
  given Codec[SubprocessEnd] = deriveCodec
  given Codec[Process] = deriveCodec
  given Codec[InvocationEnd] = deriveCodec

  /** Discriminator string for each variant. Single source of truth — used by both the encoder (to write `"type":"<tag>"`) and the decoder (to dispatch on it).
    */
  def discriminator(e: MetricEvent): String = e match {
    case _: JvmSample         => "jvm"
    case _: CompileStart      => "compile_start"
    case _: CompileEnd        => "compile_end"
    case _: BuildStart        => "build_start"
    case _: BuildEnd          => "build_end"
    case _: CacheEvict        => "cache_evict"
    case _: CleanCache        => "clean_cache"
    case _: ConnectionOpen    => "connection_open"
    case _: ConnectionClose   => "connection_close"
    case _: SourcegenStart    => "sourcegen_start"
    case _: SourcegenEnd      => "sourcegen_end"
    case _: CompilePhase      => "compile_phase"
    case _: HeapPressureStall => "heap_pressure_stall"
    case _: Summary           => "summary"
    case _: OomPressure       => "oom_pressure"
    case _: OomCrash          => "oom_crash"
    case _: ProjectTestStart  => "project_test_start"
    case _: ProjectTestEnd    => "project_test_end"
    case _: RequestStart      => "request_start"
    case _: RequestEnd        => "request_end"
    case _: SuiteStart        => "suite_start"
    case _: SuiteEnd          => "suite_end"
    case _: TestEnd           => "test_end"
    case _: SubprocessStart   => "subprocess_start"
    case _: SubprocessEnd     => "subprocess_end"
    case _: Process           => "process"
    case _: InvocationEnd     => "invocation_end"
    case u: Unknown           => u.eventType
  }

  given Decoder[MetricEvent] = Decoder.instance { c =>
    c.downField("type").as[String].flatMap {
      case "jvm"                 => c.as[JvmSample]
      case "compile_start"       => c.as[CompileStart]
      case "compile_end"         => c.as[CompileEnd]
      case "build_start"         => c.as[BuildStart]
      case "build_end"           => c.as[BuildEnd]
      case "cache_evict"         => c.as[CacheEvict]
      case "clean_cache"         => c.as[CleanCache]
      case "connection_open"     => c.as[ConnectionOpen]
      case "connection_close"    => c.as[ConnectionClose]
      case "sourcegen_start"     => c.as[SourcegenStart]
      case "sourcegen_end"       => c.as[SourcegenEnd]
      case "compile_phase"       => c.as[CompilePhase]
      case "heap_pressure_stall" => c.as[HeapPressureStall]
      case "summary"             => c.as[Summary]
      case "oom_pressure"        => c.as[OomPressure]
      case "oom_crash"           => c.as[OomCrash]
      case "project_test_start"  => c.as[ProjectTestStart]
      case "project_test_end"    => c.as[ProjectTestEnd]
      case "request_start"       => c.as[RequestStart]
      case "request_end"         => c.as[RequestEnd]
      case "suite_start"         => c.as[SuiteStart]
      case "suite_end"           => c.as[SuiteEnd]
      case "test_end"            => c.as[TestEnd]
      case "subprocess_start"    => c.as[SubprocessStart]
      case "subprocess_end"      => c.as[SubprocessEnd]
      case "process"             => c.as[Process]
      case "invocation_end"      => c.as[InvocationEnd]
      case other                 =>
        val ts = c.downField("ts").as[Long].getOrElse(0L)
        c.as[Json].map(j => Unknown(ts, other, j))
    }
  }

  given Encoder[MetricEvent] = Encoder.instance { e =>
    val variantJson: Json = e match {
      case x: JvmSample         => x.asJson
      case x: CompileStart      => x.asJson
      case x: CompileEnd        => x.asJson
      case x: BuildStart        => x.asJson
      case x: BuildEnd          => x.asJson
      case x: CacheEvict        => x.asJson
      case x: CleanCache        => x.asJson
      case x: ConnectionOpen    => x.asJson
      case x: ConnectionClose   => x.asJson
      case x: SourcegenStart    => x.asJson
      case x: SourcegenEnd      => x.asJson
      case x: CompilePhase      => x.asJson
      case x: HeapPressureStall => x.asJson
      case x: Summary           => x.asJson
      case x: OomPressure       => x.asJson
      case x: OomCrash          => x.asJson
      case x: ProjectTestStart  => x.asJson
      case x: ProjectTestEnd    => x.asJson
      case x: RequestStart      => x.asJson
      case x: RequestEnd        => x.asJson
      case x: SuiteStart        => x.asJson
      case x: SuiteEnd          => x.asJson
      case x: TestEnd           => x.asJson
      case x: SubprocessStart   => x.asJson
      case x: SubprocessEnd     => x.asJson
      case x: Process           => x.asJson
      case x: InvocationEnd     => x.asJson
      case x: Unknown           => x.raw
    }
    variantJson.deepMerge(Json.obj("type" -> Json.fromString(discriminator(e))))
  }
}
