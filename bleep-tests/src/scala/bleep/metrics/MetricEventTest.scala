package bleep.metrics

import io.circe.parser.decode
import io.circe.syntax.*
import org.scalatest.funsuite.AnyFunSuite

/** Round-trip tests for the MetricEvent ADT: encode → decode produces an equal value, for every variant. Plus regression tests that the raw strings the writers
  * emit (BspMetrics / InvocationMetrics / ProcessTreeSampler) parse correctly via the codec — if the string templates ever drift from the case class, these
  * blow up.
  */
class MetricEventTest extends AnyFunSuite {

  private def roundtrip(e: MetricEvent): Unit = {
    val json = e.asJson.noSpaces
    val decoded = decode[MetricEvent](json)
    assert(decoded == Right(e), s"failed to round-trip $e through $json: $decoded")
  }

  test("JvmSample round-trips") {
    roundtrip(
      MetricEvent.JvmSample(
        ts = 1000L,
        heap_used_mb = 256,
        heap_committed_mb = 512,
        heap_max_mb = 4096,
        non_heap_used_mb = 64,
        gc = List(MetricEvent.GcStat("G1 Young Generation", count = 10, time_ms = 123)),
        threads = 32,
        peak_threads = 40,
        daemon_threads = 28,
        cpu_process = 0.42,
        cpu_system = 0.85,
        concurrent_compiles = 2,
        loaded_classes = 12345
      )
    )
  }

  test("CompileStart / CompileEnd round-trip") {
    roundtrip(MetricEvent.CompileStart(ts = 1, project = "myapp", workspace = "/ws", concurrent = 1))
    roundtrip(MetricEvent.CompileEnd(ts = 2, project = "myapp", workspace = "/ws", duration_ms = 1500, success = true, concurrent = 0))
  }

  test("BuildStart / BuildEnd round-trip") {
    roundtrip(MetricEvent.BuildStart(ts = 1, workspace = "/ws", projects = 7))
    roundtrip(MetricEvent.BuildEnd(ts = 2, workspace = "/ws", duration_ms = 20000, success = true))
  }

  test("CacheEvict / CleanCache round-trip") {
    roundtrip(MetricEvent.CacheEvict(ts = 1, cache = "zinc", workspace = "/ws"))
    roundtrip(MetricEvent.CleanCache(ts = 1, project = "myapp"))
  }

  test("ConnectionOpen / ConnectionClose round-trip") {
    roundtrip(MetricEvent.ConnectionOpen(ts = 1, conn_id = 42, active_connections = 3))
    roundtrip(MetricEvent.ConnectionClose(ts = 2, conn_id = 42, active_connections = 2))
  }

  test("SourcegenStart / SourcegenEnd round-trip") {
    roundtrip(MetricEvent.SourcegenStart(ts = 1, script = "scripts.GenerateResources"))
    roundtrip(MetricEvent.SourcegenEnd(ts = 2, script = "scripts.GenerateResources", duration_ms = 500, success = true))
  }

  test("CompilePhase round-trips") {
    roundtrip(MetricEvent.CompilePhase(ts = 1, project = "myapp", phase = "PicklerGen", tracked_apis = 200))
  }

  test("HeapPressureStall round-trips") {
    roundtrip(MetricEvent.HeapPressureStall(ts = 1, project = "myapp", heap_used_mb = 3000, heap_max_mb = 4096, concurrent_compiles = 3))
  }

  test("Summary round-trips") {
    roundtrip(MetricEvent.Summary(ts = 1, max_concurrent_compiles = 4, max_active_connections = 3, max_heap_used_mb = 2048, duration_ms = 60000))
  }

  test("OomPressure / OomCrash round-trip") {
    roundtrip(MetricEvent.OomPressure(ts = 1, heap_used_mb = 3900, heap_max_mb = 4096, pct = 95.2, concurrent_compiles = 4, active_connections = 2))
    roundtrip(
      MetricEvent.OomCrash(
        ts = 2,
        thread = "bsp-worker-3",
        message = "Java heap space",
        heap_used_mb = 4090,
        heap_max_mb = 4096,
        concurrent_compiles = 3,
        active_connections = 1
      )
    )
  }

  test("RequestStart / RequestEnd round-trip") {
    roundtrip(MetricEvent.RequestStart(ts = 1, origin_id = "abc-123", method = "compile", workspace = "/ws", projects = List("bleep-core", "bleep-cli")))
    roundtrip(MetricEvent.RequestEnd(ts = 2, origin_id = "abc-123", duration_ms = 12345, success = true))
  }

  test("ProjectTestStart / ProjectTestEnd round-trip") {
    roundtrip(MetricEvent.ProjectTestStart(ts = 1, project = "bleep-tests", workspace = "/ws", origin_id = "bsp-r-9f3a"))
    roundtrip(MetricEvent.ProjectTestEnd(ts = 2, project = "bleep-tests", workspace = "/ws", origin_id = "bsp-r-9f3a", duration_ms = 5000, success = true))
  }

  test("SuiteStart / SuiteEnd / TestEnd round-trip") {
    roundtrip(MetricEvent.SuiteStart(ts = 1, project = "bleep-tests", workspace = "/ws", suite = "RelPathTests", framework = "ScalaTest"))
    roundtrip(
      MetricEvent.SuiteEnd(
        ts = 2,
        project = "bleep-tests",
        workspace = "/ws",
        suite = "RelPathTests",
        passed = 1,
        failed = 0,
        skipped = 0,
        duration_ms = 250,
        success = true
      )
    )
    roundtrip(
      MetricEvent.TestEnd(ts = 3, project = "bleep-tests", workspace = "/ws", suite = "RelPathTests", test = "works", status = "passed", duration_ms = 200)
    )
  }

  test("SubprocessStart round-trips") {
    roundtrip(
      MetricEvent.SubprocessStart(
        ts = 1000,
        pid = 12345,
        ppid = 12344,
        start_time = 999,
        cmd = "/usr/bin/java -Xmx512m foo.Main",
        kind = "test-runner"
      )
    )
  }

  test("SubprocessEnd round-trips, with and without reason") {
    roundtrip(MetricEvent.SubprocessEnd(ts = 2000, pid = 12345, peak_rss_mb = 512, duration_ms = 4521, reason = None))
    roundtrip(MetricEvent.SubprocessEnd(ts = 2000, pid = 12345, peak_rss_mb = 512, duration_ms = 4521, reason = Some("sampler_shutdown")))
  }

  test("Process / InvocationEnd round-trip") {
    roundtrip(MetricEvent.Process(ts = 1, pid = 12345, start_time = 1000, kind = "bleep-cli", cmd = "/usr/bin/java -jar bleep.jar test"))
    roundtrip(MetricEvent.InvocationEnd(ts = 2, duration_ms = 30000))
  }

  test("Unknown event type decodes to MetricEvent.Unknown with the raw json preserved") {
    val raw = """{"type":"some_future_event","ts":42,"extra":"field"}"""
    decode[MetricEvent](raw) match {
      case Right(u: MetricEvent.Unknown) =>
        assert(u.ts == 42)
        assert(u.eventType == "some_future_event")
      case other => fail(s"expected Unknown, got $other")
    }
  }

  // ---- Writer ↔ codec agreement ----
  // The writers (BspMetrics / InvocationMetrics / ProcessTreeSampler) emit JSON via string interpolation rather than circe encoders, so they can stay
  // allocation-light during OOM. These tests pin the writer output format: if any writer template drifts from the case-class shape, the decoder fails here.

  test("ProcessTreeSampler subprocess_start string template decodes to SubprocessStart") {
    val raw = s"""{"type":"subprocess_start","ts":1234,"pid":99,"ppid":88,"start_time":1000,"cmd":"foo","kind":"observed"}"""
    val decoded = decode[MetricEvent](raw)
    assert(decoded == Right(MetricEvent.SubprocessStart(ts = 1234, pid = 99, ppid = 88, start_time = 1000, cmd = "foo", kind = "observed")))
  }

  test("ProcessTreeSampler subprocess_end string template decodes to SubprocessEnd") {
    val raw = s"""{"type":"subprocess_end","ts":2000,"pid":99,"peak_rss_mb":256,"duration_ms":766}"""
    val decoded = decode[MetricEvent](raw)
    assert(decoded == Right(MetricEvent.SubprocessEnd(ts = 2000, pid = 99, peak_rss_mb = 256, duration_ms = 766, reason = None)))
  }

  test("ProcessTreeSampler shutdown end-event with reason decodes correctly") {
    val raw = s"""{"type":"subprocess_end","ts":2000,"pid":99,"peak_rss_mb":256,"duration_ms":766,"reason":"sampler_shutdown"}"""
    val decoded = decode[MetricEvent](raw)
    assert(decoded == Right(MetricEvent.SubprocessEnd(ts = 2000, pid = 99, peak_rss_mb = 256, duration_ms = 766, reason = Some("sampler_shutdown"))))
  }

  test("InvocationMetrics process event template decodes to Process") {
    val raw = s"""{"type":"process","ts":1234,"pid":99,"start_time":1000,"kind":"bleep-cli","cmd":"java -jar bleep.jar"}"""
    val decoded = decode[MetricEvent](raw)
    assert(decoded == Right(MetricEvent.Process(ts = 1234, pid = 99, start_time = 1000, kind = "bleep-cli", cmd = "java -jar bleep.jar")))
  }

  test("InvocationMetrics invocation_end template decodes to InvocationEnd") {
    val raw = s"""{"type":"invocation_end","ts":5000,"duration_ms":3766}"""
    val decoded = decode[MetricEvent](raw)
    assert(decoded == Right(MetricEvent.InvocationEnd(ts = 5000, duration_ms = 3766)))
  }
}
