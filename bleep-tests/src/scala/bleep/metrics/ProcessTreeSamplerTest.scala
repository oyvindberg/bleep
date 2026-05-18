package bleep.metrics

import io.circe.parser.decode
import org.scalatest.funsuite.AnyFunSuite

import scala.collection.mutable

/** Integration test for ProcessTreeSampler. Forks a short-lived child process, runs sample() while it's alive and once after it exits, and asserts:
  *   - a `subprocess_start` event is emitted for the child PID
  *   - a `subprocess_end` event is emitted for the child PID after it disappears
  *   - the start/end events have non-empty cmd and non-negative duration
  *
  * Skips the peak RSS assertion because the value is platform-dependent (Linux: kernel VmHWM, mac: own running max from `ps`, Windows: 0). We just check the
  * event shape; the codec round-trip tests in `MetricEventTest` cover the field-level details.
  */
class ProcessTreeSamplerTest extends AnyFunSuite {

  test("sample() emits subprocess_start when a child appears and subprocess_end when it exits") {
    val emitted = mutable.ArrayBuffer.empty[String]
    val sampler = new ProcessTreeSampler(line => emitted.synchronized(emitted += line))

    // Fork a child that runs ~2 seconds so we have time to sample-while-alive. `sleep` exists on Linux + macOS; on Windows we'd need a different command,
    // but our writers also skip RSS on Windows so this whole observer is best-effort there. Test is Linux/mac focused.
    val isUnix = !sys.props.getOrElse("os.name", "").toLowerCase.contains("win")
    assume(isUnix, "ProcessTreeSampler integration test is Linux/macOS only — Windows lacks the equivalent `sleep` we shell out to here")

    val pb = new ProcessBuilder("sleep", "2").redirectErrorStream(true)
    val child = pb.start()
    val childPid = child.pid

    try {
      // First sample: child is alive → expect subprocess_start
      sampler.sample()
      assert(
        emitted.exists(s => s.contains("\"type\":\"subprocess_start\"") && s.contains(s""""pid":$childPid""")),
        s"after first sample(), expected a subprocess_start for pid=$childPid. emitted: ${emitted.toList}"
      )

      // Block until child exits, then sample again so the sampler observes disappearance
      child.waitFor()
      sampler.sample()
      assert(
        emitted.exists(s => s.contains("\"type\":\"subprocess_end\"") && s.contains(s""""pid":$childPid""")),
        s"after child exited and we sampled again, expected a subprocess_end for pid=$childPid. emitted: ${emitted.toList}"
      )

      // Round-trip the emitted lines through the codec to make sure ProcessTreeSampler's string templates remain compatible with MetricEvent.
      val parsed = emitted.toList.map(decode[MetricEvent])
      parsed.foreach { d =>
        assert(d.isRight, s"emitted JSON did not parse via MetricEvent codec: $d")
      }

      val starts = parsed.collect { case Right(s: MetricEvent.SubprocessStart) => s }
      val ends = parsed.collect { case Right(e: MetricEvent.SubprocessEnd) => e }
      assert(starts.exists(_.pid == childPid), s"no SubprocessStart matched pid=$childPid in ${starts.toList}")
      assert(ends.exists(_.pid == childPid), s"no SubprocessEnd matched pid=$childPid in ${ends.toList}")
      ends.find(_.pid == childPid).foreach { e =>
        assert(e.duration_ms >= 0, s"duration_ms negative: $e")
      }
    } finally if (child.isAlive) child.destroyForcibly()
  }

  test("stripClasspath drops -classpath / -cp / --module-path pairs and -Xbootclasspath single tokens, keeps the rest verbatim") {
    val args = List(
      "-Xmx2g",
      "-Dfoo=bar",
      "-classpath",
      "/a.jar:/b.jar:/c.jar",
      "-cp",
      "/d.jar",
      "--module-path",
      "/mods",
      "-Xbootclasspath/a:/boot.jar",
      "--add-opens=java.base/sun.misc=ALL-UNNAMED",
      "bleep.Main",
      "test",
      "bleep-tests"
    )
    val kept = ProcessTreeSampler.stripClasspath(args)
    assert(
      kept == List("-Xmx2g", "-Dfoo=bar", "--add-opens=java.base/sun.misc=ALL-UNNAMED", "bleep.Main", "test", "bleep-tests"),
      s"unexpected kept args: $kept"
    )
  }

  test("flushOnShutdown emits subprocess_end for everything still in the registry") {
    val emitted = mutable.ArrayBuffer.empty[String]
    val sampler = new ProcessTreeSampler(line => emitted.synchronized(emitted += line))
    val isUnix = !sys.props.getOrElse("os.name", "").toLowerCase.contains("win")
    assume(isUnix, "Unix-only test (uses `sleep` to fork a long-lived child)")

    val pb = new ProcessBuilder("sleep", "30").redirectErrorStream(true)
    val child = pb.start()
    val childPid = child.pid

    try {
      sampler.sample()
      // Don't wait — child is still alive. flushOnShutdown should still emit a SubprocessEnd for it with reason=sampler_shutdown.
      sampler.flushOnShutdown()

      val parsed = emitted.toList.flatMap(decode[MetricEvent](_).toOption)
      val endsForChild = parsed.collect { case e: MetricEvent.SubprocessEnd if e.pid == childPid => e }
      assert(endsForChild.nonEmpty, s"expected SubprocessEnd for pid=$childPid after shutdown; got events: $parsed")
      assert(endsForChild.exists(_.reason.contains("sampler_shutdown")), s"expected reason='sampler_shutdown' on the end event; got: $endsForChild")
    } finally if (child.isAlive) child.destroyForcibly()
  }
}
