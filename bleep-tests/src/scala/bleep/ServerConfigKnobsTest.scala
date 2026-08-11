package bleep

import bleep.model.{BleepConfig, BspServerConfig}
import io.circe.parser.decode
import io.circe.syntax._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

/** Every knob `bleep server config` exposes must survive a round trip through the config file, and every `-clear` must actually put the default back.
  *
  * These are cheap to get wrong in a way nothing notices: an unknown key in this file is silently ignored, so a knob wired to a field that does not decode
  * looks like it worked and simply never takes effect. That is exactly how `bspServer:` survived in the docs for so long — see [[BspServerConfigTest]].
  */
class ServerConfigKnobsTest extends AnyFunSuite with Matchers {

  private def roundTrip(config: BspServerConfig): BspServerConfig = {
    val json = BleepConfig.default.copy(bspServerConfig = Some(config)).asJson.noSpaces
    decode[BleepConfig](json).fold(throw _, identity).bspServerConfigOrDefault
  }

  private val base = BspServerConfig.default

  test("every knob round-trips through the config file") {
    val allSet = base.copy(
      parallelism = Some(4),
      parallelismRatio = Some(0.5),
      compileServerMaxMemory = Some("4g"),
      maxCachedWorkspaces = Some(6),
      bspReadTimeoutMinutes = Some(15),
      compileServerIdleTimeoutMinutes = Some(120),
      heapPressureThreshold = Some(0.85),
      testRunnerHeap = Some("2g"),
      testIdleTimeoutMinutes = Some(5),
      sourcegenMaxMemory = Some("500m"),
      kspRunnerMaxMemory = Some("1500m")
    )

    roundTrip(allSet) shouldBe allSet
  }

  test("clearing a knob really removes it, rather than writing a default that looks like a choice") {
    val cleared = roundTrip(base.copy(parallelism = Some(4))).copy(parallelism = None)
    roundTrip(cleared).parallelism shouldBe None
  }

  test("the five knobs that had no command before are real fields, not typos") {
    // These could only be set by hand-editing YAML until `bleep server config` existed. A typo here would decode to None and be invisible.
    val config = roundTrip(
      base.copy(
        parallelismRatio = Some(0.25),
        heapPressureThreshold = Some(0.9),
        testIdleTimeoutMinutes = Some(7),
        sourcegenMaxMemory = Some("750m"),
        kspRunnerMaxMemory = Some("900m")
      )
    )

    config.parallelismRatio shouldBe Some(0.25)
    config.heapPressureThreshold shouldBe Some(0.9)
    config.testIdleTimeoutMinutes shouldBe Some(7)
    config.sourcegenMaxMemory shouldBe Some("750m")
    config.kspRunnerMaxMemory shouldBe Some("900m")
  }

  test("parallelism-ratio takes effect only while parallelism is unset, which is why -clear removes both") {
    val cores = Runtime.getRuntime.availableProcessors

    base.copy(parallelism = Some(3), parallelismRatio = Some(1.0)).effectiveParallelism shouldBe 3
    base.copy(parallelism = None, parallelismRatio = Some(1.0)).effectiveParallelism shouldBe cores
    base.copy(parallelism = None, parallelismRatio = None).effectiveParallelism shouldBe cores
  }

  test("the timeout knobs keep 0 meaning 'never', not 'immediately'") {
    base.copy(compileServerIdleTimeoutMinutes = Some(0)).effectiveCompileServerIdleTimeoutMillis shouldBe 0L
    base.copy(bspReadTimeoutMinutes = Some(0)).effectiveBspReadTimeoutMillis shouldBe 0
  }

  test("a negative timeout is refused loudly rather than wrapping into a huge one") {
    a[RuntimeException] should be thrownBy base.copy(compileServerIdleTimeoutMinutes = Some(-1)).effectiveCompileServerIdleTimeoutMillis
    a[RuntimeException] should be thrownBy base.copy(bspReadTimeoutMinutes = Some(-5)).effectiveBspReadTimeoutMillis
  }
}
