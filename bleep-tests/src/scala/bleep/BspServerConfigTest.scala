package bleep

import bleep.model.BspServerConfig
import io.circe.parser.decode
import io.circe.syntax.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class BspServerConfigTest extends AnyFunSuite with Matchers {

  test("an absent bspReadTimeoutMinutes decodes to the default") {
    val config = decode[BspServerConfig]("""{}""").fold(throw _, identity)
    config.bspReadTimeoutMinutes shouldBe None
    config.effectiveBspReadTimeoutMillis shouldBe BspServerConfig.DefaultBspReadTimeoutMinutes * 60 * 1000
  }

  test("bspReadTimeoutMinutes is read from config and converted to millis") {
    val config = decode[BspServerConfig]("""{"bspReadTimeoutMinutes": 5}""").fold(throw _, identity)
    config.bspReadTimeoutMinutes shouldBe Some(5)
    config.effectiveBspReadTimeoutMillis shouldBe 300000
  }

  test("0 means wait forever — that is what setSoTimeout takes") {
    val config = decode[BspServerConfig]("""{"bspReadTimeoutMinutes": 0}""").fold(throw _, identity)
    config.effectiveBspReadTimeoutMillis shouldBe 0
  }

  test("a negative timeout is rejected rather than silently passed to setSoTimeout") {
    val config = decode[BspServerConfig]("""{"bspReadTimeoutMinutes": -1}""").fold(throw _, identity)
    a[RuntimeException] should be thrownBy config.effectiveBspReadTimeoutMillis
  }

  // Guards docs/guides/compile-servers.mdx, which tells people to hand-edit this file. The keys
  // are silently ignored when wrong, so a docs typo is invisible until someone wonders why their
  // setting does nothing — which is exactly how `bspServer:` survived in the docs for so long.
  test("the YAML shape documented in compile-servers.mdx actually decodes") {
    val documented =
      """{"compileServerMode":{"mode":"shared"},"bspServerConfig":{"compileServerMaxMemory":"4g","heapPressureThreshold":0.85,"testRunnerMaxMemory":"2g","bspReadTimeoutMinutes":30}}"""
    val config = decode[bleep.model.BleepConfig](documented).fold(throw _, identity)
    config.compileServerModeOrDefault shouldBe bleep.model.CompileServerMode.Shared
    val bsp = config.bspServerConfigOrDefault
    bsp.compileServerMaxMemory shouldBe Some("4g")
    bsp.heapPressureThreshold shouldBe Some(0.85)
    bsp.testRunnerMaxMemory shouldBe Some("2g")
    bsp.bspReadTimeoutMinutes shouldBe Some(30)
  }

  test("the key the docs used to name (bspServer) really is silently ignored") {
    val wrong = """{"bspServer":{"compileServerMaxMemory":"4g"}}"""
    val config = decode[bleep.model.BleepConfig](wrong).fold(throw _, identity)
    config.bspServerConfig shouldBe None
  }

  test("adding the field didn't break round-tripping an existing config") {
    val original = BspServerConfig.default.copy(parallelism = Some(4), bspReadTimeoutMinutes = Some(10))
    decode[BspServerConfig](original.asJson.noSpaces).fold(throw _, identity) shouldBe original
  }
}
