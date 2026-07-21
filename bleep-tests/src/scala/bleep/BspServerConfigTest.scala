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

  test("adding the field didn't break round-tripping an existing config") {
    val original = BspServerConfig.default.copy(parallelism = Some(4), bspReadTimeoutMinutes = Some(10))
    decode[BspServerConfig](original.asJson.noSpaces).fold(throw _, identity) shouldBe original
  }
}
