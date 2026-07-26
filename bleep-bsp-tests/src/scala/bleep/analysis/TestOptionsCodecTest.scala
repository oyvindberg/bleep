package bleep.analysis

import bleep.bsp.protocol.BleepBspProtocol
import io.circe.parser.decode
import io.circe.syntax._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

/** `TestOptions` rides on the BSP request as `dataKind: bleep-test-options`, and the daemon serving that request is long-lived and shared — routinely a
  * different bleep version from the client that connects to it. The decoder must therefore tolerate absent fields in BOTH directions, because the failure mode
  * is silent: `handleTest` degrades a decode failure to `TestOptions.empty`, which throws away that run's `--only`, `--exclude` and `--jvm-opt` and then runs
  * the wrong set of tests without saying so.
  */
class TestOptionsCodecTest extends AnyFunSuite with Matchers {
  private val full = BleepBspProtocol.TestOptions(
    jvmOptions = List("-Xmx2g"),
    testArgs = List("-oD"),
    only = List("MySuite"),
    exclude = List("SlowSuite"),
    includeTags = List("fast"),
    excludeTags = List("flaky"),
    flamegraph = true,
    env = Map("DATABASE_URL" -> "postgres://localhost/test", "AWS_REGION" -> "eu-north-1")
  )

  test("round-trips every field, env included") {
    BleepBspProtocol.TestOptions.decode(BleepBspProtocol.TestOptions.encode(full)) shouldBe Right(full)
  }

  test("env survives values that are awkward on the wire") {
    val awkward = full.copy(env = Map("EMPTY" -> "", "SPACES" -> "a b c", "QUOTES" -> """{"k":"v"}""", "UNICODE" -> "æøå", "NEWLINE" -> "a\nb"))
    BleepBspProtocol.TestOptions.decode(BleepBspProtocol.TestOptions.encode(awkward)) shouldBe Right(awkward)
  }

  /** What a client predating `env` puts on the wire: the same object with that key absent. */
  private def withoutField(name: String): String =
    full.asJson.hcursor.withFocus(_.mapObject(_.remove(name))).top.get.noSpaces

  test("an older client that omits `env` still has all its OTHER options honored") {
    decode[BleepBspProtocol.TestOptions](withoutField("env")) shouldBe Right(full.copy(env = Map.empty))
  }

  test("every field is individually optional, so the next addition stays non-breaking too") {
    List("jvmOptions", "testArgs", "only", "exclude", "includeTags", "excludeTags", "flamegraph", "env").foreach { field =>
      withClue(s"omitting $field: ") {
        decode[BleepBspProtocol.TestOptions](withoutField(field)) shouldBe a[Right[?, ?]]
      }
    }
  }

  test("an empty object decodes to empty rather than failing") {
    decode[BleepBspProtocol.TestOptions]("{}") shouldBe Right(BleepBspProtocol.TestOptions.empty)
  }

  test("unknown fields from a NEWER client are ignored, not fatal") {
    val fromFuture = full.asJson.hcursor.withFocus(_.mapObject(_.add("retryFlaky", io.circe.Json.True))).top.get.noSpaces
    decode[BleepBspProtocol.TestOptions](fromFuture) shouldBe Right(full)
  }
}
