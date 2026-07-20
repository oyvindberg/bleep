package bleep.analysis

import bleep.bsp.protocol.{BleepBspProtocol, SuiteOutcome}
import bleep.model.{CrossProjectName, ProjectName, SuiteName}
import io.circe.parser.decode
import io.circe.syntax._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

/** The `SuiteFinished` wire format must survive version skew across the addition of `outcome`: a long-lived server and a newer client (or vice versa) have to
  * keep talking. Regression for the `DecodingFailure at .data.outcome` a new client hit against an older server.
  */
class SuiteFinishedCodecTest extends AnyFunSuite with Matchers {
  private val proj = CrossProjectName(ProjectName("dquery"), None)
  private val suite = SuiteName("ai.datoria.dquery.test.IdentityTestsDoris")

  private def sf(outcome: SuiteOutcome): BleepBspProtocol.Event.SuiteFinished =
    BleepBspProtocol.Event.SuiteFinished(proj, suite, outcome, durationMs = 42, timestamp = 100)

  test("round-trips every outcome variant through the full Event codec") {
    val outcomes = List(
      SuiteOutcome.Executed(3, 1, 2, 0),
      SuiteOutcome.Empty,
      SuiteOutcome.NoFrameworkMatched,
      SuiteOutcome.Errored("boom", Some("stack"))
    )
    outcomes.foreach { o =>
      val event: BleepBspProtocol.Event = sf(o)
      BleepBspProtocol.decode(BleepBspProtocol.encode(event)) shouldBe Right(event)
    }
  }

  test("encodes flat counts alongside outcome (so a client predating `outcome` still reads them)") {
    val json = (sf(SuiteOutcome.Executed(3, 1, 2, 4)): BleepBspProtocol.Event).asJson.hcursor.downField("data")
    json.get[Int]("passed") shouldBe Right(3)
    json.get[Int]("failed") shouldBe Right(1)
    json.get[Int]("skipped") shouldBe Right(2)
    json.get[Int]("ignored") shouldBe Right(4)
  }

  /** The JSON a pre-`outcome` server emits: same shape, but with the `outcome` field removed from `data` (leaving the flat counts). Built by stripping a real
    * encode so project/suite shapes are exactly right.
    */
  private def legacyJson(counts: SuiteOutcome.Executed): String = {
    val full = (sf(counts): BleepBspProtocol.Event).asJson
    full.hcursor.downField("data").withFocus(_.mapObject(_.remove("outcome"))).top.get.noSpaces
  }

  test("decodes a legacy message with flat counts and NO outcome (older server)") {
    decode[BleepBspProtocol.Event](legacyJson(SuiteOutcome.Executed(1, 0, 0, 0))) match {
      case Right(e: BleepBspProtocol.Event.SuiteFinished) => e.outcome shouldBe SuiteOutcome.Executed(1, 0, 0, 0)
      case other                                          => fail(s"expected SuiteFinished, got $other")
    }
  }

  test("decodes a legacy zero-count message as Empty rather than crashing") {
    decode[BleepBspProtocol.Event](legacyJson(SuiteOutcome.Executed(0, 0, 0, 0))) match {
      case Right(e: BleepBspProtocol.Event.SuiteFinished) => e.outcome shouldBe SuiteOutcome.Empty
      case other                                          => fail(s"expected SuiteFinished, got $other")
    }
  }

  /** The JSON a FUTURE bleep would emit: an `outcome` naming a variant this version has never heard of. Tolerance has to run in this direction too, or the day
    * a variant is added every older client hits `DecodingFailure at .data.outcome` all over again.
    */
  private def futureVariantJson(counts: SuiteOutcome.Executed): String = {
    val full = (sf(counts): BleepBspProtocol.Event).asJson
    full.hcursor
      .downField("data")
      .downField("outcome")
      .withFocus(_.mapObject(_.add("kind", io.circe.Json.fromString("flakyRetried"))))
      .top
      .get
      .noSpaces
  }

  test("an unknown outcome variant from a NEWER bleep falls back to the flat counts") {
    decode[BleepBspProtocol.Event](futureVariantJson(SuiteOutcome.Executed(7, 2, 0, 0))) match {
      case Right(e: BleepBspProtocol.Event.SuiteFinished) => e.outcome shouldBe SuiteOutcome.Executed(7, 2, 0, 0)
      case other                                          => fail(s"expected SuiteFinished, got $other")
    }
  }

  test("an unknown outcome variant with no counts reports Errored, never a silently-empty suite") {
    decode[BleepBspProtocol.Event](futureVariantJson(SuiteOutcome.Executed(0, 0, 0, 0))) match {
      case Right(e: BleepBspProtocol.Event.SuiteFinished) =>
        e.outcome shouldBe a[SuiteOutcome.Errored]
        e.outcome.asInstanceOf[SuiteOutcome.Errored].message should include("flakyRetried")
      case other => fail(s"expected SuiteFinished, got $other")
    }
  }
}
