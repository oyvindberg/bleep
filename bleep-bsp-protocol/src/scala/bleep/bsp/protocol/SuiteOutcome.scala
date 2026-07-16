package bleep.bsp.protocol

import io.circe._
import io.circe.syntax._

/** The terminal outcome of running one test suite.
  *
  * Replaces the old `(passed, failed, skipped, ignored)` tuple, whose all-zero value meant three unrelated things at once — a suite with no tests, a suite
  * whose tests never ran, and an errored suite — all of which a caller then had to disambiguate with ad-hoc count arithmetic. The distinction is only knowable
  * at the forked runner (it alone sees whether the framework produced tests and whether they executed), so it is captured there and carried verbatim to the
  * summary.
  *
  * Note the level: this is about ONE suite. A whole project with zero *discovered* suites is a success and never produces a SuiteOutcome at all — that case is
  * the absence of suites, not an [[SuiteOutcome.Empty]] suite.
  */
sealed trait SuiteOutcome {

  /** True when this suite must not count as a passing suite: it failed tests, ran nothing, matched no framework, or errored. */
  def isFailure: Boolean = this match {
    case e: SuiteOutcome.Executed => e.failed > 0
    case _                        => true // Empty, NoFrameworkMatched, Errored are all failures
  }

  // Count accessors — non-zero only for Executed. Convenience for display/report sites that aggregate.
  def passedCount: Int = this match { case e: SuiteOutcome.Executed => e.passed; case _ => 0 }
  def failedCount: Int = this match { case e: SuiteOutcome.Executed => e.failed; case _ => 0 }
  def skippedCount: Int = this match { case e: SuiteOutcome.Executed => e.skipped; case _ => 0 }
  def ignoredCount: Int = this match { case e: SuiteOutcome.Executed => e.ignored; case _ => 0 }
}

object SuiteOutcome {

  /** The framework executed tests; counts are authoritative. By construction `passed + failed + skipped + ignored > 0` — a completed suite that produced no
    * test events is [[Empty]], not `Executed(0,0,0,0)`.
    */
  case class Executed(passed: Int, failed: Int, skipped: Int, ignored: Int) extends SuiteOutcome

  /** The framework recognized the suite class but it registered/ran zero tests (an empty suite). A defect, not a pass. */
  case object Empty extends SuiteOutcome

  /** No engine claimed the suite class — e.g. a JUnit 4 (`@org.junit.Test`) class with no junit-vintage-engine on the test classpath. A misconfiguration. */
  case object NoFrameworkMatched extends SuiteOutcome

  /** The suite could not run to completion: a class-load/link error, or a Throwable escaped the framework's task execution. */
  case class Errored(message: String, throwable: Option[String]) extends SuiteOutcome

  /** Build an outcome from bare counts. Used by runners that only produce counts (Scala.js/Native/Kotlin): all-zero means the suite ran nothing → [[Empty]],
    * never a silent pass.
    */
  def fromCounts(passed: Int, failed: Int, skipped: Int, ignored: Int): SuiteOutcome =
    if (passed + failed + skipped + ignored == 0) Empty
    else Executed(passed, failed, skipped, ignored)

  /** Wire discriminator string for an outcome — the inverse of the `kind` field [[fromWire]] reads. */
  def tagOf(o: SuiteOutcome): String = o match {
    case _: Executed        => "executed"
    case Empty              => "empty"
    case NoFrameworkMatched => "noFrameworkMatched"
    case _: Errored         => "errored"
  }

  /** Parse the forked runner's wire discriminator (see `TestProtocol.encodeSuite*`). Counts are only consulted for `"executed"`. */
  def fromWire(kind: String, passed: Int, failed: Int, skipped: Int, ignored: Int, message: Option[String], throwable: Option[String]): SuiteOutcome =
    kind match {
      case "executed"           => Executed(passed, failed, skipped, ignored)
      case "empty"              => Empty
      case "noFrameworkMatched" => NoFrameworkMatched
      case "errored"            => Errored(message.getOrElse("suite errored"), throwable)
      case other                => Errored(s"unknown suite outcome '$other'", None)
    }

  implicit val encoder: Encoder[SuiteOutcome] = Encoder.instance {
    case e: Executed =>
      Json.obj(
        "kind" -> "executed".asJson,
        "passed" -> e.passed.asJson,
        "failed" -> e.failed.asJson,
        "skipped" -> e.skipped.asJson,
        "ignored" -> e.ignored.asJson
      )
    case Empty              => Json.obj("kind" -> "empty".asJson)
    case NoFrameworkMatched => Json.obj("kind" -> "noFrameworkMatched".asJson)
    case e: Errored         => Json.obj("kind" -> "errored".asJson, "message" -> e.message.asJson, "throwable" -> e.throwable.asJson)
  }

  implicit val decoder: Decoder[SuiteOutcome] = Decoder.instance { cursor =>
    cursor.downField("kind").as[String].flatMap {
      case "executed" =>
        for {
          p <- cursor.downField("passed").as[Int]
          f <- cursor.downField("failed").as[Int]
          s <- cursor.downField("skipped").as[Int]
          i <- cursor.downField("ignored").as[Int]
        } yield Executed(p, f, s, i)
      case "empty"              => Right(Empty)
      case "noFrameworkMatched" => Right(NoFrameworkMatched)
      case "errored"            =>
        for {
          m <- cursor.downField("message").as[String]
          t <- cursor.downField("throwable").as[Option[String]]
        } yield Errored(m, t)
      case other => Left(DecodingFailure(s"Unknown SuiteOutcome kind: $other", cursor.history))
    }
  }

  implicit val codec: Codec[SuiteOutcome] = Codec.from(decoder, encoder)
}
