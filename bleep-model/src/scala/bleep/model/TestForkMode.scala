package bleep
package model

import io.circe.{Decoder, DecodingFailure, Encoder}

/** Where a project's test suites run relative to the JVM that hosts them — the fork granularity.
  *
  *   - [[PerProject]] (the default) runs every one of the project's suites in a single forked JVM — maven surefire's `forkCount=1 reuseForks=true`. JVM-wide
  *     state carries across suites: a booted application and its dev-service containers, a shared Testcontainers instance, schema an earlier suite created.
  *     Suites run one at a time by default. `maxConcurrentSuites` only speeds up JUnit Platform suites: raising it lets the JUnit engine run that many of the
  *     project's JUnit classes at once inside the one fork. sbt-interface frameworks (ScalaTest, MUnit, utest, Specs2, ScalaCheck, weaver, …) always run
  *     sequentially in this mode — they share one `Runner` and have no lock-aware scheduler, so concurrency in a shared JVM is unsafe; use [[PerSuite]] for
  *     concurrent sbt suites.
  *   - [[PerSuite]] forks a JVM per suite, pooled by classpath. Suites are isolated by the operating system: one that calls `System.exit`, wedges a thread or
  *     corrupts a static kills a process bleep can replace. `maxConcurrentSuites` then bounds how many such forks run at once (unset = unbounded; the
  *     machine-wide governor still caps the total). This is how you run sbt suites concurrently — every framework, isolated.
  */
sealed abstract class TestForkMode(val value: String)

object TestForkMode {
  case object PerSuite extends TestForkMode("per-suite")
  case object PerProject extends TestForkMode("per-project")

  val All: List[TestForkMode] = List(PerSuite, PerProject)
  val byName: Map[String, TestForkMode] = All.map(x => x.value -> x).toMap

  def fromString(str: String): Either[String, TestForkMode] =
    byName.get(str).toRight(s"'$str' not among ${byName.keys.mkString(", ")}")

  implicit val decoder: Decoder[TestForkMode] =
    Decoder.instance { c =>
      c.as[String].flatMap(str => fromString(str).left.map(err => DecodingFailure(err, c.history)))
    }

  implicit val encoder: Encoder[TestForkMode] =
    Encoder.encodeString.contramap(_.value)
}
