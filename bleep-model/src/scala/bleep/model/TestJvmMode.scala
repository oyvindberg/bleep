package bleep
package model

import io.circe.{Decoder, DecodingFailure, Encoder}

/** Where a project's test suites run relative to the JVM that hosts them.
  *
  *   - [[PerSuite]] (the default) forks a JVM per suite, pooled by classpath. Suites are isolated by the operating system: one that calls `System.exit`, wedges
  *     a thread or corrupts a static kills a process bleep can replace. `testSuiteParallelism` bounds how many such forks run at once.
  *   - [[PerProject]] runs every one of the project's suites in a single forked JVM — maven surefire's `forkCount=1 reuseForks=true`. JVM-wide state carries
  *     across suites: a booted application and its dev-service containers, a shared Testcontainers instance, schema an earlier suite created.
  *     `testSuiteParallelism` then bounds how many of the project's suites run *concurrently inside that one fork* (`1` = sequential, the safe default for
  *     frameworks whose per-JVM state is a singleton, e.g. `@QuarkusTest`).
  */
sealed abstract class TestJvmMode(val value: String)

object TestJvmMode {
  case object PerSuite extends TestJvmMode("per-suite")
  case object PerProject extends TestJvmMode("per-project")

  val All: List[TestJvmMode] = List(PerSuite, PerProject)
  val byName: Map[String, TestJvmMode] = All.map(x => x.value -> x).toMap

  def fromString(str: String): Either[String, TestJvmMode] =
    byName.get(str).toRight(s"'$str' not among ${byName.keys.mkString(", ")}")

  implicit val decoder: Decoder[TestJvmMode] =
    Decoder.instance { c =>
      c.as[String].flatMap(str => fromString(str).left.map(err => DecodingFailure(err, c.history)))
    }

  implicit val encoder: Encoder[TestJvmMode] =
    Encoder.encodeString.contramap(_.value)
}
