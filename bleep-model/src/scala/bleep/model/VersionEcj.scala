package bleep.model

import io.circe.{Decoder, Encoder}

/** Eclipse Compiler for Java (ECJ) version.
  *
  * ECJ is an alternative to javac with:
  *   - Better error messages
  *   - Faster compilation in some cases
  *   - More lenient error handling (can compile partial code)
  */
case class VersionEcj(version: String) {
  require(version.nonEmpty, "ECJ version must not be empty")

  /** Maven coordinates for ECJ */
  val dependency: Dep = Dep.Java("org.eclipse.jdt", "ecj", version)
}

object VersionEcj {
  implicit val decodes: Decoder[VersionEcj] = Decoder[String].map(VersionEcj.apply)
  implicit val encodes: Encoder[VersionEcj] = Encoder[String].contramap(_.version)

  implicit val ordering: Ordering[VersionEcj] = Ordering.by(_.version)

  /** Recent stable ECJ version */
  val Default = VersionEcj("3.44.0")
}
