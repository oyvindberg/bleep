package bleep.model

import io.circe.{Decoder, Encoder}

/** Represents a Kotlin version with associated compiler and library dependencies.
  *
  * Similar to [[VersionScala]], this provides convenient access to the compiler and standard library artifacts for a given Kotlin version.
  */
case class VersionKotlin(kotlinVersion: String) {
  require(kotlinVersion.nonEmpty)

  val kotlinOrganization: String = "org.jetbrains.kotlin"

  /** The Kotlin compiler dependency */
  val compiler: Dep =
    Dep.Java(kotlinOrganization, "kotlin-compiler", kotlinVersion)

  /** The Kotlin compiler runner dependency (for incremental compilation) */
  val compilerRunner: Dep =
    Dep.Java(kotlinOrganization, "kotlin-compiler-runner", kotlinVersion)

  /** The Kotlin standard library dependency */
  val library: Dep =
    Dep.Java(kotlinOrganization, "kotlin-stdlib", kotlinVersion)

  /** The Kotlin reflection library dependency */
  val reflect: Dep =
    Dep.Java(kotlinOrganization, "kotlin-reflect", kotlinVersion)

  /** The Kotlin metadata JVM library (needed for class analysis) */
  val metadataJvm: Dep =
    Dep.Java(kotlinOrganization, "kotlin-metadata-jvm", kotlinVersion)

  /** Binary version for artifact naming (e.g., "2.0", "2.1") */
  val binVersion: String = kotlinVersion match {
    case VersionKotlin.Version(major, minor, _) => s"$major.$minor"
    case other                                  => other
  }

  /** Major version (e.g., "2" for "2.0.0") */
  val majorVersion: String = kotlinVersion match {
    case VersionKotlin.Version(major, _, _) => major
    case _                                  => "2"
  }

  /** Check if this is Kotlin 2.x */
  def isK2: Boolean = kotlinVersion.startsWith("2.")

  /** Check if this is Kotlin 1.x */
  def isK1: Boolean = kotlinVersion.startsWith("1.")

  /** Libraries needed for compilation */
  val libraries: List[Dep] = List(library)
}

object VersionKotlin {
  implicit val decodes: Decoder[VersionKotlin] = Decoder[String].map(VersionKotlin.apply)
  implicit val encodes: Encoder[VersionKotlin] = Encoder[String].contramap(_.kotlinVersion)

  val Version = "(\\d+).(\\d+).(\\d+).*".r

  implicit val ordering: Ordering[VersionKotlin] = Ordering.by(_.kotlinVersion)

  val Kotlin2 = VersionKotlin("2.0.0")
  val Kotlin21 = VersionKotlin("2.1.0")
  val Kotlin23 = VersionKotlin("2.3.0")
}
