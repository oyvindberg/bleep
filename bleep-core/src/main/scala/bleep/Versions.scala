package bleep

import coursier.parse.JavaOrScalaDependency

object Versions {
  // this accepts any nightly or milestone with the same binversion as a major release. good enough for now
  private val Version = "(\\d+).(\\d+).(\\d+).*".r

  case class Scala(scalaVersion: String) {
    val is3 = scalaVersion.startsWith("3.")

    val epoch = scalaVersion.head

    val scalaOrganization: String =
      "org.scala-lang"

    val compiler: JavaOrScalaDependency =
      if (is3) Deps.Scala(scalaOrganization, "scala3-compiler", scalaVersion)
      else Deps.Java(scalaOrganization, "scala-compiler", scalaVersion)

    val library: JavaOrScalaDependency =
      if (is3) Scala213.library
      else Deps.Java(scalaOrganization, "scala-library", scalaVersion)

    val dottyLibrary: Option[JavaOrScalaDependency] =
      if (is3) Some(Deps.Java(scalaOrganization, "scala3-library", scalaVersion))
      else None

    val binVersion: String = scalaVersion match {
      case Version("3", _, _)     => s"3"
      case Version("2", minor, _) => s"2.$minor"
      case other                  => other
    }

    val compilerBridge: Option[JavaOrScalaDependency] =
      if (is3)
        Some(Deps.Java(scalaOrganization, "scala3-sbt-bridge", scalaVersion))
      else None
  }

  val Scala212 = Scala("2.12.14")
  val Scala213 = Scala("2.13.5")
  val Scala3 = Scala("3.0.2")

  case class ScalaJs(scalaJsVersion: String) {
    val scalaJsBinVersion: String =
      scalaJsVersion match {
        case Version("1", _, _)   => "1"
        case Version("0", "6", _) => "0.6"
        case other                => other
      }

    val scalaJsOrganization = "org.scala-js"
    val sbtPlugin = Deps.Scala(scalaJsOrganization, "sbt-scalajs", scalaJsVersion)
  }

  val ScalaJs1 = ScalaJs("1.7.0")
}
