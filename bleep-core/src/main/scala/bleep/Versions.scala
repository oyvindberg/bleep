package bleep

object Versions {
  // this accepts any nightly or milestone with the same binversion as a major release. good enough for now
  private val Version = "(\\d+).(\\d+).(\\d+).*".r

  case class Scala(scalaVersion: String) {
    require(scalaVersion.nonEmpty)

    val is3 = scalaVersion.startsWith("3.")

    val epoch = scalaVersion.head

    val scalaOrganization: String =
      "org.scala-lang"

    val compiler: Dep =
      if (is3) Dep.Scala(scalaOrganization, "scala3-compiler", scalaVersion)
      else Dep.Java(scalaOrganization, "scala-compiler", scalaVersion)

    val library: Dep =
      if (is3) Scala213.library
      else Dep.Java(scalaOrganization, "scala-library", scalaVersion)

    val dottyLibrary: Option[Dep] =
      if (is3) Some(Dep.Java(scalaOrganization, "scala3-library", scalaVersion))
      else None

    val binVersion: String = scalaVersion match {
      case Version("3", _, _)     => s"3"
      case Version("2", minor, _) => s"2.$minor"
      case other                  => other
    }

    val compilerBridge: Option[Dep] =
      if (is3) Some(Dep.Java(scalaOrganization, "scala3-sbt-bridge", scalaVersion))
      else None
  }

  val Scala212 = Scala("2.12.15")
  val Scala213 = Scala("2.13.8")
  val Scala3 = Scala("3.1.1")

  case class ScalaJs(scalaJsVersion: String) {
    require(scalaJsVersion.nonEmpty)

    val scalaJsBinVersion: String =
      scalaJsVersion match {
        case Version("1", _, _)   => "1"
        case Version("0", "6", _) => "0.6"
        case other                => other
      }

    val scalaJsOrganization = "org.scala-js"
    val sbtPlugin = Dep.Scala(scalaJsOrganization, "sbt-scalajs", scalaJsVersion)
    val compilerPlugin = Dep.ScalaFullVersion(scalaJsOrganization, "scalajs-compiler", scalaJsVersion)
  }

  val ScalaJs1 = ScalaJs("1.9.0")

  case class ScalaNative(scalaNativeVersion: String) {
    val scalaNativeBinVersion: String =
      scalaNativeVersion match {
        case Version("1", _, _) => "1"
        case Version("0", x, _) => s"0.$x"
        case other              => other
      }

    def compilerPlugin: Dep = sys.error("todo: implement")
  }
}
