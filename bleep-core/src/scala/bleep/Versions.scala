package bleep

import coursier.core.{ModuleName, Organization}

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

    val scala3Library: Option[Dep] =
      if (is3) Some(Dep.Scala(scalaOrganization, "scala3-library", scalaVersion))
      else None

    // this does look funky. it does come from sbt with the suffix already baked in, so let's roll with that for now
    val scala3JsLibrary: Dep.ScalaDependency =
      Dep.ScalaDependency(Organization(scalaOrganization), ModuleName("scala3-library_sjs1"), scalaVersion, fullCrossVersion = false, forceJvm = true)

    val libraries: List[Dep] =
      List(Some(library), scala3Library).flatten

    val binVersion: String = scalaVersion match {
      case Version("3", _, _)     => s"3"
      case Version("2", minor, _) => s"2.$minor"
      case other                  => other
    }
  }

  val Scala212 = Scala("2.12.15")
  val Scala213 = Scala("2.13.6")
  val Scala3 = Scala("3.1.1")

  val scalaJsOrganization = Organization("org.scala-js")

  case class ScalaJs(scalaJsVersion: String) {
    require(scalaJsVersion.nonEmpty)

    val scalaJsBinVersion: String =
      scalaJsVersion match {
        case Version("1", _, _)   => "1"
        case Version("0", "6", _) => "0.6"
        case other                => other
      }

    val compilerPlugin: Dep.ScalaDependency =
      Dep.ScalaDependency(scalaJsOrganization, ModuleName("scalajs-compiler"), scalaJsVersion, fullCrossVersion = true)
    val library: Dep.ScalaDependency =
      Dep.ScalaDependency(scalaJsOrganization, ModuleName("scalajs-library"), scalaJsVersion, fullCrossVersion = false, forceJvm = true)

    // this does look funky. it does come from sbt with the suffix already baked in, so let's roll with that for now
    val library3: Dep =
      Dep.JavaDependency(scalaJsOrganization, ModuleName("scalajs-library_2.13"), scalaJsVersion)
    val testInterface: Dep.ScalaDependency =
      Dep.ScalaDependency(
        scalaJsOrganization,
        ModuleName("scalajs-test-interface"),
        scalaJsVersion,
        fullCrossVersion = false,
        forceJvm = true,
        for3Use213 = true
      )
    val testBridge: Dep.ScalaDependency =
      Dep.ScalaDependency(scalaJsOrganization, ModuleName("scalajs-test-bridge"), scalaJsVersion, fullCrossVersion = false, forceJvm = true, for3Use213 = true)
  }

  val ScalaJs1 = ScalaJs("1.9.0")
  val Node = "18.4.0"

  case class ScalaNative(scalaNativeVersion: String) {
    val scalaNativeOrg = Organization("org.scala-native")

    val scalaNativeBinVersion: String =
      scalaNativeVersion match {
        case Version("1", _, _) => "1"
        case Version("0", x, _) => s"0.$x"
        case other              => other
      }

    def compilerPlugin: Dep =
      Dep.ScalaDependency(scalaNativeOrg, ModuleName("nscplugin"), scalaNativeVersion, fullCrossVersion = true)
  }
}
