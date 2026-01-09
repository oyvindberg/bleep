package bleep.model

import coursier.core.{ModuleName, Organization}
import io.circe.{Decoder, Encoder}

case class VersionScalaJs(scalaJsVersion: String) {
  require(scalaJsVersion.nonEmpty)

  val scalaJsBinVersion: String =
    scalaJsVersion match {
      case VersionScala.Version("1", _, _)   => "1"
      case VersionScala.Version("0", "6", _) => "0.6"
      case other                             => other
    }

  val compilerPlugin: Dep.ScalaDependency =
    Dep.ScalaDependency(VersionScalaJs.org, ModuleName("scalajs-compiler"), scalaJsVersion, fullCrossVersion = true)
  val library: Dep.ScalaDependency =
    Dep.ScalaDependency(VersionScalaJs.org, ModuleName("scalajs-library"), scalaJsVersion, fullCrossVersion = false, forceJvm = true)

  // this does look funky. it does come from sbt with the suffix already baked in, so let's roll with that for now
  val library3: Dep =
    Dep.JavaDependency(VersionScalaJs.org, ModuleName("scalajs-library_2.13"), scalaJsVersion)
  val testInterface: Dep.ScalaDependency =
    Dep.ScalaDependency(VersionScalaJs.org, ModuleName("scalajs-test-interface"), scalaJsVersion, fullCrossVersion = false, forceJvm = true, for3Use213 = true)
  val testBridge: Dep.ScalaDependency =
    Dep.ScalaDependency(VersionScalaJs.org, ModuleName("scalajs-test-bridge"), scalaJsVersion, fullCrossVersion = false, forceJvm = true, for3Use213 = true)
}

object VersionScalaJs {
  val org = Organization("org.scala-js")
  val ScalaJs1 = VersionScalaJs("1.20.2")

  implicit val decodes: Decoder[VersionScalaJs] = Decoder[String].map(VersionScalaJs.apply)
  implicit val encodes: Encoder[VersionScalaJs] = Encoder[String].contramap(_.scalaJsVersion)
}
