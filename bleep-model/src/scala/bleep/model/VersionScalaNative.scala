package bleep.model

import coursier.core.{ModuleName, Organization}
import io.circe.{Decoder, Encoder}

case class VersionScalaNative(scalaNativeVersion: String) {
  def scalaNativeBinVersion: String =
    scalaNativeVersion match {
      case VersionScala.Version("1", _, _) => "1"
      case VersionScala.Version("0", x, _) => s"0.$x"
      case other                           => other
    }

  def majorVersionNum: Double =
    scalaNativeVersion.take(3).toDouble

  def compilerPlugin: Dep =
    Dep.ScalaDependency(VersionScalaNative.org, ModuleName("nscplugin"), scalaNativeVersion, fullCrossVersion = true)

  val testInterface = Dep.ScalaDependency(VersionScalaNative.org, ModuleName("test-interface"), scalaNativeVersion, fullCrossVersion = false)
}

object VersionScalaNative {
  val org = Organization("org.scala-native")
  val ScalaNative05 = VersionScalaNative("0.5.6")

  implicit val decodesScalaNativeVersion: Decoder[VersionScalaNative] = Decoder[String].map(VersionScalaNative.apply)
  implicit val encodesScalaNativeVersion: Encoder[VersionScalaNative] = Encoder[String].contramap(_.scalaNativeVersion)
}
