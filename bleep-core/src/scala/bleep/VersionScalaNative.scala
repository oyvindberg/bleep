package bleep

import coursier.core.{ModuleName, Organization}

case class VersionScalaNative(scalaNativeVersion: String) {
  val scalaNativeBinVersion: String =
    scalaNativeVersion match {
      case VersionScala.Version("1", _, _) => "1"
      case VersionScala.Version("0", x, _) => s"0.$x"
      case other                           => other
    }

  def compilerPlugin: Dep =
    Dep.ScalaDependency(VersionScalaNative.org, ModuleName("nscplugin"), scalaNativeVersion, fullCrossVersion = true)
}

object VersionScalaNative {
  val org = Organization("org.scala-native")
}
