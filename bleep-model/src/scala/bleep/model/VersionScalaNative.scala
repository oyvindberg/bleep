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

  /** Makes the compiler plugin record source positions in NIR relative to the source directory a file is in, instead of as absolute paths, so the NIR (and a
    * binary linked from it) doesn't depend on where the build is checked out. sbt-scala-native passes all source directories of a project the same way
    * (`ScalaNativePluginInternal.scalaNativeConfigSettings`). The plugin knows the option from 0.5.0
    */
  def positionRelativizationPaths(sourceDirs: Iterable[java.nio.file.Path]): Option[Options.Opt] =
    if (majorVersionNum < 0.5) None
    else Some(Options.Opt.Flag(VersionScalaNative.PositionRelativizationPathsPrefix + sourceDirs.map(_.toString).toList.distinct.sorted.mkString(";")))
}

object VersionScalaNative {
  val org = Organization("org.scala-native")
  val ScalaNative05 = VersionScalaNative(Versions.ScalaNative05)

  val PositionRelativizationPathsPrefix = "-P:scalanative:positionRelativizationPaths:"

  /** The option [[VersionScalaNative.positionRelativizationPaths]] adds, which an imported build carries but bleep adds itself */
  def isPositionRelativizationPaths(opt: Options.Opt): Boolean =
    opt.render.headOption.exists(_.startsWith(PositionRelativizationPathsPrefix))

  implicit val decodesScalaNativeVersion: Decoder[VersionScalaNative] = Decoder[String].map(VersionScalaNative.apply)
  implicit val encodesScalaNativeVersion: Encoder[VersionScalaNative] = Encoder[String].contramap(_.scalaNativeVersion)
}
