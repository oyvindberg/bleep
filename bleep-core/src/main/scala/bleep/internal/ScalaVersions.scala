package bleep
package internal

import coursier.core.{Module, ModuleName}

/** Encodes legal combinations of scala versions and platform versions */
sealed trait ScalaVersions {

  final def hasPlatformSuffix: Boolean =
    platformSuffix(true).isDefined

  final def platformSuffix(needsPlatformSuffix: Boolean): Option[String] =
    if (!needsPlatformSuffix) Some("")
    else
      this match {
        case ScalaVersions.Java                        => None
        case ScalaVersions.Jvm(_)                      => None
        case ScalaVersions.Js(_, None)                 => Some(s"_sjs1")
        case ScalaVersions.Js(_, Some(scalaJsVersion)) => Some(s"_sjs${scalaJsVersion.scalaJsBinVersion}")
        case ScalaVersions.Native(_, scalaNative)      => Some(s"_native${scalaNative.scalaNativeBinVersion}")
      }

  final def scalaSuffix(needsScala: Boolean, needsFullCrossVersion: Boolean): Option[String] =
    this match {
      case scala: ScalaVersions.WithScala if needsFullCrossVersion =>
        Some("_" + scala.scalaVersion.scalaVersion)
      case scala: ScalaVersions.WithScala if needsScala =>
        Some("_" + scala.scalaVersion.binVersion)
      case ScalaVersions.Java =>
        if (needsFullCrossVersion || needsScala) None else Some("")
    }

  final def moduleName(baseModuleName: ModuleName, needsScala: Boolean, needsFullCrossVersion: Boolean, needsPlatformSuffix: Boolean): Option[ModuleName] =
    for {
      p <- platformSuffix(needsPlatformSuffix)
      s <- scalaSuffix(needsScala, needsFullCrossVersion)
    } yield baseModuleName.map(_ + p + s)
}

object ScalaVersions {
  case object Java extends ScalaVersions

  sealed trait WithScala extends ScalaVersions {
    def scalaVersion: Versions.Scala
  }
  case class Jvm(scalaVersion: Versions.Scala) extends WithScala
  case class Js(scalaVersion: Versions.Scala, maybeScalaJs: Option[Versions.ScalaJs]) extends WithScala
  case class Native(scalaVersion: Versions.Scala, scalaNative: Versions.ScalaNative) extends WithScala

  def fromExplodedScalaAndPlatform(maybeScala: Option[Versions.Scala], maybePlatform: Option[model.Platform]): Either[String, ScalaVersions] =
    maybeScala match {
      case Some(scalaVersion) =>
        maybePlatform match {
          case Some(_: model.Platform.Jvm) =>
            Right(Jvm(scalaVersion))
          case Some(x: model.Platform.Js) =>
            x.version match {
              case Some(scalaJsVersion) =>
                if (scalaVersion.is3) Left("Must not specify scala.js version for scala 3 - it's bundled")
                else Right(Js(scalaVersion, Some(scalaJsVersion)))
              case None =>
                if (scalaVersion.is3) Right(Js(scalaVersion, None))
                else Left(s"Must specify scala.js version for scala ${scalaVersion.scalaVersion}")
            }

          case Some(x: model.Platform.Native) =>
            x.version match {
              case Some(scalaNativeVersion) => Right(Native(scalaVersion, scalaNativeVersion))
              case None                     => Left(s"Must specify scala native version for scala ${scalaVersion.scalaVersion}")
            }
          case None => Left("Must specify platform")
        }

      case None =>
        maybePlatform match {
          case Some(platform) => Left(s"Must specify scala version to use platform ${platform.name}")
          case None           => Right(Java)
        }
    }

  def fromExplodedProject(p: model.Project): Either[String, ScalaVersions] =
    fromExplodedScalaAndPlatform(p.scala.flatMap(_.version), p.platform)
}
