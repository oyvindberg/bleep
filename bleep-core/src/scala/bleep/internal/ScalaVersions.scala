package bleep
package internal

import coursier.core.ModuleName

/** Encodes legal combinations of scala versions and platform versions */
sealed trait ScalaVersions {
  final def platformSuffix(forceJvm: Boolean): Option[String] =
    if (forceJvm) Some("")
    else
      this match {
        case ScalaVersions.Java                   => None
        case ScalaVersions.Jvm(_)                 => Some("")
        case ScalaVersions.Js(_, scalaJsVersion)  => Some(s"_sjs${scalaJsVersion.scalaJsBinVersion}")
        case ScalaVersions.Native(_, scalaNative) => Some(s"_native${scalaNative.scalaNativeBinVersion}")
      }

  final def scalaSuffix(needsScala: Boolean, needsFullCrossVersion: Boolean, for3Use213: Boolean, for213use3: Boolean): Option[String] =
    this match {
      case scala: ScalaVersions.WithScala if needsFullCrossVersion =>
        Some("_" + scala.scalaVersion.scalaVersion)
      case scala: ScalaVersions.WithScala if for3Use213 && scala.scalaVersion.is3 =>
        Some("_2.13")
      case scala: ScalaVersions.WithScala if for213use3 && scala.scalaVersion.binVersion == "2.13" =>
        Some("_3")
      case scala: ScalaVersions.WithScala if needsScala =>
        Some("_" + scala.scalaVersion.binVersion)
      case ScalaVersions.Java if !needsScala =>
        Some("")
      case _ =>
        None
    }

  final def fullSuffix(needsScala: Boolean, needsFullCrossVersion: Boolean, forceJvm: Boolean, for3Use213: Boolean, for213use3: Boolean): Option[String] =
    for {
      p <- platformSuffix(forceJvm = forceJvm)
      s <- scalaSuffix(needsScala = needsScala, needsFullCrossVersion = needsFullCrossVersion, for3Use213 = for3Use213, for213use3 = for213use3)
    } yield p + s

  final def moduleName(
      baseModuleName: ModuleName,
      needsScala: Boolean,
      needsFullCrossVersion: Boolean,
      forceJvm: Boolean,
      for3Use213: Boolean,
      for213Use3: Boolean
  ): Option[ModuleName] =
    fullSuffix(needsScala = needsScala, needsFullCrossVersion = needsFullCrossVersion, forceJvm = forceJvm, for3Use213 = for3Use213, for213use3 = for213Use3)
      .map(s => baseModuleName.map(_ + s))

  def libraries(isTest: Boolean): Seq[Dep] =
    this match {
      case ScalaVersions.Java              => Nil
      case ScalaVersions.Jvm(scalaVersion) => scalaVersion.libraries
      case ScalaVersions.Js(scalaVersion, scalaJs) =>
        val testLibs = if (isTest) List(scalaJs.testInterface, scalaJs.testBridge) else Nil

        if (scalaVersion.is3) List(scalaVersion.libraries, List(scalaJs.library3, scalaVersion.scala3JsLibrary), testLibs).flatten
        else List(scalaVersion.libraries, List(scalaJs.library), testLibs).flatten

      case ScalaVersions.Native(_, _) => Nil
    }

  def compilerPlugin: Option[Dep]
}

object ScalaVersions {
  case object Java extends ScalaVersions {
    override def compilerPlugin: Option[Dep] = None
  }

  sealed trait WithScala extends ScalaVersions {
    def scalaVersion: Versions.Scala
    def asJvm: Jvm = Jvm(scalaVersion)
  }

  case class Jvm(scalaVersion: Versions.Scala) extends WithScala {
    override def compilerPlugin: Option[Dep] = None
  }
  case class Js(scalaVersion: Versions.Scala, scalaJsVersion: Versions.ScalaJs) extends WithScala {
    override def compilerPlugin: Option[Dep] =
      if (scalaVersion.is3) None else Some(scalaJsVersion.compilerPlugin)
  }
  case class Native(scalaVersion: Versions.Scala, scalaNative: Versions.ScalaNative) extends WithScala {
    override def compilerPlugin: Option[Dep] =
      Some(scalaNative.compilerPlugin)
  }

  def fromExplodedScalaAndPlatform(maybeScala: Option[Versions.Scala], maybePlatform: Option[model.Platform]): Either[String, ScalaVersions] =
    maybeScala match {
      case Some(scalaVersion) =>
        maybePlatform match {
          case Some(model.Platform.Jvm(_)) =>
            Right(Jvm(scalaVersion))
          case Some(model.Platform.Js(platform)) =>
            platform.jsVersion match {
              case Some(scalaJsVersion) =>
                Right(Js(scalaVersion, scalaJsVersion))
              case None =>
                Left(s"Must specify scala.js version for scala ${scalaVersion.scalaVersion}")
            }
          case Some(model.Platform.Native(platform)) =>
            platform.nativeVersion match {
              case Some(scalaNativeVersion) => Right(Native(scalaVersion, scalaNativeVersion))
              case None                     => Left(s"Must specify scala native version for scala ${scalaVersion.scalaVersion}")
            }
          case _ => Left("Must specify platform")
        }

      case None =>
        maybePlatform match {
          case Some(model.Platform.Jvm(_)) | None => Right(Java)
          case Some(platform)                     => Left(s"Must specify scala version to use platform ${platform.name}")
        }
    }

  def fromExplodedProject(p: model.Project): Either[String, ScalaVersions] =
    fromExplodedScalaAndPlatform(p.scala.flatMap(_.version), p.platform)
}
