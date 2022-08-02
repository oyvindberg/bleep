package bleep

import coursier.core.ModuleName

/** Encodes legal combinations of scala versions and platform versions */
sealed trait VersionScalaPlatform {
  final def platformSuffix(forceJvm: Boolean): Option[String] =
    if (forceJvm) Some("")
    else
      this match {
        case VersionScalaPlatform.Java                   => None
        case VersionScalaPlatform.Jvm(_)                 => Some("")
        case VersionScalaPlatform.Js(_, scalaJsVersion)  => Some(s"_sjs${scalaJsVersion.scalaJsBinVersion}")
        case VersionScalaPlatform.Native(_, scalaNative) => Some(s"_native${scalaNative.scalaNativeBinVersion}")
      }

  final def scalaSuffix(needsScala: Boolean, needsFullCrossVersion: Boolean, for3Use213: Boolean, for213use3: Boolean): Option[String] =
    this match {
      case scala: VersionScalaPlatform.WithScala if needsFullCrossVersion =>
        Some("_" + scala.scalaVersion.scalaVersion)
      case scala: VersionScalaPlatform.WithScala if for3Use213 && scala.scalaVersion.is3 =>
        Some("_2.13")
      case scala: VersionScalaPlatform.WithScala if for213use3 && scala.scalaVersion.binVersion == "2.13" =>
        Some("_3")
      case scala: VersionScalaPlatform.WithScala if needsScala =>
        Some("_" + scala.scalaVersion.binVersion)
      case VersionScalaPlatform.Java if !needsScala =>
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
      case VersionScalaPlatform.Java              => Nil
      case VersionScalaPlatform.Jvm(scalaVersion) => scalaVersion.libraries
      case VersionScalaPlatform.Js(scalaVersion, scalaJs) =>
        val testLibs = if (isTest) List(scalaJs.testInterface, scalaJs.testBridge) else Nil

        if (scalaVersion.is3) List(scalaVersion.libraries, List(scalaJs.library3, scalaVersion.scala3JsLibrary), testLibs).flatten
        else List(scalaVersion.libraries, List(scalaJs.library), testLibs).flatten

      case VersionScalaPlatform.Native(_, _) => Nil
    }

  def compilerPlugin: Option[Dep]
}

object VersionScalaPlatform {
  case object Java extends VersionScalaPlatform {
    override def compilerPlugin: Option[Dep] = None
  }

  sealed trait WithScala extends VersionScalaPlatform {
    def scalaVersion: VersionScala
    def asJvm: Jvm = Jvm(scalaVersion)
  }

  case class Jvm(scalaVersion: VersionScala) extends WithScala {
    override def compilerPlugin: Option[Dep] = None
  }
  case class Js(scalaVersion: VersionScala, scalaJsVersion: VersionScalaJs) extends WithScala {
    override def compilerPlugin: Option[Dep] =
      if (scalaVersion.is3) None else Some(scalaJsVersion.compilerPlugin)
  }
  case class Native(scalaVersion: VersionScala, scalaNative: VersionScalaNative) extends WithScala {
    override def compilerPlugin: Option[Dep] =
      Some(scalaNative.compilerPlugin)
  }

  def fromExplodedScalaAndPlatform(maybeScala: Option[VersionScala], maybePlatform: Option[model.Platform]): Either[String, VersionScalaPlatform] =
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

  def fromExplodedProject(p: model.Project): Either[String, VersionScalaPlatform] =
    fromExplodedScalaAndPlatform(p.scala.flatMap(_.version), p.platform)
}
