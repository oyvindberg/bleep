package bleep.model

import io.circe.*
import io.circe.syntax.*

/** Encodes legal combinations of scala versions and platform versions */
sealed trait VersionCombo {
  def asScala: Option[VersionCombo.Scala] = this match {
    case scala: VersionCombo.Scala => Some(scala)
    case _                         => None
  }
  def asJava: Option[VersionCombo.Java.type] = this match {
    case VersionCombo.Java => Some(VersionCombo.Java)
    case _                 => None
  }

  def libraries(isTest: Boolean): List[Dep] =
    this match {
      case VersionCombo.Java              => Nil
      case VersionCombo.Jvm(scalaVersion) => scalaVersion.libraries
      case VersionCombo.Js(scalaVersion, scalaJs) =>
        val testLibs = if (isTest) List(scalaJs.testInterface, scalaJs.testBridge) else Nil

        if (scalaVersion.is3) List(scalaVersion.libraries, List(scalaJs.library3, scalaVersion.scala3JsLibrary), testLibs).flatten
        else List(scalaVersion.libraries, List(scalaJs.library), testLibs).flatten

      case VersionCombo.Native(scalaVersion, scalaNative) =>
        val testLibs = if (isTest) Some(scalaNative.testInterface) else None
        val libs = if (scalaVersion.is3) scalaNative.scala3Lib(scalaVersion.scalaVersion) else scalaNative.scalaLib
        List(libs, scalaVersion.library) ++ testLibs.toList
    }

  val compilerPlugin: Option[Dep]
  val compilerOptions: Options
}

object VersionCombo {

  implicit val encoder: Encoder[VersionCombo] =
    Encoder.instance {
      case Java =>
        Json.obj("Java" -> Json.Null)
      case Jvm(scalaVersion) =>
        Json.obj("Jvm" := Json.obj("scalaVersion" := scalaVersion))
      case Js(scalaVersion, scalaJsVersion) =>
        Json.obj("Js" := Json.obj("scalaVersion" := scalaVersion, "scalaJsVersion" := scalaJsVersion))
      case Native(scalaVersion, scalaNative) =>
        Json.obj("Native" := Json.obj("scalaVersion" := scalaVersion, "scalaNative" := scalaNative))
    }

  implicit val decoder: Decoder[VersionCombo] =
    (c: HCursor) =>
      c.keys.flatMap(_.headOption) match {
        case Some("Java") =>
          Right(Java)
        case Some("Jvm") =>
          c.downField("Jvm").downField("scalaVersion").as[VersionScala].map(Jvm.apply)
        case Some("Js") =>
          for {
            scalaVersion <- c.downField("Js").downField("scalaVersion").as[VersionScala]
            scalaJsVersion <- c.downField("Js").downField("scalaJsVersion").as[VersionScalaJs]
          } yield Js(scalaVersion, scalaJsVersion)
        case Some("Native") =>
          for {
            scalaVersion <- c.downField("Native").downField("scalaVersion").as[VersionScala]
            scalaNative <- c.downField("Native").downField("scalaNative").as[VersionScalaNative]
          } yield Native(scalaVersion, scalaNative)
        case _ =>
          Left(DecodingFailure("expected object with one of `Java`, `Jvm`, `Js` or `Native` keys", c.history))
      }

  case object Java extends VersionCombo {
    override val compilerPlugin: Option[Dep] = None
    override val compilerOptions: Options = Options.empty
  }

  sealed trait Scala extends VersionCombo {
    def scalaVersion: VersionScala
    def asJvm: Jvm = Jvm(scalaVersion)
  }

  case class Jvm(scalaVersion: VersionScala) extends Scala {
    override val compilerPlugin: Option[Dep] = None
    override val compilerOptions: Options = Options.empty
  }

  case class Js(scalaVersion: VersionScala, scalaJsVersion: VersionScalaJs) extends Scala {
    override val compilerPlugin: Option[Dep] =
      if (scalaVersion.is3) None else Some(scalaJsVersion.compilerPlugin)

    override val compilerOptions: Options =
      if (scalaVersion.is3) Options.parse(List("-scalajs"), None)
      else Options.empty
  }

  case class Native(scalaVersion: VersionScala, scalaNative: VersionScalaNative) extends Scala {
    override val compilerPlugin: Option[Dep] =
      Some(scalaNative.compilerPlugin)

    override val compilerOptions: Options =
      Options.empty
  }

  def fromExplodedScalaAndPlatform(maybeScala: Option[VersionScala], maybePlatform: Option[Platform]): Either[String, VersionCombo] =
    maybeScala match {
      case Some(scalaVersion) =>
        maybePlatform match {
          case Some(Platform.Jvm(_)) =>
            Right(Jvm(scalaVersion))
          case Some(Platform.Js(platform)) =>
            platform.jsVersion match {
              case Some(scalaJsVersion) =>
                Right(Js(scalaVersion, scalaJsVersion))
              case None =>
                Left(s"Must specify scala.js version for scala ${scalaVersion.scalaVersion}")
            }
          case Some(Platform.Native(platform)) =>
            platform.nativeVersion match {
              case Some(scalaNativeVersion) => Right(Native(scalaVersion, scalaNativeVersion))
              case None                     => Left(s"Must specify scala native version for scala ${scalaVersion.scalaVersion}")
            }
          case _ => Left("Must specify platform")
        }

      case None =>
        maybePlatform match {
          case Some(Platform.Jvm(_)) | None => Right(Java)
          case Some(platform)               => Left(s"Must specify scala version to use platform ${platform.name}")
        }
    }

  def fromExplodedProject(p: Project): Either[String, VersionCombo] =
    fromExplodedScalaAndPlatform(p.scala.flatMap(_.version), p.platform)
}
