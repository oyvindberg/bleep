package bleep.model

import coursier.ModuleName
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
      case VersionCombo.Java                      => Nil
      case VersionCombo.Kotlin(kotlinVersion)     => kotlinVersion.libraries
      case VersionCombo.Jvm(scalaVersion)         => scalaVersion.libraries
      case VersionCombo.Js(scalaVersion, scalaJs) =>
        val testLibs = if (isTest) List(scalaJs.testInterface, scalaJs.testBridge) else Nil

        if (scalaVersion.is3) List(scalaVersion.libraries, List(scalaJs.library3, scalaVersion.scala3JsLibrary), testLibs).flatten
        else List(scalaVersion.libraries, List(scalaJs.library), testLibs).flatten

      case VersionCombo.Native(scalaVersion, scalaNative) =>
        val testLibs = if (isTest) Some(scalaNative.testInterface) else None

        val libs = {
          val version: String =
            if (scalaNative.majorVersionNum < 0.5) scalaNative.scalaNativeVersion
            else s"${scalaVersion.scalaVersion}+${scalaNative.scalaNativeVersion}"

          Dep.ScalaDependency(
            VersionScalaNative.org,
            ModuleName(if (scalaVersion.is3) "scala3lib" else "scalalib"),
            version,
            fullCrossVersion = false
          )
        }
        // javalib is explicitly excluded from scala3lib/scalalib POMs,
        // so it must be added separately for the linker classpath
        val javalib = Dep.ScalaDependency(
          VersionScalaNative.org,
          ModuleName("javalib"),
          scalaNative.scalaNativeVersion,
          fullCrossVersion = false
        )
        List(libs, scalaVersion.library, javalib) ++ testLibs.toList
    }

  /** Version schemes for the test-harness libraries [[libraries]] injects.
    *
    * bleep pins these to the project's own Scala.js / Scala Native version, because the linked artifact's test bridge and the JVM-side adapter that talks to it
    * have to come from one toolchain. A test framework published against an older patch of the same platform line brings its own copy along, and the eviction
    * check then fails the build over a conflict bleep created and the user cannot see: `test-interface_native0.5_3:0.5.12 (strict) is selected over 0.5.1`,
    * which is scalatest 3.2.19 against the [[Versions.ScalaNative05]] that `bleep build new` writes.
    *
    * `strict` there is not bleep's word: `org.scala-native` publishes `<info.versionScheme>strict</info.versionScheme>` in its POMs, so upstream's own position
    * is that no two versions of this artifact are interchangeable. Overriding that deserves a reason rather than a convenience, and the reason is the module
    * name. `test-interface_native0.5_3` can only ever hold a 0.5.x version — the binary line is in the artifact id, and both platforms keep compatibility
    * across a line — so `early-semver` describes this module exactly: differing patches within one line are compatible, and there is no version of this module
    * that early-semver would wave through but the platform would not. That is a narrower claim than `always`, which would also cover artifacts whose names
    * carry no such guarantee, and it is checkable rather than merely asserted.
    *
    * Scoped to the test libraries deliberately. It says nothing about ordinary dependencies, and does not touch the standard library — a dependency demanding a
    * newer scala-library is a real conflict, and one the user can act on.
    */
  def testLibraryVersionSchemes(isTest: Boolean): List[LibraryVersionScheme] =
    if (!isTest) Nil
    else
      this match {
        case VersionCombo.Js(_, scalaJs)         => List(scalaJs.testInterface, scalaJs.testBridge).map(earlySemVer)
        case VersionCombo.Native(_, scalaNative) => List(earlySemVer(scalaNative.testInterface))
        case _                                   => Nil
      }

  /** A [[LibraryVersionScheme]] keeps the scheme in its dep's *version* slot — `org::name:early-semver` is how one is written in `bleep.yaml`, and
    * [[LibraryVersionScheme.from]] parses the version as the scheme. The `scheme` field and `dep.version` are therefore two spellings of one fact, and they
    * have to agree.
    *
    * Handing the library dep straight in leaves them disagreeing: the in-memory value resolves correctly, because resolution reads the organization, the module
    * name and `scheme`. But the pair is also part of the resolution cache key, and there the dep is what gets written — so the entry serialized as
    * `org.scala-native::test-interface:0.5.8` and refused to decode on the way back in: "Invalid version scheme: '0.5.8'". Round-tripping is what caught it.
    */
  private def earlySemVer(dep: Dep): LibraryVersionScheme = {
    val scheme = LibraryVersionScheme.VersionScheme.EarlySemVer
    LibraryVersionScheme(scheme, dep.withVersion(scheme.value))
  }

  val compilerPlugin: Option[Dep]
  val compilerOptions: Options
}

object VersionCombo {

  implicit val encoder: Encoder[VersionCombo] =
    Encoder.instance {
      case Java =>
        Json.obj("Java" -> Json.Null)
      case Kotlin(kotlinVersion) =>
        Json.obj("Kotlin" := Json.obj("kotlinVersion" := kotlinVersion))
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
        case Some("Kotlin") =>
          c.downField("Kotlin").downField("kotlinVersion").as[VersionKotlin].map(Kotlin.apply)
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
          Left(DecodingFailure("expected object with one of `Java`, `Kotlin`, `Jvm`, `Js` or `Native` keys", c.history))
      }

  case object Java extends VersionCombo {
    override val compilerPlugin: Option[Dep] = None
    override val compilerOptions: Options = Options.empty
  }

  case class Kotlin(kotlinVersion: VersionKotlin) extends VersionCombo {
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
    // Kotlin takes precedence if specified
    p.kotlin.flatMap(_.version) match {
      case Some(kotlinVersion) => Right(Kotlin(kotlinVersion))
      case None                => fromExplodedScalaAndPlatform(p.scala.flatMap(_.version), p.platform)
    }
}
