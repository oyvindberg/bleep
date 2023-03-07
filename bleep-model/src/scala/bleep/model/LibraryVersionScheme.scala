package bleep.model

import bleep.BleepException
import coursier.Organization
import coursier.core.ModuleName
import io.circe.{Decoder, Encoder}

sealed trait LibraryVersionScheme {
  def org: Organization
  def scheme: LibraryVersionScheme.VersionScheme

  val repr: String = this match {
    case LibraryVersionScheme.Java(org, moduleName, scheme)      => s"${org.value}:${moduleName.value}:${scheme.value}"
    case LibraryVersionScheme.Scala(org, baseModuleName, scheme) => s"${org.value}::${baseModuleName.value}:${scheme.value}"
  }

  // todo: simplified version of the one in `Dep`. Unify.
  def moduleName(combo: VersionCombo): Either[BleepException, ModuleName] =
    this match {
      case LibraryVersionScheme.Scala(_, baseModuleName, _) =>
        combo match {
          case VersionCombo.Java =>
            Left(new BleepException.Text(s"scala library version scheme $repr (note double `:`) is used in java-only project"))
          case combo: VersionCombo.Scala =>
            val platformSuffix: String =
              combo match {
                case VersionCombo.Jvm(_)                 => ""
                case VersionCombo.Js(_, scalaJsVersion)  => s"_sjs${scalaJsVersion.scalaJsBinVersion}"
                case VersionCombo.Native(_, scalaNative) => s"_native${scalaNative.scalaNativeBinVersion}"
              }

            val scalaSuffix: String = "_" + combo.scalaVersion.binVersion

            Right(baseModuleName.map(_ + (platformSuffix + scalaSuffix)))
        }
      case LibraryVersionScheme.Java(_, moduleName, _) => Right(moduleName)
    }
}

object LibraryVersionScheme {
  sealed abstract class VersionScheme(val value: String)

  object VersionScheme {
    case object EarlySemVer extends VersionScheme("early-semver")
    case object SemVerSpec extends VersionScheme("semver-spec")
    case object PackVer extends VersionScheme("pvp")
    case object Strict extends VersionScheme("strict")
    case object Always extends VersionScheme("always")

    val All: List[VersionScheme] =
      List(EarlySemVer, SemVerSpec, PackVer, Strict, Always)
    val byName: Map[String, VersionScheme] =
      All.map(x => x.value -> x).toMap
    def fromString(str: String): Either[String, VersionScheme] =
      byName.get(str).toRight(s"'${str}' not among ${byName.keys.mkString(" ")}")
  }

  case class Java(org: Organization, moduleName: ModuleName, scheme: VersionScheme) extends LibraryVersionScheme
  case class Scala(org: Organization, baseModuleName: ModuleName, scheme: VersionScheme) extends LibraryVersionScheme

  def parse(input: String): Either[String, LibraryVersionScheme] =
    input.split(":") match {
      case Array(org, "", name, scheme) =>
        VersionScheme.fromString(scheme).map(scheme => Scala(Organization(org), ModuleName(name), scheme))
      case Array(org, name, scheme) =>
        VersionScheme.fromString(scheme).map(scheme => Java(Organization(org), ModuleName(name), scheme))
      case _ =>
        Left(s"Wrong amount of colons in your library version scheme $input")
    }

  implicit val ordering: Ordering[LibraryVersionScheme] = Ordering.by(_.repr)
  implicit val decoder: Decoder[LibraryVersionScheme] = Decoder[String].emap(parse)
  implicit val encoder: Encoder[LibraryVersionScheme] = Encoder[String].contramap(_.repr)
}
