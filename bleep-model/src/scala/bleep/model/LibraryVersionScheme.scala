package bleep
package model

import io.circe.{Decoder, Encoder}

case class LibraryVersionScheme(scheme: LibraryVersionScheme.VersionScheme, dep: Dep)

object LibraryVersionScheme {
  def from(dep: Dep): Either[String, LibraryVersionScheme] =
    VersionScheme.fromString(dep.version) match {
      case Left(err)            => Left(s"Invalid version scheme: $err")
      case Right(versionScheme) => Right(LibraryVersionScheme(versionScheme, dep))
    }

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
      byName.get(str).toRight(s"'$str' not among ${byName.keys.mkString(" ")}")
  }

  implicit val ordering: Ordering[LibraryVersionScheme] = Ordering.by(_.dep.repr)
  implicit val decoder: Decoder[LibraryVersionScheme] = Decoder[Dep].emap(from)
  implicit val encoder: Encoder[LibraryVersionScheme] = Encoder[Dep].contramap(_.dep)
}
