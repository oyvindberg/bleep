package bleep.model

import coursier.core.{ModuleName, Organization}
import io.circe.{Decoder, Encoder}

case class VersionScala(scalaVersion: String) {
  require(scalaVersion.nonEmpty)

  val is3 = scalaVersion.startsWith("3.")

  val epoch = scalaVersion.head

  val scalaOrganization: String =
    "org.scala-lang"

  val compiler: Dep =
    if (is3) Dep.Scala(scalaOrganization, "scala3-compiler", scalaVersion)
    else Dep.Java(scalaOrganization, "scala-compiler", scalaVersion)

  val library: Dep =
    if (is3) VersionScala.Scala213.library
    else Dep.Java(scalaOrganization, "scala-library", scalaVersion)

  val scala3Library: Option[Dep] =
    if (is3) Some(Dep.Scala(scalaOrganization, "scala3-library", scalaVersion))
    else None

  // this does look funky. it does come from sbt with the suffix already baked in, so let's roll with that for now
  val scala3JsLibrary: Dep.ScalaDependency =
    Dep.ScalaDependency(Organization(scalaOrganization), ModuleName("scala3-library_sjs1"), scalaVersion, fullCrossVersion = false, forceJvm = true)

  val libraries: List[Dep] =
    List(Some(library), scala3Library).flatten

  val binVersion: String = scalaVersion match {
    case VersionScala.Version("3", _, _)     => s"3"
    case VersionScala.Version("2", minor, _) => s"2.$minor"
    case other                               => other
  }
}
object VersionScala {
  implicit val decodes: Decoder[VersionScala] = Decoder[String].map(VersionScala.apply)
  implicit val encodes: Encoder[VersionScala] = Encoder[String].contramap(_.scalaVersion)

  // this accepts any nightly or milestone with the same binversion as a major release. good enough for now
  val Version = "(\\d+).(\\d+).(\\d+).*".r

  implicit val ordering: Ordering[VersionScala] = Ordering.by(_.scalaVersion)

  val Scala212 = VersionScala("2.12.15")
  val Scala213 = VersionScala("2.13.6")
  val Scala3 = VersionScala("3.2.0")
}
