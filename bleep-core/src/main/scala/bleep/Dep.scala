package bleep

import bleep.Dep.quote
import io.circe.{Decoder, DecodingFailure, Encoder, Json}

sealed trait Dep {
  def org: String
  def version: String

  def for3Use2_13(is3: Boolean): Dep =
    if (is3) Dep.For3Use2_13(this) else this

  // todo: look up the details
  def asCoursier: String =
    this match {
      case concrete: Dep.Concrete =>
        s"${concrete.org}:${concrete.mangledArtifact}:${concrete.version}"
      case Dep.For3Use2_13(dep) =>
        s"${dep.asCoursier},for3use213"
      case Dep.Scala(org, name, version) =>
        s"$org::$name:$version"
      case Dep.ScalaFullVersion(_, _, _) =>
        sys.error("find a way to encode this")
      case Dep.ScalaJs(org, name, version) =>
        s"$org:::$name:$version"
    }

  def asSbt: String =
    this match {
      case Dep.Mangled(_, dep) =>
        dep.asSbt
      case Dep.Java(_, artifact, _) =>
        s"${quote(org)} % ${quote(artifact)} % ${quote(version)}"
      case Dep.Scala(_, artifact, _) =>
        s"${quote(org)} %% ${quote(artifact)} % ${quote(version)}"
      case Dep.ScalaJs(_, artifact, _) =>
        s"${quote(org)} %%% ${quote(artifact)} % ${quote(version)}"
      case Dep.ScalaFullVersion(_, artifact, _) =>
        s"${quote(org)} % ${quote(artifact)} % ${quote(version)} cross CrossVersion.Full()"
      case Dep.For3Use2_13(dep) =>
        s"""(${dep.asSbt}).cross(CrossVersion.for3Use2_13)"""
      case Dep.WithConfiguration =>
    }

  def concrete(scalaVersion: Versions.Scala, maybeScalaJs: Option[Versions.ScalaJs]): Dep.Concrete =
    this match {
      case concrete: Dep.Concrete => concrete
      case Dep.Scala(_, artifact, _) =>
        Dep.Mangled(s"${artifact}_${scalaVersion.binVersion}", this)
      case Dep.ScalaJs(_, artifact, _) =>
        maybeScalaJs match {
          case Some(scalaJs) =>
            Dep.Mangled(s"${artifact}_sjs${scalaJs.scalaJsBinVersion}_${scalaVersion.binVersion}", this)
          case None =>
            sys.error("you can only use ::: syntax within a scalajs project")
        }

      case Dep.ScalaFullVersion(_, artifact, _) =>
        Dep.Mangled(s"${artifact}_${scalaVersion.scalaVersion}", this)
      case Dep.For3Use2_13(dep) =>
        dep.concrete(Versions.Scala213, maybeScalaJs)
    }
}

object Dep {
  val Quote = '"'

  def quote(s: String): String =
    s"$Quote${s}$Quote"

  sealed trait Concrete extends Dep {

    def mangledArtifact: String

    def asMangledSbt: String =
      s"${quote(org)} % ${quote(mangledArtifact)} % ${quote(version)}"
  }

  object Concrete {
    implicit val concreteOrdering: Ordering[Concrete] =
      Ordering.by(_.asMangledSbt)
  }

  case class Java(org: String, name: String, version: String) extends Concrete {
    override def mangledArtifact: String = name
  }

  case class For3Use2_13(dep: Dep) extends Dep {
    override def org: String = dep.org
    override def version: String = dep.version
  }
  case class Scala(org: String, name: String, version: String) extends Dep
  case class ScalaFullVersion(org: String, name: String, version: String) extends Dep
  case class ScalaJs(org: String, name: String, version: String) extends Dep

  case class Mangled(mangledArtifact: String, dep: Dep) extends Concrete {
    override def org: String = dep.org
    override def version: String = dep.version
  }

  def parseCoursierString(_str: String): Either[String, Dep] = {
    val splitFlags = _str.split(",")
    val flags = splitFlags.drop(1).toSet

    val str = splitFlags(0)
    val dep = str.split(":::") match {
      case Array(org, rest) =>
        rest.split(":") match {
          case Array(name, version) => Right(Dep.Java(org, name, version))
          case _                    => Left(s"illegal dependency string: $str")
        }
      case _ =>
        str.split("::") match {
          case Array(org, rest) =>
            rest.split(":") match {
              case Array(name, version) => Right(Dep.Scala(org, name, version))
              case _                    => Left(s"illegal dependency string: $str")
            }
          case _ =>
            str.split(":") match {
              case Array(org, name, version) => Right(Dep.Java(org, name, version))
              case _                         => Left(s"illegal dependency string: $str")
            }
        }
    }

    if (flags("for3use213")) dep.map(Dep.For3Use2_13.apply)
    else dep
  }

  implicit def decodes: Decoder[Dep] =
    Decoder.instance(c =>
      for {
        str <- c.as[String]
        dep <- parseCoursierString(str).left.map(str => DecodingFailure(str, c.history))
      } yield dep
    )

  implicit def encodes: Encoder[Dep] =
    Encoder.instance(dep => Json.fromString(dep.asCoursier))
}
