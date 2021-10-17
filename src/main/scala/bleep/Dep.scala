package bleep

import coursier.{Dependency, Module}
import bleep.Dep.quote
import io.circe.{Decoder, DecodingFailure}

import scala.xml.Elem

sealed trait Dep {
  def org: String
  def version: String

  def for3Use2_13(is3: Boolean): Dep =
    if (is3) Dep.For3Use2_13(this) else this

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
    def asCoursier: Dependency =
      Dependency(Module(coursier.Organization(org), coursier.ModuleName(mangledArtifact)), version)

    def mangledArtifact: String

    def asMangledSbt: String =
      s"${quote(org)} % ${quote(mangledArtifact)} % ${quote(version)}"

    def asIvy(config: String = "compile->default(compile)"): Elem =
      <dependency org={org} name={mangledArtifact} rev={version} conf={config}/>

    // format: off
    def asMaven: Elem =
      <dependency>
        <groupId>{org}</groupId>
        <artifactId>{mangledArtifact}</artifactId>
        <version>{version}</version>
      </dependency>

    def asMavenTest: Elem =
      <dependency>
        <groupId>{org}</groupId>
        <artifactId>{mangledArtifact}</artifactId>
        <version>{version}</version>
        <scope>test</scope>
      </dependency>
    // format: on
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

  def parse(str: String): Either[String, Dep] =
    str.split(":::") match {
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

  implicit def decodes: Decoder[Dep] =
    Decoder.instance(c =>
      for {
        str <- c.as[String]
        dep <- parse(str).left.map(str => DecodingFailure(str, c.history))
      } yield dep
    )
}
