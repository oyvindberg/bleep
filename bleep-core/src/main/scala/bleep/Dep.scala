package bleep

import coursier._
import coursier.core.Configuration
import coursier.parse.{DependencyParser, JavaOrScalaDependency}
import io.circe.{Decoder, DecodingFailure, Encoder, Json}

object Dep {
  def Java(org: String, name: String, version: String): JavaOrScalaDependency =
    JavaOrScalaDependency.JavaDependency(Dependency(Module(Organization(org), ModuleName(name)), version), Set.empty)

  def Scala(org: String, name: String, version: String): JavaOrScalaDependency =
    JavaOrScalaDependency.ScalaDependency(
      Dependency(Module(Organization(org), ModuleName(name)), version),
      fullCrossVersion = false,
      withPlatformSuffix = false,
      Set.empty
    )

  def ScalaFullVersion(org: String, name: String, version: String): JavaOrScalaDependency =
    JavaOrScalaDependency.ScalaDependency(
      Dependency(Module(Organization(org), ModuleName(name)), version),
      fullCrossVersion = true,
      withPlatformSuffix = false,
      Set.empty
    )

  def ScalaJs(org: String, name: String, version: String): JavaOrScalaDependency =
    JavaOrScalaDependency.ScalaDependency(
      Dependency(Module(Organization(org), ModuleName(name)), version),
      fullCrossVersion = false,
      withPlatformSuffix = true,
      Set.empty
    )

  implicit def decodesDep: Decoder[JavaOrScalaDependency] =
    Decoder.instance(c =>
      for {
        str <- c.as[String]
        tuple <- DependencyParser.javaOrScalaDependencyParams(str, Configuration.empty).left.map(err => DecodingFailure(err, c.history))
      } yield tuple._1
    )

  implicit def encodesDep: Encoder[JavaOrScalaDependency] =
    Encoder.instance {
      case dep: JavaOrScalaDependency.JavaDependency =>
        Json.fromString(s"${dep.module}:${dep.dependency.version}")
      case dep: JavaOrScalaDependency.ScalaDependency => Json.fromString(dep.repr)
    }
}
