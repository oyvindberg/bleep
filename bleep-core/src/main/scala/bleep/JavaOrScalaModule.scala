package bleep

import coursier.core.{Module, ModuleName, Organization}
import io.circe.syntax._
import io.circe.{Decoder, DecodingFailure, Encoder}

sealed trait JavaOrScalaModule {
  def attributes: Map[String, String]
  def repr: String
  final override def toString: String = repr
}

object JavaOrScalaModule {
  implicit val decoder: Decoder[JavaOrScalaModule] =
    Decoder.instance { c =>
      c.as[String].flatMap { module =>
        parse(module) match {
          case Left(err)    => Left(DecodingFailure(err, c.history))
          case Right(value) => Right(value)
        }
      }
    }
  implicit val encoder: Encoder[JavaOrScalaModule] =
    Encoder.instance(_.repr.asJson)

  /** Parses a module like org:name possibly with attributes, like org:name;attr1=val1;attr2=val2
    *
    * Two semi-columns after the org part is interpreted as a scala module. E.g. if the scala version is 2.11., org::name is equivalent to org:name_2.11.
    */
  def parse(s: String): Either[String, JavaOrScalaModule] = {

    val parts = s.split(":", -1)

    val values = parts match {
      case Array(org, rawName) =>
        Right((Organization(org), rawName, None))
      case Array(org, "", rawName) =>
        Right((Organization(org), rawName, Some(false)))
      case Array(org, "", "", rawName) =>
        Right((Organization(org), rawName, Some(true)))
      case _ =>
        Left(s"malformed module: $s")
    }

    values.flatMap { case (org, rawName, scalaFullVerOpt) =>
      val splitName = rawName.split(';')

      if (splitName.tail.exists(!_.contains("=")))
        Left(s"malformed attribute(s) in $s")
      else {
        val name = splitName.head
        val attributes = splitName.tail
          .map(_.split("=", 2))
          .map { case Array(key, value) =>
            key -> value
          }
          .toMap

        val baseModule = Module(org, ModuleName(name), attributes)

        val module = scalaFullVerOpt match {
          case None =>
            JavaOrScalaModule.JavaModule(baseModule)
          case Some(scalaFullVer) =>
            JavaOrScalaModule.ScalaModule(baseModule, scalaFullVer)
        }

        Right(module)
      }
    }
  }

  case class JavaModule(module: Module) extends JavaOrScalaModule {
    def attributes: Map[String, String] = module.attributes
    override def repr = module.repr
  }

  case class ScalaModule(baseModule: Module, fullCrossVersion: Boolean) extends JavaOrScalaModule {
    def attributes: Map[String, String] = baseModule.attributes
    override def repr = {
      val sep = if (fullCrossVersion) ":::" else "::"
      s"${baseModule.organization.value}$sep${baseModule.nameWithAttributes}"
    }
  }
}
