package bleep
package internal

import coursier.core.{Module, ModuleName, Organization}

object ModuleParser {

  /** Parses a module like org:name possibly with attributes, like org:name;attr1=val1;attr2=val2
    *
    * Two semi-columns after the org part is interpreted as a scala module. E.g. if the scala version is 2.11., org::name is equivalent to org:name_2.11.
    */
  def javaOrScalaModule(s: String): Either[String, JavaOrScalaModule] = {

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
}
