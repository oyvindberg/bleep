package bleep

import coursier.core.Module

sealed trait JavaOrScalaModule {
  def attributes: Map[String, String]
}

object JavaOrScalaModule {
  case class JavaModule(module: Module) extends JavaOrScalaModule {
    def attributes: Map[String, String] = module.attributes
    override def toString = module.repr
  }
  case class ScalaModule(baseModule: Module, fullCrossVersion: Boolean) extends JavaOrScalaModule {
    def attributes: Map[String, String] = baseModule.attributes
    override def toString = {
      val sep = if (fullCrossVersion) ":::" else "::"
      s"${baseModule.organization.value}$sep${baseModule.nameWithAttributes}"
    }
  }
}
