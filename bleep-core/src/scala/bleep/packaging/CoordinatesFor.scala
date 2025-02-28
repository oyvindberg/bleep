package bleep
package packaging

trait CoordinatesFor {
  def apply(crossName: model.CrossProjectName, explodedProject: model.Project): model.Dep
}

object CoordinatesFor {
  case class Default(groupId: String, version: String) extends CoordinatesFor {
    override def apply(crossName: model.CrossProjectName, explodedProject: model.Project): model.Dep = {
      // move prefixes separated by slash in the name to groupId
      val (org, name) = crossName.name.value.split('/') match {
        case Array(name) => (groupId, name)
        case more        => (groupId + more.init.map("." + _).mkString, more.last)
      }

      explodedProject.scala match {
        case Some(_) => model.Dep.Scala(org = org, name = name, version = version)
        case None    => model.Dep.Java(org = org, name = name, version = version)
      }
    }
  }
}
