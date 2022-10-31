package bleep
package packaging

trait CoordinatesFor {
  def apply(crossName: model.CrossProjectName, explodedProject: model.Project): model.Dep
}

object CoordinatesFor {
  case class Default(groupId: String, version: String) extends CoordinatesFor {
    override def apply(crossName: model.CrossProjectName, explodedProject: model.Project): model.Dep =
      explodedProject.scala match {
        case Some(_) => model.Dep.Scala(org = groupId, name = crossName.name.value, version = version)
        case None    => model.Dep.Java(org = groupId, name = crossName.name.value, version = version)
      }
  }
}
