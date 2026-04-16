package bleep
package packaging

trait CoordinatesFor {
  def apply(crossName: model.CrossProjectName, explodedProject: model.Project): model.Dep
}

object CoordinatesFor {
  case class Default(groupId: String, version: String) extends CoordinatesFor {
    override def apply(crossName: model.CrossProjectName, explodedProject: model.Project): model.Dep =
      fromGroupId(groupId, version, crossName, explodedProject)
  }

  /** Reads groupId from each project's publish config. Falls back to the provided default. */
  case class FromModel(version: String, fallbackGroupId: String) extends CoordinatesFor {
    override def apply(crossName: model.CrossProjectName, explodedProject: model.Project): model.Dep = {
      val groupId = explodedProject.publish.flatMap(_.groupId).getOrElse(fallbackGroupId)
      fromGroupId(groupId, version, crossName, explodedProject)
    }
  }

  private def fromGroupId(groupId: String, version: String, crossName: model.CrossProjectName, explodedProject: model.Project): model.Dep = {
    val name = crossName.name.value.replace('/', '-')

    explodedProject.scala match {
      case Some(_) => model.Dep.Scala(org = groupId, name = name, version = version)
      case None    => model.Dep.Java(org = groupId, name = name, version = version)
    }
  }
}
