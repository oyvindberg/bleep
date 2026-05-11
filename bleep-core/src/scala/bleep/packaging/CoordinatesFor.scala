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

    // Check for an actual Scala version, not just the presence of a `scala:` block. Projects can
    // inherit Scala options (encoding, language flags, strict mode) from shared templates without
    // declaring a Scala version themselves — those projects are Java artifacts and should publish
    // with a plain `groupId:name:version` coord, not `groupId:name_${binVersion}:version`.
    explodedProject.scala.flatMap(_.version) match {
      case Some(_) => model.Dep.Scala(org = groupId, name = name, version = version)
      case None    => model.Dep.Java(org = groupId, name = name, version = version)
    }
  }
}
