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
    val name = crossName.name.fileSafeValue

    // Check for an actual Scala version, not just the presence of a `scala:` block. Projects can
    // inherit Scala options (encoding, language flags, strict mode) from shared templates without
    // declaring a Scala version themselves — those projects are Java artifacts and should publish
    // with a plain `groupId:name:version` coord, not `groupId:name_${binVersion}:version`.
    explodedProject.scala match {
      // under the name sbt looks the plugin up by: `name_2.12_1.0` for sbt 1, `name_sbt2_3` for sbt 2
      case Some(scala) if scala.sbtPlugin.contains(true) =>
        val scalaVersion = scala.version.getOrElse {
          throw new BleepException.Text(s"${crossName.value} is an sbt plugin without a scala version: 2.12 for sbt 1, 3 for sbt 2")
        }
        model.Scala.SbtPlugin.forScalaVersion(scalaVersion) match {
          case Right(plugin) => model.Dep.Java(org = groupId, name = plugin.artifactName(name), version = version)
          case Left(err)     => throw new BleepException.Text(s"${crossName.value}: $err")
        }
      case Some(scala) if scala.version.isDefined => model.Dep.Scala(org = groupId, name = name, version = version)
      case _                                      => model.Dep.Java(org = groupId, name = name, version = version)
    }
  }
}
