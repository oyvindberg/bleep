package bleep
package packaging

import coursier.core.{ModuleName, Organization}

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
      // as sbt looks the plugin up, like a dependency on it: sbt 1 by its name and attributes, which a pom writes as `name_2.12_1.0` and an ivy
      // repository as `name/scala_2.12/sbt_1.0`, sbt 2 as `name_sbt2_3`
      case Some(scala) if scala.sbtPlugin.contains(true) =>
        val scalaVersion = scala.version.getOrElse {
          throw new BleepException.Text(s"${crossName.value} is an sbt plugin without a scala version: 2.12 for sbt 1, 3 for sbt 2")
        }
        model.Scala.SbtPlugin.forScalaVersion(scalaVersion) match {
          case Right(_) =>
            model.Dep.ScalaDependency(Organization(groupId), ModuleName(name), version, fullCrossVersion = false, isSbtPlugin = true)
          case Left(err) => throw new BleepException.Text(s"${crossName.value}: $err")
        }
      case Some(scala) if scala.version.isDefined => model.Dep.Scala(org = groupId, name = name, version = version)
      case _                                      => model.Dep.Java(org = groupId, name = name, version = version)
    }
  }
}
