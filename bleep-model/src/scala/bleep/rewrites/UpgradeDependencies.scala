package bleep
package rewrites

import bleep.rewrites.UpgradeDependencies.{ContextualDep, UpgradeLogger}

case class UpgradeDependencies(logger: UpgradeLogger, upgrades: Map[ContextualDep, model.Dep]) extends BuildRewrite {
  override val name = model.BuildRewriteName("upgrade-dependencies")

  override protected def newExplodedProjects(oldBuild: model.Build, buildPaths: BuildPaths): Map[model.CrossProjectName, model.Project] = {
    val newProjects = oldBuild.explodedProjects.map { case (crossName, p) =>
      val versionCombo = model.VersionCombo.fromExplodedProject(p).orThrowTextWithContext(crossName)
      val newDeps = p.dependencies.map { dep =>
        upgrades.get((dep, versionCombo)) match {
          case Some(newDep) =>
            logger.upgraded(crossName, dep, newDep.version)
            newDep
          case None => dep
        }
      }
      (crossName, p.copy(dependencies = newDeps))
    }
    newProjects
  }
}

object UpgradeDependencies {
  trait UpgradeLogger {
    def upgraded(project: model.CrossProjectName, dep: model.Dep, newVersion: String): Unit
  }
  object UpgradeLogger {
    object Noop extends UpgradeLogger {
      override def upgraded(project: model.CrossProjectName, dep: model.Dep, newVersion: String): Unit = ()
    }
  }

  type ContextualDep = (model.Dep, model.VersionCombo)
}
