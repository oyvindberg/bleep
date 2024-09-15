package bleep
package rewrites

import bleep.internal.rewriteDependentData
import coursier.core.Configuration

/** Trims dependencies, both on libraries and on projects, which are already provided by a parent project */
object deduplicateDependencies extends BuildRewrite {
  override val name = model.BuildRewriteName("deduplicate-dependencies")

  protected def newExplodedProjects(oldBuild: model.Build, buildPaths: BuildPaths): Map[model.CrossProjectName, model.Project] =
    rewriteDependentData(oldBuild.explodedProjects).eager[model.Project] { (projectName, p, eval) =>
      val shortenedDeps: Map[model.CrossProjectName, model.Project] =
        oldBuild.transitiveDependenciesFor(projectName).map { case (pn, _) => (pn, eval(pn).forceGet(pn.value)) }

      val shortenedDependsOn: model.JsonSet[model.ProjectName] =
        p.dependsOn.filterNot(shortenedDeps.flatMap { case (_, p) => p.dependsOn.values }.toSet)

      val shortenedDependencies: model.JsonSet[model.Dep] = {
        val includedInDependees =
          shortenedDeps.flatMap { case (_, p) =>
            p.dependencies.values.filter { dep =>
              dep.configuration != Configuration.optional && dep.configuration != Configuration.provided
            }
          }.toSet

        val newInThisProject =
          p.dependencies.filterNot(includedInDependees)

        newInThisProject
      }

      p.copy(dependsOn = shortenedDependsOn, dependencies = shortenedDependencies)
    }
}
