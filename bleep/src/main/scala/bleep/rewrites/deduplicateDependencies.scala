package bleep
package rewrites

import bleep.internal.rewriteDependentData
import coursier.core.Configuration

/** Trims dependencies, both on libraries and on projects, which are already provided by a parent project */
object deduplicateDependencies extends Rewrite {
  override val name = "deduplicate-dependencies"

  override def apply(build: ExplodedBuild): ExplodedBuild = {
    val projects = rewriteDependentData(build.projects).apply[model.Project] { (projectName, p, getDep) =>
      val shortenedDeps: Map[model.CrossProjectName, model.Project] =
        build.transitiveDependenciesFor(projectName).map { case (pn, _) => (pn, getDep(pn).forceGet(pn.value)) }

      val shortenedDependsOn: JsonSet[model.ProjectName] =
        p.dependsOn.filterNot(shortenedDeps.flatMap { case (_, p) => p.dependsOn.values }.toSet)

      val shortenedDependencies: JsonSet[Dep] = {
        val includedInDependees: Set[Dep] =
          shortenedDeps.flatMap { case (_, p) =>
            p.dependencies.values.filter { dep =>
              dep.configuration != Configuration.optional && dep.configuration != Configuration.provided
            }
          }.toSet

        val newInThisProject: JsonSet[Dep] =
          p.dependencies.filterNot(includedInDependees)

        newInThisProject
      }

      p.copy(dependsOn = shortenedDependsOn, dependencies = shortenedDependencies)
    }

    val forcedProjects = projects.map { case (projectName, lazyProject) => (projectName, lazyProject.forceGet(projectName.value)) }

    ExplodedBuild(
      templates = build.templates,
      projects = forcedProjects,
      scripts = build.scripts,
      resolvers = build.resolvers,
      retainCrossTemplates = build.retainCrossTemplates
    )
  }
}
