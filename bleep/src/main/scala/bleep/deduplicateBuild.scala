package bleep

import bleep.internal.rewriteDependentData
import coursier.parse.JavaOrScalaDependency

object deduplicateBuild {
  def apply(build: model.Build): model.Build = {
    val projects = rewriteDependentData[model.ProjectName, model.Project, model.Project](build.projects) { (projectName, p, getDep) =>
      val shortenedDeps: Map[model.ProjectName, model.Project] =
        build.transitiveDependenciesFor(projectName).map { case (pn, _) => (pn, getDep(pn).forceGet(pn.value)) }

      val dependsOn: JsonSet[model.ProjectName] =
        p.dependsOn.filterNot(shortenedDeps.flatMap { case (_, p) => p.dependsOn.values }.toSet)

      val dependencies: JsonSet[JavaOrScalaDependency] = {
        val includedInDependees: Set[JavaOrScalaDependency] =
          shortenedDeps.flatMap { case (_, p) => p.dependencies.values }.toSet

        val newInThisProject: JsonSet[JavaOrScalaDependency] =
          p.dependencies.filterNot(includedInDependees)

        newInThisProject
      }

      model.Project(
        folder = p.folder,
        dependsOn = dependsOn,
        `source-layout` = p.`source-layout`,
        `sbt-scope` = p.`sbt-scope`,
        sources = p.sources,
        resources = p.resources,
        dependencies = dependencies,
        java = p.java,
        scala = p.scala,
        platform = p.platform
      )
    }

    val forcedProjects = projects.map { case (pn, lp) => (pn, lp.forceGet(pn.value)) }

    model.Build(
      version = build.version,
      projects = forcedProjects,
      platforms = build.platforms,
      scala = build.scala,
      java = build.java,
      scripts = build.scripts,
      resolvers = build.resolvers
    )
  }
}
