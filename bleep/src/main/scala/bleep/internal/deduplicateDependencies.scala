package bleep
package internal

/** Trims dependencies, both on libraries and on projects, which are already provided by a parent project */
object deduplicateDependencies {
  def apply(build: ExplodedBuild): ExplodedBuild = {
    val projects = rewriteDependentData[model.CrossProjectName, model.Project, model.Project](build.projects) { (projectName, p, getDep) =>
      val shortenedDeps: Map[model.CrossProjectName, model.Project] =
        build.transitiveDependenciesFor(projectName).map { case (pn, _) => (pn, getDep(pn).forceGet(pn.value)) }

      val shortenedDependsOn: JsonSet[model.ProjectName] =
        p.dependsOn.filterNot(shortenedDeps.flatMap { case (_, p) => p.dependsOn.values }.toSet)

      val shortenedDependencies: JsonSet[Dep] = {
        val includedInDependees: Set[Dep] =
          shortenedDeps.flatMap { case (_, p) => p.dependencies.values }.toSet

        val newInThisProject: JsonSet[Dep] =
          p.dependencies.filterNot(includedInDependees)

        newInThisProject
      }

      model.Project(
        `extends` = p.`extends`,
        cross = p.cross,
        folder = p.folder,
        dependsOn = shortenedDependsOn,
        `source-layout` = p.`source-layout`,
        `sbt-scope` = p.`sbt-scope`,
        sources = p.sources,
        resources = p.resources,
        dependencies = shortenedDependencies,
        java = p.java,
        scala = p.scala,
        platform = p.platform,
        p.testFrameworks
      )
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
