package bleep

import bleep.internal.rewriteDependentData
import bleep.model.Platform

object deduplicateBuild {
  def apply(build: model.Build): model.Build = {
    val templates = build.templates.map { templates =>
      templates.map { case (templateId, templateProject) =>
        templateId -> templateProject.copy(
          scala = templateProject.scala.map(removeScalaDefaults),
          platform = templateProject.platform.map(removePlatformDefaults)
        )
      }
    }

    val projects = rewriteDependentData[model.ProjectName, model.Project, model.Project](build.projects) { (projectName, p, getDep) =>
      val shortenedDeps: Map[model.ProjectName, model.Project] =
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
        folder = p.folder,
        dependsOn = shortenedDependsOn,
        `source-layout` = p.`source-layout`,
        `sbt-scope` = p.`sbt-scope`,
        sources = p.sources,
        resources = p.resources,
        dependencies = shortenedDependencies,
        java = p.java,
        scala = p.scala.map(removeScalaDefaults),
        platform = p.platform.map(removePlatformDefaults),
        p.testFrameworks
      )
    }

    val forcedProjects = projects.map { case (projectName, lazyProject) => (projectName, lazyProject.forceGet(projectName.value)) }

    model.Build(
      version = build.version,
      templates = templates,
      projects = forcedProjects,
      scripts = build.scripts,
      resolvers = build.resolvers
    )
  }

  def removeScalaDefaults(ret: model.Scala): model.Scala =
    ret.copy(setup = ret.setup.map(setup => setup.removeAll(Defaults.DefaultCompileSetup)))

  def removePlatformDefaults(x: model.Platform): model.Platform =
    x.removeAll(Defaults.Jvm)
}
