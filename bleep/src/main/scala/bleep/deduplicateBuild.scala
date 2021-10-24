package bleep

import bleep.internal.rewriteDependentData
import coursier.parse.JavaOrScalaDependency

object deduplicateBuild {
  def apply(build: model.Build): model.Build = {
    val projects = rewriteDependentData[model.ProjectName, model.Project, model.Project](build.projects) { (projectName, p, getDep) =>
      val shortenedDeps: Map[model.ProjectName, model.Project] =
        build.transitiveDependenciesFor(projectName).map { case (pn, _) => (pn, getDep(pn).forceGet(pn.value)) }

      val dependsOn: List[model.ProjectName] =
        p.dependsOn.flat.filterNot(shortenedDeps.flatMap { case (_, p) => p.dependsOn.flat }.toSet)

      val directDeps: List[model.Project] =
        dependsOn.map(pn => shortenedDeps(pn))

      val dependencies: List[JavaOrScalaDependency] =
        p.dependencies.flat.filterNot(shortenedDeps.flatMap { case (_, p) => p.dependencies.flat }.toSet).sortBy(_.module.toString).distinctBy(_.module)

      val (sourceLayout, sources, resources) = {
        val scalaVersion: Option[Versions.Scala] = {
          def go(scala: model.Scala): Option[Versions.Scala] =
            scala.version.orElse {
              scala.`extends`.zip(build.scala).flatMap{case (id, scalas) => go(scalas(id))}
            }
          p.scala.flatMap(go)
        }

        val inferredSourceLayout: Option[SourceLayout] =
          p.`source-layout`.orElse {
            SourceLayout.All.values.maxByOption { layout =>
              val fromLayout = layout.sources(scalaVersion, p.`sbt-scope`).toSet
              val fromProject = p.sources.flat.toSet
              val matching = fromLayout.intersect(fromProject).size
              val notMatching = fromLayout.removedAll(fromProject).size
              (matching, -notMatching)
            }
          }

        inferredSourceLayout match {
          case Some(sl) =>
            val shortenedSources = p.sources.flat.filterNot(sl.sources(scalaVersion, p.`sbt-scope`).toSet)
            val shortenedResources = p.resources.flat.filterNot(sl.resources(scalaVersion, p.`sbt-scope`).toSet)
            (Some(sl), Option(JsonList(shortenedSources)).filterNot(_.isEmpty), Option(JsonList(shortenedResources)).filterNot(_.isEmpty))
          case None =>
            (None, p.sources, p.resources)
        }
      }

      model.Project(
        folder = p.folder,
        dependsOn = Some(JsonList(dependsOn)).filterNot(_.isEmpty),
        `source-layout` = sourceLayout,
        `sbt-scope` = p.`sbt-scope`,
        sources = sources,
        resources = resources,
        dependencies = Some(JsonList(dependencies)).filterNot(_.isEmpty),
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
