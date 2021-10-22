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
          for {
            requiredScala <- p.scala
            scalas <- build.scala
            found <- scalas.get(requiredScala)
            foundVersion <- found.version
          } yield foundVersion
        }

        val inferredSourceLayout: Option[SourceLayout] =
          p.`source-layout`.orElse {
            val withIntersection: Iterable[(Int, SourceLayout)] =
              SourceLayout.All.values.map(sl => (sl.sources(scalaVersion, p.`sbt-scope`).intersect(p.sources.flat).length, sl)).filter { case (n, _) => n > 0 }
            withIntersection.maxByOption(_._1).map(_._2)
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
      scala = build.scala,
      java = build.java,
      scripts = build.scripts,
      projects = forcedProjects,
      resolvers = build.resolvers
    )
  }
}
