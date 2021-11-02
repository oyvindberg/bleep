package bleep

import bleep.internal.rewriteDependentData
import coursier.parse.JavaOrScalaDependency

object deduplicateBuild {
  def apply(build: model.Build): model.Build = {
    val projects = rewriteDependentData[model.ProjectName, model.Project, model.Project](build.projects) { (projectName, p, getDep) =>
      val shortenedDeps: Map[model.ProjectName, model.Project] =
        build.transitiveDependenciesFor(projectName).map { case (pn, _) => (pn, getDep(pn).forceGet(pn.value)) }

      val shortenedDependsOn: JsonSet[model.ProjectName] =
        p.dependsOn.filterNot(shortenedDeps.flatMap { case (_, p) => p.dependsOn.values }.toSet)

      val shortenedDependencies: JsonSet[JavaOrScalaDependency] = {
        val includedInDependees: Set[JavaOrScalaDependency] =
          shortenedDeps.flatMap { case (_, p) => p.dependencies.values }.toSet

        val newInThisProject: JsonSet[JavaOrScalaDependency] =
          p.dependencies.filterNot(includedInDependees)

        newInThisProject
      }

      val shortenedJava: Option[model.Java] = {
        val shortened = build.java match {
          case Some(buildJava) => p.java.map(_.removeAll(buildJava))
          case None            => p.java
        }
        shortened.filterNot(_.isEmpty)
      }

      val shortenedScala: Option[model.Scala] =
        p.scala.map { projectScala =>
          val removeInherited = for {
            parentScala <- projectScala.parent(build)
            explodedParentScala = parentScala.explode(build)
          } yield projectScala.removeAll(explodedParentScala)

          val ret = removeInherited.getOrElse(projectScala)

          ret.copy(setup = ret.setup.map(setup => setup.removeAll(Defaults.DefaultCompileSetup)))
        }

      val shortenedPlatform: Option[model.Platform] =
        p.platform.map { platform =>
          platform.parent(build).foldLeft(platform) { case (acc, parent) => acc.removeAll(parent.explode(build)) }
        }

      model.Project(
        folder = p.folder,
        dependsOn = shortenedDependsOn,
        `source-layout` = p.`source-layout`,
        `sbt-scope` = p.`sbt-scope`,
        sources = p.sources,
        resources = p.resources,
        dependencies = shortenedDependencies,
        java = shortenedJava,
        scala = shortenedScala,
        platform = shortenedPlatform
      )
    }

    val forcedProjects = projects.map { case (projectName, lazyProject) => (projectName, lazyProject.forceGet(projectName.value)) }

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
