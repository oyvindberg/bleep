package bleep

import bleep.internal.rewriteDependentData
import bleep.model.Platform
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

          removeScalaDefaults(ret)
        }

      val shortenedPlatform: Option[model.Platform] =
        p.platform.map { platform =>
          removePlatformDefaults(platform.parent(build).foldLeft(platform) { case (acc, parent) => acc.removeAll(parent.explode(build)) })
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
      platforms = build.platforms.map(_.map { case (id, p) => (id, removePlatformDefaults(p)) }),
      scala = build.scala.map(_.map { case (id, s) => (id, removeScalaDefaults(s)) }),
      java = build.java,
      scripts = build.scripts,
      resolvers = build.resolvers
    )
  }

  def removeScalaDefaults(ret: model.Scala): model.Scala =
    ret.copy(setup = ret.setup.map(setup => setup.removeAll(Defaults.DefaultCompileSetup)))

  def removePlatformDefaults(x: model.Platform): model.Platform =
    x match {
      case x: Platform.Js     => x
      case x: Platform.Jvm    => x.removeAllJvm(Defaults.Jvm)
      case x: Platform.Native => x
    }
}
