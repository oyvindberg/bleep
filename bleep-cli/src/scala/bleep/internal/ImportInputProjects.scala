package bleep.internal

import bleep.model
import bleep.model.{CrossId, CrossProjectName, VersionScala}
import bloop.config.Config
import coursier.core.Configuration
import sbt.librarymanagement.CrossVersion

import java.nio.file.{Files, Path}
import scala.collection.mutable

case class ImportInputProjects(values: Map[model.CrossProjectName, ImportInputProjects.InputProject]) {
  val byBloopName: Map[String, model.CrossProjectName] =
    values.map { case (crossName, ip) => (ip.bloopName, crossName) }
}

object ImportInputProjects {
  case class InputProject(bloopFile: Config.File, sbtExportFile: ReadSbtExportFile.ExportedProject) {
    val projectType = ProjectType.of(bloopFile.project.name)
    val sbtName: String = sbtExportFile.sbtName
    val bloopName: String = bloopFile.project.name
    val bleepProjectName: model.ProjectName = {
      val suffix: String = projectType match {
        case ProjectType.Main => ""
        case ProjectType.Test => "-test"
        case ProjectType.It   => "-it"
      }

      if (sbtName.endsWith("-test"))
        model.ProjectName(sbtName.replace("-test", "+test") + suffix)
      else if (sbtName.endsWith("-it"))
        model.ProjectName(sbtName.replace("-it", "+it") + suffix)
      else model.ProjectName(sbtName + suffix)
    }
  }

  sealed abstract class ProjectType(val configuration: Configuration, val sbtScope: String, val testLike: Boolean)

  object ProjectType {
    case object Main extends ProjectType(Configuration.empty, "main", testLike = false)
    case object Test extends ProjectType(Configuration.test, "test", testLike = true)
    case object It extends ProjectType(Configuration("it"), "it", testLike = true)

    def of(bloopName: String): ProjectType =
      if (bloopName.endsWith("-test")) Test
      else if (bloopName.endsWith("-it")) It
      else Main
  }

  def keepRelevant(
      projects: Map[model.CrossProjectName, ImportInputProjects.InputProject],
      forceInclude: Set[model.CrossProjectName]
  ): Map[CrossProjectName, InputProject] = {
    // not transitive
    val reverseBloopDeps: Map[String, Iterable[String]] =
      projects
        .flatMap { case (_, f) => f.bloopFile.project.dependencies.map(from => (from, f.bloopFile.project.name)) }
        .groupBy { case (from, _) => from }
        .map { case (name, tuples) => (name, tuples.map { case (_, to) => to }.toSet) }

    // may have multiple cross builds per project name. we'll consider them together
    val byName: Map[String, Map[CrossProjectName, InputProject]] =
      projects.groupBy { case (_, f) => f.bloopFile.project.name }

    def cachedFn[In, Out](f: In => Out): (In => Out) = {
      val cache = mutable.Map.empty[In, Out]
      in => cache.getOrElseUpdate(in, f(in))
    }

    val hasSources: Path => Boolean =
      cachedFn { path =>
        def isSource(path: Path): Boolean =
          path.toString match {
            case p if p.endsWith(".scala") => true
            case p if p.endsWith(".java")  => true
            case _                         => false
          }

        Files.exists(path) && Files.walk(path).filter(isSource).findFirst().isPresent
      }

    def include(projectName: String): Boolean = {
      val keepBecauseFoundSources = byName(projectName).exists { case (_, f) => f.bloopFile.project.sources.exists(hasSources.apply) }
      val keepBecauseFoundDownstreamWithSources = reverseBloopDeps.getOrElse(projectName, Nil).exists(include)
      keepBecauseFoundSources || keepBecauseFoundDownstreamWithSources
    }

    projects.filter { case (crossProjectName, f) => forceInclude(crossProjectName) || include(f.bloopFile.project.name) }
  }

  def apply(
      bloopFiles: Iterable[Config.File],
      sbtExportFiles: Iterable[ReadSbtExportFile.ExportedProject],
      // note: this is here for test purposes (at least so far)
      forceInclude: Set[model.CrossProjectName]
  ): ImportInputProjects = {
    val all = bloopFiles
      .collect { case bloopFile =>
        val sbtExportFile =
          sbtExportFiles.filter(f => f.bloopName == bloopFile.project.name).toList match {
            case Nil =>
              sys.error(s"Couldn't pick sbt export file for project ${bloopFile.project.name}")
            case List(one) =>
              one
            case many =>
              many
                .find(f => bloopFile.project.scala.map(_.version).contains(f.scalaVersion.full))
                .orElse(many.find(f => bloopFile.project.scala.map(s => VersionScala(s.version).binVersion).contains(f.scalaVersion.binary)))
                .getOrElse {
                  sys.error(s"Couldn't pick sbt export file for project ${bloopFile.project.name} among ${many.map(_.scalaVersion)}")
                }
          }

        InputProject(bloopFile, sbtExportFile)
      }

    val cross = all
      .groupBy(ip => ip.bleepProjectName)
      .flatMap {
        case (name, one) if one.size == 1 =>
          List((model.CrossProjectName(name, None), one.head))
        case (name, importableProjects) =>
          // multiple projects may translate to same crossId. we discard all except one
          val byCrossId: Map[Option[CrossId], Iterable[InputProject]] =
            importableProjects.groupBy(ip => mkCrossId(ip, None))

          byCrossId.map {
            case (maybeCrossId, one) if one.size == 1 =>
              (model.CrossProjectName(name, maybeCrossId), one.head)
            case (maybeCrossId, conflicting) =>
              // note that this being a `Map` the discard would be automatic,
              // but for consistency this code will pick the same each time
              val chosen = conflicting.maxBy(ip => mkCrossId(ip, overrideIsFull = Some(true)))
              (model.CrossProjectName(name, maybeCrossId), chosen)
          }
      }

    val relevant = keepRelevant(cross, forceInclude)
    ImportInputProjects(relevant)
  }

  def mkCrossId(ip: ImportInputProjects.InputProject, overrideIsFull: Option[Boolean]): Option[CrossId] =
    model.CrossId.defaultFrom(
      maybeScalaVersion = ip.bloopFile.project.scala.map(s => model.VersionScala(s.version)),
      maybePlatformId = ip.bloopFile.project.platform.flatMap(p => model.PlatformId.fromName(p.name)),
      isFull = overrideIsFull.getOrElse {
        ip.sbtExportFile.crossVersion match {
          case _: CrossVersion.Full => true
          case _                    => false
        }
      }
    )
}
