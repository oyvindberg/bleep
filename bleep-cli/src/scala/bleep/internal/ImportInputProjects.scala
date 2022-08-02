package bleep.internal

import bleep.model
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
    val name: model.ProjectName = {
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

  def keepRelevant(bloopFiles: Iterable[Config.File]): Iterable[Config.File] = {
    // not transitive
    val reverseDeps: Map[String, Iterable[String]] =
      bloopFiles
        .flatMap(f => f.project.dependencies.map(from => (from, f.project.name)))
        .groupBy { case (from, _) => from }
        .map { case (name, tuples) => (name, tuples.map { case (_, to) => to }.toSet) }

    // may have multiple cross builds per project name. we'll consider them together
    val byName: Map[String, Iterable[Config.File]] =
      bloopFiles.groupBy(f => f.project.name)

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
      val keepBecauseFoundSources = byName(projectName).exists(f => f.project.sources.exists(hasSources.apply))
      val keepBecauseFoundDownstreamWithSources = reverseDeps.getOrElse(projectName, Nil).exists(include)
      keepBecauseFoundSources || keepBecauseFoundDownstreamWithSources
    }

    bloopFiles.filter(f => include(f.project.name))
  }

  def apply(bloopFiles: Iterable[Config.File], sbtExportFiles: Iterable[ReadSbtExportFile.ExportedProject]): ImportInputProjects =
    ImportInputProjects(
      keepRelevant(bloopFiles)
        .collect { case bloopFile =>
          val sbtExportFile =
            sbtExportFiles.filter(f => f.bloopName == bloopFile.project.name).toList match {
              case Nil =>
                sys.error(s"Couldn't pick sbt export file for project ${bloopFile.project.name}")
              case List(one) =>
                one
              case many =>
                many.find(f => bloopFile.project.scala.map(_.version).contains(f.scalaVersion.full)).getOrElse {
                  sys.error(s"Couldn't pick sbt export file for project ${bloopFile.project.name} among ${many.map(_.scalaVersion)}")
                }
            }

          InputProject(bloopFile, sbtExportFile)
        }
        .groupBy(ip => ip.name)
        .flatMap {
          case (name, one) if one.size == 1 =>
            List((model.CrossProjectName(name, None), one.head))
          case (name, importableProjects) =>
            importableProjects.map { ip =>
              val maybeCrossId = model.CrossId.defaultFrom(
                maybeScalaVersion = ip.bloopFile.project.scala.map(s => model.VersionScala(s.version)),
                maybePlatformId = ip.bloopFile.project.platform.flatMap(p => model.PlatformId.fromName(p.name)),
                isFull = ip.sbtExportFile.crossVersion match {
                  case _: CrossVersion.Full => true
                  case _                    => false
                }
              )

              (model.CrossProjectName(name, maybeCrossId), ip)
            }
        }
    )
}
