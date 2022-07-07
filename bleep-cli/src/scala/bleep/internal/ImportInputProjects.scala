package bleep.internal

import bleep.{model, Versions}
import bloop.config.Config
import coursier.core.Configuration

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

  def cachedFn[In, Out](f: In => Out): (In => Out) = {
    val cache = mutable.Map.empty[In, Out]
    in => cache.getOrElseUpdate(in, f(in))
  }

  def apply(bloopFiles: Iterable[Config.File], sbtExportFiles: Iterable[ReadSbtExportFile.ExportedProject]): ImportInputProjects = {
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

    ImportInputProjects(
      bloopFiles
        .collect {
          case bloopFile if bloopFile.project.sources.exists(hasSources.apply) =>
            val sbtExportFile =
              sbtExportFiles
                .find(f => f.bloopName == bloopFile.project.name && bloopFile.project.scala.map(_.version).contains(f.scalaVersion.full))
                .getOrElse {
                  sys.error(s"Expected to find a sbt dependency file for ${bloopFile.project.name}")
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
                maybeScalaVersion = ip.bloopFile.project.scala.map(s => Versions.Scala(s.version)),
                maybePlatformId = ip.bloopFile.project.platform.flatMap(p => model.PlatformId.fromName(p.name))
              )

              (model.CrossProjectName(name, maybeCrossId), ip)
            }
        }
    )
  }
}
