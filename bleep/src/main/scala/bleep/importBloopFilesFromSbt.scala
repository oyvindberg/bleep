package bleep

import bloop.config.Config

import java.nio.file.{Files, Path}
import java.util.stream.Collectors
import scala.jdk.CollectionConverters._

object importBloopFilesFromSbt {
  def apply(buildDir: Path) = {
    val bloopFilesDir = buildDir / Defaults.BloopFolder

    val projectNames: List[model.ProjectName] =
      Files.list(bloopFilesDir).map(path => model.ProjectName(path.getFileName.toString)).collect(Collectors.toList[model.ProjectName]).asScala.toList

    val bloopProjectFiles: Map[model.ProjectName, Config.File] =
      projectNames.map(name => name -> readBloopFile(bloopFilesDir, name)).toMap

    bloopProjectFiles.map { case (projectName, bloopFile) =>
      val bloopProject = bloopFile.project

      val folder: Option[RelPath] = {
        RelPath.relativeTo(buildDir, bloopProject.directory) match {
          case Left(errMsg)                            => sys.error(errMsg)
          case Right(RelPath(List(projectName.value))) => None
          case Right(relPath)                          => Some(relPath)
        }
      }

      val dependsOn: Option[JsonList[model.ProjectName]] =
        Some(JsonList(bloopProject.dependencies.map(model.ProjectName.apply))).filterNot(_.isEmpty)

      val scalaVersion: Option[Versions.Scala] =
        bloopProject.scala.map(s => Versions.Scala(s.version))

      val isTest = projectName.value.endsWith("-test")
      val scope = if (isTest) "test" else "main"

      val sourceDirs: Option[JsonList[RelPath]] = {
        val defaultRelPaths = Defaults.sourceDirs(scalaVersion, scope)
        val givenRelPaths: List[RelPath] =
          bloopProject.sources.map { absoluteDir =>
            RelPath.relativeTo(bloopProject.directory, absoluteDir) match {
              case Left(errMsg) => sys.error(errMsg)
              case Right(rel)   => rel
            }
          }

        givenRelPaths.filterNot(defaultRelPaths.contains) match {
          case Nil => None
          case _   => Some(JsonList(givenRelPaths))
        }
      }

      val resourceDirs: Option[JsonList[RelPath]] = {
        val defaultRelPaths = Defaults.resourceDirs(scalaVersion, scope)
        val givenRelPaths: List[RelPath] =
          bloopProject.resources.getOrElse(Nil).map { absoluteDir =>
            RelPath.relativeTo(bloopProject.directory, absoluteDir) match {
              case Left(errMsg) => sys.error(errMsg)
              case Right(rel)   => rel
            }
          }

        givenRelPaths.filterNot(defaultRelPaths.contains) match {
          case Nil => None
          case _   => Some(JsonList(givenRelPaths))
        }
      }

      val dependencies = bloopProject.resolution
        .getOrElse(sys.error(s"Expected bloop file for ${projectName.value} to have resolution"))
        .modules
        .map { mod =>

          mod
        }

      model.Project(
        folder = folder,
        dependsOn = dependsOn,
        sources = sourceDirs,
        resources = resourceDirs,
        dependencies = ???,
        java = ???,
        scala = ???,
        platform = ???
      )
    }
  }
}
