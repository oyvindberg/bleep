package bleep
package commands

import bleep.model.{BuildFile, CrossProjectName}
import cats.data.NonEmptyList

import java.nio.file.{Files, Path}
import scala.collection.immutable.SortedMap

class BuildProjectMove(parent: Path, projectNames: NonEmptyList[model.ProjectName]) extends BleepBuildCommand {
  override def run(started: Started): Either[BleepException, Unit] = {
    val buildFile: BuildFile = started.build.requireFileBacked(ctx = "command project-move").file

    val result: Either[BleepException, (SortedMap[Path, Path], BuildFile)] =
      projectNames.foldLeft[Either[BleepException, (SortedMap[Path, Path], model.BuildFile)]](Right((SortedMap.empty, buildFile))) {
        case (Left(err), _) => Left(err)
        case (Right((moves, buildFile)), projectName) =>
          moveOne(started, parent, projectName, buildFile).map { case (newMoves, newBuildFile) => (moves ++ newMoves, newBuildFile) }
      }

    result.map { case (moves, newBuildFile) =>
      commit(started.logger, started.buildPaths, moves, newBuildFile)
    }
  }

  private def moveOne(
      started: Started,
      parent: Path,
      projectName: model.ProjectName,
      buildFile: model.BuildFile
  ): Either[BleepException.Text, (SortedMap[Path, Path], model.BuildFile)] = {
    val fromCrossProjects: List[CrossProjectName] =
      started.build.explodedProjects.toList.collect { case (cp @ model.CrossProjectName(`projectName`, _), _) => cp }
    if (fromCrossProjects.isEmpty)
      return Left(new BleepException.Text(s"Project $projectName does not exist"))

    val toDir = parent.resolve(projectName.value)

    started.build.explodedProjects.collect { case (cn, _) if started.projectPaths(cn).dir == toDir => cn.name } match {
      case Nil                 => ()
      case List(`projectName`) => return Right((SortedMap.empty, buildFile))
      case _                   => return Left(new BleepException.Text(s"Project ${projectName.value} already exists in $toDir"))
    }

    val newBuildFile = buildFile.copy(
      projects = buildFile.projects.map {
        case (`projectName`, p) =>
          val folderValue = RelPath.relativeTo(started.buildPaths.buildDir, toDir) match {
            case RelPath(Array(projectName.value)) => None
            case other                             => Some(other)
          }

          (projectName, p.copy(folder = folderValue))
        case other => other
      }
    )

    fromCrossProjects.map(started.projectPaths).map(_.dir).distinct match {
      case List(`toDir`) => Right((SortedMap.empty, newBuildFile))
      case List(fromDir) =>
        if (Files.exists(toDir))
          return Left(new BleepException.Text(s"Expected to move project ${projectName.value} from $fromDir to $toDir, but it already exists"))

        Right((SortedMap(fromDir -> toDir), newBuildFile))

      case more => Left(new BleepException.Text(s"Multiple directories for project $projectName: $more"))
    }
  }
}
