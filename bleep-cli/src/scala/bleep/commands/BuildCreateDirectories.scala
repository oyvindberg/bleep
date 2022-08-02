package bleep
package commands

import bleep.internal.{formatsCrossProjectName, FileUtils}

import java.nio.file.{Files, Path}

case class BuildCreateDirectories(started: Started, projects: List[model.CrossProjectName]) extends BleepCommand {
  override def run(): Either[BleepException, Unit] = {
    val dirsWithProject: Map[Path, List[model.CrossProjectName]] =
      projects
        .flatMap { crossName =>
          val paths = started.projectPaths(crossName)
          val dirs = List(
            // don't generate folders for generated sources
            paths.sourcesDirs.fromSourceLayout,
            paths.sourcesDirs.fromJson,
            paths.resourcesDirs.fromSourceLayout,
            paths.resourcesDirs.fromJson
          ).flatten
          dirs.map(dir => (dir, crossName))
        }
        .groupBy { case (dir, _) => dir }
        .map { case (dir, tuples) => (dir, tuples.map { case (_, crossName) => crossName }) }

    dirsWithProject.foreach { case (dir, crossNames) =>
      val logger = started.logger.withContext("for projects", crossNames)

      if (FileUtils.exists(dir)) {
        logger.info(s"Already exists: $dir")
      } else {
        Files.createDirectories(dir)
        logger.info(s"Created $dir")
      }
    }

    Right(())
  }
}
