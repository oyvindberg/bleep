package bleep
package commands

import bleep.BleepException
import bleep.internal.FileUtils

import java.nio.file.{Files, Path}
import scala.util.Properties

case class Clean(started: Started, projects: List[model.CrossProjectName]) extends BleepCommand {
  override def run(): Either[BleepException, Unit] = {
    val outDirectories: List[Path] =
      projects.map(projectName => started.bloopFiles(projectName).forceGet.project.out).filter(Files.exists(_))

    Right {
      if (Properties.isWin) {
        outDirectories.foreach { directory =>
          FileUtils.deleteDirectory(directory)
          started.logger.info(s"Deleted $directory")
        }
      } else {
        // fast path
        cli(
          action = "clean files",
          cwd = FileUtils.TempDir,
          cmd = List(List("rm", "-Rf"), outDirectories.map(_.toString)).flatten,
          logger = started.logger
        )
        outDirectories.foreach(directory => started.logger.info(s"Deleted $directory"))
      }
    }
  }
}
