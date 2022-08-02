package bleep
package commands

import bleep.internal.FileUtils
import bleep.BleepException

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
        cli(List(List("rm", "-Rf"), outDirectories.map(_.toString)).flatten, started.logger, "clean files")(FileUtils.TempDir)
        outDirectories.foreach(directory => started.logger.info(s"Deleted $directory"))
      }
    }
  }
}
