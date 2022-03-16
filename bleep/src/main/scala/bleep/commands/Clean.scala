package bleep
package commands

import bleep.internal.FileUtils

import java.nio.file.{Files, Path, Paths}
import scala.util.Properties

case class Clean(started: Started, projects: List[model.CrossProjectName]) extends BleepCommand {
  override def run(): Either[BuildException, Unit] = {
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
        cli(s"rm -Rf ${outDirectories.map(p => s"'$p'").mkString(" ")}")(Paths.get("/tmp"))
        outDirectories.foreach(directory => started.logger.info(s"Deleted $directory"))
      }
    }
  }
}
