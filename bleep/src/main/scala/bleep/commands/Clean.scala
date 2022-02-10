package bleep
package commands

import bleep.internal.FileUtils

import java.nio.file.{Files, Path, Paths}
import scala.util.Properties

case class Clean(started: Started, opts: CommonOpts, projects: Option[List[model.CrossProjectName]]) extends BleepCommand {
  override def run(): Unit = {
    val outDirectories: List[Path] =
      started.chosenProjects(projects).map(projectName => started.bloopFiles(projectName).forceGet.project.out).filter(Files.exists(_))

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
