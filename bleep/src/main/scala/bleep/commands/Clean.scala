package bleep
package commands

import bleep.internal.FileUtils

import java.nio.file.Files

case class Clean(started: Started, opts: CommonOpts, projects: Option[List[model.CrossProjectName]]) extends BleepCommand {
  override def run(): Unit = {
    val isChosen = started.chosenProjects(projects).toSet
    started.bloopFiles.foreach {
      case (projectName, lazyProject) if isChosen(projectName) =>
        val outFolder = lazyProject.forceGet.project.out
        if (Files.exists(outFolder)) {
          FileUtils.deleteDirectory(outFolder)
          started.logger.info(s"Deleted $outFolder")
        }
      case _ => ()
    }
  }
}
