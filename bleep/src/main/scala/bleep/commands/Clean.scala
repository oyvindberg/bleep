package bleep
package commands

import bleep.internal.FileUtils
import cats.effect.{ExitCode, IO}

import java.nio.file.Files

case object Clean extends BleepCommand {
  override def run(): IO[ExitCode] =
    runWithEnv { started =>
      IO {
        val isChosen = started.chosenProjects.toSet

        started.bloopFiles.foreach {
          case (projectName, lazyProject) if isChosen(projectName) =>
            val outFolder = lazyProject.forceGet.project.out
            if (Files.exists(outFolder)) {
              FileUtils.deleteDirectory(outFolder)
              started.logger.info(s"Deleted $outFolder")
            }
        }
        ExitCode.Success
      }
    }
}
