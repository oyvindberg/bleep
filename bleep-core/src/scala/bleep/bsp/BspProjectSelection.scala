package bleep
package bsp

import bleep.internal.{writeYamlLogged, FileUtils}
import bleep.logging.Logger

import java.nio.file.Files

object BspProjectSelection {
  def store(logger: Logger, buildPaths: BuildPaths, maybeSelectedProjectGlobs: Option[List[String]]): Unit =
    maybeSelectedProjectGlobs match {
      case Some(selectedProjectGlobs) =>
        writeYamlLogged(logger, "Wrote update BSP project selection file", selectedProjectGlobs, buildPaths.projectSelectionYaml)
      case None =>
        Files.deleteIfExists(buildPaths.projectSelectionYaml)
        ()
    }

  def load(buildPaths: BuildPaths): Either[BleepException, Option[List[String]]] =
    if (FileUtils.exists(buildPaths.projectSelectionYaml)) {
      yaml.decode[List[String]](Files.readString(buildPaths.projectSelectionYaml)) match {
        case Left(err)       => Left(new BleepException.InvalidJson(buildPaths.projectSelectionYaml, err))
        case Right(selected) => Right(Some(selected))
      }
    } else Right(None)
}
