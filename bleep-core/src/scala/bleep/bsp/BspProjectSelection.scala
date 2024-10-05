package bleep
package bsp

import bleep.internal.{writeYamlLogged, FileUtils}
import ryddig.Logger

import java.nio.file.Files

object BspProjectSelection {
  def store(logger: Logger, buildPaths: BuildPaths, maybeSelectedProjectGlobs: Option[List[String]]): Unit =
    maybeSelectedProjectGlobs match {
      case Some(selectedProjectGlobs) =>
        writeYamlLogged(logger, "Wrote update BSP project selection file", selectedProjectGlobs, buildPaths.bspProjectSelectionYaml)
      case None =>
        Files.deleteIfExists(buildPaths.bspProjectSelectionYaml)
        ()
    }

  def load(buildPaths: BuildPaths): Either[BleepException, Option[List[String]]] =
    if (FileUtils.exists(buildPaths.bspProjectSelectionYaml)) {
      yaml.decode[List[String]](Files.readString(buildPaths.bspProjectSelectionYaml)) match {
        case Left(err)       => Left(new BleepException.InvalidJson(buildPaths.bspProjectSelectionYaml, err))
        case Right(selected) => Right(Some(selected))
      }
    } else Right(None)
}
