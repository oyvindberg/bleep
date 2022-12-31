package bleep
package bsp

import bleep.internal.{writeYamlLogged, FileUtils}
import bleep.logging.Logger

import java.nio.file.{Files, Path}

object BspProjectSelection {
  def file(buildPaths: BuildPaths): Path = buildPaths.buildsDir / "bsp" / "project-selection.yaml"

  def store(logger: Logger, buildPaths: BuildPaths, maybeSelectedProjectGlobs: Option[List[String]]): Unit =
    maybeSelectedProjectGlobs match {
      case Some(selectedProjectGlobs) =>
        writeYamlLogged(logger, "Wrote update BSP project selection file", selectedProjectGlobs, file(buildPaths))
      case None =>
        Files.deleteIfExists(file(buildPaths))
        ()
    }

  def load(buildPaths: BuildPaths): Either[BleepException, Option[List[String]]] =
    if (FileUtils.exists(file(buildPaths))) {
      yaml.decode[List[String]](Files.readString(file(buildPaths))) match {
        case Left(err)       => Left(new BleepException.InvalidJson(file(buildPaths), err))
        case Right(selected) => Right(Some(selected))
      }
    } else Right(None)
}
