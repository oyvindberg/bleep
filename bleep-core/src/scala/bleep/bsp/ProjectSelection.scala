package bleep.bsp

import bleep.internal.FileUtils
import bleep.toYaml.asYamlString
import bleep.{BleepException, BuildPaths}
import io.circe.yaml12.parser.decode

import java.nio.file.Files

object ProjectSelection {
  def store(buildPaths: BuildPaths, maybeSelectedProjectGlobs: Option[List[String]]): Unit =
    maybeSelectedProjectGlobs match {
      case Some(selectedProjectGlobs) =>
        FileUtils.writeString(buildPaths.bspProjectSelectionYamlFile, asYamlString(selectedProjectGlobs))
      case None =>
        Files.deleteIfExists(buildPaths.bspProjectSelectionYamlFile)
        ()
    }

  def load(buildPaths: BuildPaths): Either[BleepException, Option[List[String]]] =
    if (FileUtils.exists(buildPaths.bspProjectSelectionYamlFile)) {
      decode[List[String]](Files.readString(buildPaths.bspProjectSelectionYamlFile)) match {
        case Left(err)       => Left(new BleepException.InvalidJson(buildPaths.bspProjectSelectionYamlFile, err))
        case Right(selected) => Right(Some(selected))
      }
    } else Right(None)
}
