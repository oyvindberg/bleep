package bleep.bsp

import bleep.internal.{asYamlString, FileUtils}
import bleep.{BuildException, BuildPaths}

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

  def load(buildPaths: BuildPaths): Either[BuildException, Option[List[String]]] =
    if (FileUtils.exists(buildPaths.bspProjectSelectionYamlFile)) {
      io.circe.parser.decode[List[String]](Files.readString(buildPaths.bspProjectSelectionYamlFile)) match {
        case Left(err)       => Left(new BuildException.InvalidJson(buildPaths.bspProjectSelectionYamlFile, err))
        case Right(selected) => Right(Some(selected))
      }
    } else Right(None)
}
