package bleep.bsp

import bleep.internal.FileUtils
import bleep.{BuildException, BuildPaths}
import io.circe.syntax.EncoderOps

import java.nio.file.Files

object ProjectSelection {
  def store(buildPaths: BuildPaths, maybeSelectedProjectGlobs: Option[List[String]]): Unit =
    maybeSelectedProjectGlobs match {
      case Some(selectedProjectGlobs) =>
        FileUtils.writeString(buildPaths.bspProjectSelectionJsonFile, selectedProjectGlobs.asJson.spaces2)
      case None =>
        Files.deleteIfExists(buildPaths.bspProjectSelectionJsonFile)
        ()
    }

  def load(buildPaths: BuildPaths): Either[BuildException, Option[List[String]]] =
    if (FileUtils.exists(buildPaths.bspProjectSelectionJsonFile)) {
      io.circe.parser.decode[List[String]](Files.readString(buildPaths.bspProjectSelectionJsonFile)) match {
        case Left(err)       => Left(new BuildException.InvalidJson(buildPaths.bspProjectSelectionJsonFile, err))
        case Right(selected) => Right(Some(selected))
      }
    } else Right(None)
}
