package bleep
package bsp

import bleep.internal.FileUtils

import java.nio.file.{Files, Path}

object BspProjectSelection {
  def file(buildPaths: BuildPaths): Path = buildPaths.buildVariantDir / "project-selection.yaml"

  def store(buildPaths: BuildPaths, maybeSelectedProjectGlobs: Option[List[String]]): Unit =
    maybeSelectedProjectGlobs match {
      case Some(selectedProjectGlobs) =>
        yaml.writeShortened(selectedProjectGlobs, file(buildPaths))
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
