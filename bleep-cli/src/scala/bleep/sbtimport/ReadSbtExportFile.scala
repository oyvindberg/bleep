package bleep
package sbtimport

import bleep.sbtexport.ExportedProject
import sjsonnew.support.scalajson.unsafe.{Converter, Parser}

import java.nio.file.Path
import scala.util.{Failure, Success}

/** Reads what sbt-export-dependencies wrote. [[ExportedProject]] is the plugin's own source, compiled here against bleep's copy of sbt's librarymanagement */
object ReadSbtExportFile {
  def parse(path: Path, jsonStr: String): ExportedProject =
    Parser.parseFromString(jsonStr).flatMap(Converter.fromJson[ExportedProject](_)) match {
      case Failure(exception) => throw new BleepException.InvalidJson(path, exception)
      case Success(value)     => value
    }
}
