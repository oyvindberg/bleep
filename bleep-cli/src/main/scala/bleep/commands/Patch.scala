package bleep
package commands

import bleep.internal.{FileUtils, asYamlString}
import diffson.circe._
import diffson.jsonpatch.JsonPatch
import io.circe.Json
import io.circe.parser.decode

import java.nio.file.{Files, Path}
import scala.io.Source
import scala.util.Using

case class Patch(started: Started, file: Option[Path]) extends BleepCommand {
  override def run(): Either[BuildException, Unit] = {
    val string = file match {
      case Some(path) =>
        Files.readString(path)
      case None =>
        Using.resource(System.in)(r => Source.fromInputStream(r).toString())
    }

    val applied: Either[Throwable, model.Build] =
      decode[JsonPatch[Json]](string).flatMap(patch => ApplyPatch(patch, started.rawBuild))

    applied match {
      case Left(th)     => Left(new BuildException.Cause(th, "Couldn't patch build"))
      case Right(build) => Right(FileUtils.writeString(started.buildPaths.bleepYamlFile, asYamlString(build)))
    }
  }
}
