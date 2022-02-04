package bleep
package commands

import diffson.circe._
import diffson.jsonpatch.JsonPatch
import io.circe.Json
import io.circe.syntax._

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import scala.io.Source
import scala.util.Using

case class Patch(started: Started, opts: CommonOpts, file: Option[Path]) extends BleepCommand {
  override def run(): Unit = {
    val string = file match {
      case Some(path) =>
        Files.readString(path)
      case None =>
        Using.resource(System.in)(r => Source.fromInputStream(r).toString())
    }
    io.circe.parser
      .decode[JsonPatch[Json]](string)
      .flatMap(patch => ApplyPatch(patch, started.rawBuild))
      .foreach(build => Files.writeString(started.buildPaths.bleepJsonFile, build.asJson.spaces2, StandardCharsets.UTF_8))
  }
}
