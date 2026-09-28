package bleep

import bleep.internal.{ShortenAndSortJson, YamlComments}
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, Error, Json, ParsingFailure}

import java.nio.file.{Files, Path}

object yaml {
  def writeShortened[T: Encoder](t: T, to: Path): Unit = {
    Files.createDirectories(to.getParent)
    Files.writeString(to, encodeShortened(t))
    ()
  }

  def encodeShortened[T: Encoder](t: T): String =
    YamlComments.print(shortened(t), YamlComments.Index.empty).yaml

  /** Like [[encodeShortened]], carrying over the comments in `previous`, the YAML this replaces. Comments with nowhere to go come back as orphans. */
  def encodeShortenedKeepingComments[T: Encoder](t: T, previous: String): YamlComments.Printed =
    YamlComments.print(shortened(t), YamlComments.extract(previous))

  private def shortened[T: Encoder](t: T): Json =
    t.asJson.foldWith(ShortenAndSortJson(Nil))

  def decode[T: Decoder](yaml: String): Either[Error, T] = io.circe.yaml.v12.Parser.default.decode(yaml)

  def parse(yaml: String): Either[ParsingFailure, Json] = io.circe.yaml.v12.parser.parse(yaml)
}
