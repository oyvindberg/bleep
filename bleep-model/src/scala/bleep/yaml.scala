package bleep

import bleep.internal.ShortenAndSortJson
import io.circe.syntax.EncoderOps
import io.circe.yaml.v12.Printer
import io.circe.{Decoder, Encoder, Error, Json, ParsingFailure}

import java.nio.file.{Files, Path}

object yaml {
  private val printer = Printer.make(
    Printer.Config(
      preserveOrder = true,
      dropNullKeys = true
    )
  )

  def writeShortened[T: Encoder](t: T, to: Path): Unit = {
    Files.createDirectories(to.getParent)
    Files.writeString(to, encodeShortened(t))
    ()
  }

  def encodeShortened[T: Encoder](t: T): String =
    printer.pretty(t.asJson.foldWith(ShortenAndSortJson(Nil)))

  def decode[T: Decoder](yaml: String): Either[Error, T] = io.circe.yaml.v12.Parser.default.decode(yaml)

  def parse(yaml: String): Either[ParsingFailure, Json] = io.circe.yaml.v12.parser.parse(yaml)
}
