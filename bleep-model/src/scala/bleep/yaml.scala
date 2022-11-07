package bleep

import bleep.internal.{forkedcirceyaml, ShortenAndSortJson}
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, Error, Json, ParsingFailure}

import java.nio.file.{Files, Path}

object yaml {
  private val printer = new forkedcirceyaml.Printer(
    preserveOrder = true,
    dropNullKeys = true
  )

  def writeShortened[T: Encoder](t: T, to: Path): Unit = {
    Files.createDirectories(to.getParent)
    Files.writeString(to, encodeShortened(t))
    ()
  }

  def encodeShortened[T: Encoder](t: T): String =
    printer.pretty(t.asJson.foldWith(ShortenAndSortJson(Nil)))

  def decode[T: Decoder](yaml: String): Either[Error, T] = forkedcirceyaml.parser.decode(yaml)

  def parse(yaml: String): Either[ParsingFailure, Json] = forkedcirceyaml.parser.parse(yaml)
}
