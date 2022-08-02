package bleep

import bleep.internal.ShortenAndSortJson
import io.circe.Encoder
import io.circe.syntax.EncoderOps

object toYaml {
  private val printer = new io.circe.yaml12.Printer(
    preserveOrder = true,
    dropNullKeys = true
  )

  def asYamlString[T: Encoder](t: T): String =
    printer.pretty(t.asJson.foldWith(ShortenAndSortJson))
}
