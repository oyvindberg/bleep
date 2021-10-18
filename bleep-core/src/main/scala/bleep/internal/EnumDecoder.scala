package bleep.internal

import io.circe.Decoder

trait EnumDecoder[T] {
  val All: Map[String, T]

  implicit def decodes: Decoder[T] =
    Decoder[String].emap(str => All.get(str).toRight(s"$str not among ${All.keys.mkString(", ")}"))
}
