package bleep.internal

import io.circe.{Decoder, Encoder}

trait EnumCodec[T] {
  val All: Map[String, T]

  implicit def decodesEnum: Decoder[T] =
    Decoder[String].emap(str => All.get(str).toRight(s"$str not among ${All.keys.mkString(", ")}"))

  implicit def encodesEnum: Encoder[T] =
    Encoder[String].contramap(t => All.collectFirst { case (str, `t`) => str }.get)
}
