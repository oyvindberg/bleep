package bleep.internal

import io.circe.{Codec, Decoder, Encoder}

object EnumCodec {
  def codec[T](all: Map[String, T]): Codec[T] = {
    val decoder: Decoder[T] =
      Decoder[String].emap(str => all.get(str).toRight(s"$str not among ${all.keys.mkString(", ")}"))

    val encoder: Encoder[T] =
      Encoder[String].contramap(t => all.collectFirst { case (str, `t`) => str }.get)

    Codec.from(decoder, encoder)
  }
}
