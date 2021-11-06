package bleep.internal

import io.circe.{Codec, Decoder, Encoder}

import java.io.File
import java.net.URI
import scala.util.Try

object codecs {
  implicit val codecFile: Codec[File] = Codec.from(Decoder[String].emapTry(str => Try(new File(str))), Encoder[String].contramap(_.toString))
  implicit val codecURI: Codec[URI] = Codec.from(Decoder[String].emapTry(str => Try(URI.create(str))), Encoder[String].contramap(_.toString))
}
