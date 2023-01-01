package bleep.model

import io.circe.{Decoder, Encoder, KeyDecoder, KeyEncoder}

sealed abstract class Os(val value: String)

object Os {
  case object Windows extends Os("windows")
  case object Linux extends Os("linux")
  case object Macos extends Os("macos")

  val byName: Map[String, Os] =
    List(Windows, Linux, Macos).map(os => os.value -> os).toMap

  def from(str: String): Either[String, Os] =
    byName.get(str).toRight(s"Not among ${byName.keys.mkString(", ")}: $str")

  implicit val encoder: Encoder[Os] = Encoder[String].contramap(_.value)
  implicit val decoder: Decoder[Os] = Decoder[String].emap(from)
  implicit val keyEncoder: KeyEncoder[Os] = KeyEncoder[String].contramap(_.value)
  implicit val keyDecoder: KeyDecoder[Os] = KeyDecoder.instance(str => from(str).toOption)
}
