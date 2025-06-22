package bleep
package model

import io.circe.{Decoder, DecodingFailure, Encoder}

sealed abstract class IgnoreEvictionErrors(val value: String)

object IgnoreEvictionErrors {
  case object Yes extends IgnoreEvictionErrors("yes")
  case object Warn extends IgnoreEvictionErrors("warn")
  case object No extends IgnoreEvictionErrors("no")

  val All: List[IgnoreEvictionErrors] = List(Yes, Warn, No)
  val byName: Map[String, IgnoreEvictionErrors] = All.map(x => x.value -> x).toMap

  def fromString(str: String): Either[String, IgnoreEvictionErrors] =
    byName.get(str).toRight(s"'$str' not among ${byName.keys.mkString(", ")}")

  implicit val decoder: Decoder[IgnoreEvictionErrors] =
    Decoder.instance { c =>
      c.as[String].flatMap(str => fromString(str).left.map(err => DecodingFailure(err, c.history)))
    }

  implicit val encoder: Encoder[IgnoreEvictionErrors] =
    Encoder.encodeString.contramap(_.value)
}
