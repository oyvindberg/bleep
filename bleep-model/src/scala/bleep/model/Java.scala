package bleep.model

import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.{Decoder, Encoder}

case class Java(options: Options) extends SetLike[Java] {
  override def intersect(other: Java): Java =
    Java(options = options.intersect(other.options))

  override def removeAll(other: Java): Java =
    Java(options = options.removeAll(other.options))

  override def union(other: Java): Java =
    Java(options = options.union(other.options))

  def isEmpty: Boolean =
    this match {
      case Java(options) => options.isEmpty
    }
}

object Java {
  implicit val decodes: Decoder[Java] = deriveDecoder
  implicit val encodes: Encoder[Java] = deriveEncoder
}
