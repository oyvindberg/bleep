package bleep.model

import io.circe.{Decoder, Encoder, KeyDecoder, KeyEncoder}

case class TemplateId(value: String) extends AnyVal

object TemplateId {
  def decoder(legal: Iterable[TemplateId]): Decoder[TemplateId] = {
    val byString: Map[String, TemplateId] =
      legal.map(t => t.value -> t).toMap
    Decoder[String].emap { str =>
      byString.get(str).toRight(s"referenced template id '$str' not among ${byString.keys.mkString(", ")}")
    }
  }

  implicit val ordering: Ordering[TemplateId] = Ordering.by(_.value)
  implicit val encodes: Encoder[TemplateId] = Encoder[String].contramap(_.value)
  implicit val keyDecodes: KeyDecoder[TemplateId] = KeyDecoder[String].map(TemplateId.apply)
  implicit val keyEncodes: KeyEncoder[TemplateId] = KeyEncoder[String].contramap(_.value)
}
