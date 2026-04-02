package bleep.model

import io.circe.{Codec, Decoder, Encoder, KeyDecoder, KeyEncoder}

/** Name for a resolver, used to reference it from `publish.repository` and authentication config. */
final case class ResolverName(value: String) extends AnyVal

object ResolverName {
  implicit val codec: Codec[ResolverName] = Codec.from(
    Decoder[String].map(ResolverName.apply),
    Encoder[String].contramap(_.value)
  )
  implicit val keyEncoder: KeyEncoder[ResolverName] = KeyEncoder.encodeKeyString.contramap(_.value)
  implicit val keyDecoder: KeyDecoder[ResolverName] = KeyDecoder.decodeKeyString.map(ResolverName.apply)
  implicit val ordering: Ordering[ResolverName] = Ordering.by(_.value)
}
