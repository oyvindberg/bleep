package bleep.model

import io.circe.{Decoder, Encoder}

/** Fully-qualified name of a test suite (e.g., "com.example.MySpec"). */
case class SuiteName(value: String) extends AnyVal {
  override def toString: String = value
  def shortName: String = value.split('.').lastOption.getOrElse(value)
}

object SuiteName {
  implicit val ordering: Ordering[SuiteName] = Ordering.by(_.value)
  implicit val encodes: Encoder[SuiteName] = Encoder[String].contramap(_.value)
  implicit val decodes: Decoder[SuiteName] = Decoder[String].map(SuiteName.apply)
}
