package bleep.model

import io.circe.{Decoder, Encoder}

case class TestFrameworkName(value: String)

object TestFrameworkName {
  implicit val decodes: Decoder[TestFrameworkName] = Decoder[String].map(TestFrameworkName.apply)
  implicit val encodes: Encoder[TestFrameworkName] = Encoder[String].contramap(_.value)
  implicit val ordering: Ordering[TestFrameworkName] = Ordering[String].on(_.value)
}
