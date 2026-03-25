package bleep.model

import io.circe.{Decoder, Encoder}

/** Name of an individual test case within a suite. */
case class TestName(value: String) extends AnyVal {
  override def toString: String = value
}

object TestName {
  implicit val ordering: Ordering[TestName] = Ordering.by(_.value)
  implicit val encodes: Encoder[TestName] = Encoder[String].contramap(_.value)
  implicit val decodes: Decoder[TestName] = Decoder[String].map(TestName.apply)
}
