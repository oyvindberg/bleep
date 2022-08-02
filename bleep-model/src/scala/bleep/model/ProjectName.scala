package bleep.model

import io.circe.{Decoder, Encoder, KeyDecoder, KeyEncoder}

case class ProjectName(value: String) extends AnyVal

object ProjectName {
  implicit val ordering: Ordering[ProjectName] = Ordering.by(_.value)
  implicit val encodes: Encoder[ProjectName] = Encoder[String].contramap(_.value)
  implicit val keyDecodes: KeyDecoder[ProjectName] = KeyDecoder[String].map(ProjectName.apply)
  implicit val keyEncodes: KeyEncoder[ProjectName] = KeyEncoder[String].contramap(_.value)

  def decoder(legal: Iterable[ProjectName]): Decoder[ProjectName] = {
    val byString: Map[String, ProjectName] = legal.map(t => t.value -> t).toMap
    Decoder[String].emap(str => byString.get(str).toRight(s"referenced project name '$str' not among ${byString.keys.mkString(", ")}"))
  }
}
