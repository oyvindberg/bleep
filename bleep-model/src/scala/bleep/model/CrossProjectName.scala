package bleep.model

import io.circe.*

case class CrossProjectName(name: ProjectName, crossId: Option[CrossId]) {
  val value: String =
    crossId match {
      case Some(crossId) => s"${name.value}@${crossId.value}"
      case None          => name.value
    }

  override def toString: String = value
}

object CrossProjectName {
  implicit val ordering: Ordering[CrossProjectName] = Ordering.by(x => (x.name, x.crossId))
  implicit val decodes: Decoder[CrossProjectName] =
    Decoder.instance(c =>
      for {
        str <- c.as[String]
        crossName <- fromString(str).toRight(DecodingFailure(s"more than one '@' encountered in CrossProjectName $str", c.history))
      } yield crossName
    )
  implicit val encodes: Encoder[CrossProjectName] = Encoder[String].contramap(_.value)
  implicit val keyEncodes: KeyEncoder[CrossProjectName] = KeyEncoder[String].contramap(_.value)
  implicit val keyDecodes: KeyDecoder[CrossProjectName] = KeyDecoder.instance(fromString)

  def fromString(str: String): Option[CrossProjectName] =
    str.split("@") match {
      case Array(name)          => Some(CrossProjectName(ProjectName(name), None))
      case Array(name, crossId) => Some(CrossProjectName(ProjectName(name), Some(CrossId(crossId))))
      case _                    => None
    }
}
