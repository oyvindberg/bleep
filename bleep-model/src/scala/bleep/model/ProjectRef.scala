package bleep.model

import io.circe.*

/** A `dependsOn` entry: a project, and optionally which of its cross versions, written `name` or `name@crossId`.
  *
  * Not a [[CrossProjectName]], though it is spelled like one. A `CrossProjectName` without a cross id is the project that has no cross versions; a `ProjectRef`
  * without one leaves the choice to bleep, which picks among the named project's cross versions (see [[Build.resolvedDependsOn]]). With a cross id it names
  * exactly that cross version.
  */
case class ProjectRef(name: ProjectName, crossId: Option[CrossId]) {
  val value: String =
    crossId match {
      case Some(crossId) => s"${name.value}@${crossId.value}"
      case None          => name.value
    }

  override def toString: String = value

  def mapName(f: ProjectName => ProjectName): ProjectRef = copy(name = f(name))
}

object ProjectRef {
  def apply(name: ProjectName): ProjectRef = ProjectRef(name, None)

  implicit val ordering: Ordering[ProjectRef] = Ordering.by(x => (x.name, x.crossId))
  implicit val decodes: Decoder[ProjectRef] =
    Decoder.instance(c =>
      for {
        str <- c.as[String]
        ref <- fromString(str).toRight(DecodingFailure(s"more than one '@' encountered in dependsOn entry $str", c.history))
      } yield ref
    )
  implicit val encodes: Encoder[ProjectRef] = Encoder[String].contramap(_.value)

  def fromString(str: String): Option[ProjectRef] =
    str.split("@") match {
      case Array(name)          => Some(ProjectRef(ProjectName(name), None))
      case Array(name, crossId) => Some(ProjectRef(ProjectName(name), Some(CrossId(crossId))))
      case _                    => None
    }
}
