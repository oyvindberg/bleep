package bleep

import io.circe.{Decoder, Encoder}

import java.nio.file.Path

case class RelPath(segments: List[String])

object RelPath {
  val empty = RelPath(Nil)

  def force(str: String): RelPath = apply(str) match {
    case Left(errMsg) => sys.error(errMsg)
    case Right(value) => value
  }

  def apply(str: String): Either[String, RelPath] =
    if (str.startsWith("/")) Left(s"$str should be a relative path")
    else Right(RelPath(str.split("/").filterNot(seg => seg.isEmpty || seg == ".").toList))

  def relativeTo(shorter: Path, longer: Path): RelPath =
    force(shorter.relativize(longer).toString)

  implicit val decodesRelPath: Decoder[RelPath] =
    Decoder[String].emap(apply)

  implicit val encodesRelPath: Encoder[RelPath] =
    Encoder[String].contramap(_.segments.mkString("./", "/", ""))

  implicit val ordering: Ordering[RelPath] =
    Ordering.by(_.segments.mkString(""))
}
