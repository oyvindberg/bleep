package bleep

import io.circe.{Decoder, Encoder}

import java.nio.file.Path
import scala.jdk.CollectionConverters._

case class RelPath private (segments: List[String])

object RelPath {
  def force(str: String): RelPath = apply(str) match {
    case Left(errMsg) => sys.error(errMsg)
    case Right(value) => value
  }

  def apply(str: String): Either[String, RelPath] =
    if (str.startsWith("/")) Left(s"$str should be a relative path")
    else Right(RelPath(str.split("/").filterNot(seg => seg.isEmpty || seg == ".").toList))

  def relativeTo(shorter: Path, longer: Path): Either[String, RelPath] =
    if (longer.startsWith(shorter))
      Right(new RelPath(longer.iterator().asScala.toList.drop(shorter.getNameCount).map(_.toString)))
    else
      Left(s"expected $shorter to be contained within $longer")

  implicit def decodesRelPath: Decoder[RelPath] =
    Decoder[String].emap(apply)

  implicit def encodesRelPath: Encoder[RelPath] =
    Encoder[String].contramap(_.segments.mkString("/"))
}
