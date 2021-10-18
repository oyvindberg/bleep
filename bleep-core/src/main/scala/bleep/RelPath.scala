package bleep

import io.circe.{Decoder, Encoder}

case class RelPath private (segments: List[String])

object RelPath {
  def apply(str: String): Either[String, RelPath] =
    if (str.startsWith("/")) Left(s"$str should be a relative path")
    else Right(RelPath(str.split("/").filterNot(seg => seg.isEmpty || seg == ".").toList))

  implicit def decodesRelPath: Decoder[RelPath] =
    Decoder[String].emap(apply)

  implicit def encodesRelPath: Encoder[RelPath] =
    Encoder[String].contramap(_.segments.mkString("/"))
}
