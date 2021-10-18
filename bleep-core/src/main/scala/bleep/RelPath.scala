package bleep

import io.circe.Decoder

case class RelPath private (segments: Array[String])

object RelPath {
  def apply(str: String): Either[String, RelPath] =
    if (str.startsWith("/")) Left(s"$str should be a relative path")
    else Right(RelPath(str.split("/").filterNot(seg => seg.isEmpty || seg == ".")))

  implicit def decodesRelPath: Decoder[RelPath] =
    Decoder[String].emap(apply)
}
