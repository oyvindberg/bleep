package bleep

import io.circe.{Decoder, Encoder}

import java.nio.file.Path
import scala.collection.mutable
import scala.util.hashing.MurmurHash3

case class RelPath private (segments: Array[String]) {
  def mapSegments(f: String => String): RelPath =
    RelPath(segments.map(f))

  def apply(p: Path): Path =
    segments.foldLeft(p) {
      case (acc, "..") => acc.getParent
      case (acc, seg)  => acc.resolve(seg)
    }

  def name: Option[String] =
    segments.lastOption

  def extension: Option[String] =
    name match {
      case None => None
      case Some(name) =>
        name.split('.') match {
          case Array(_) => None
          case more     => more.lastOption
        }
    }

  def parent: Option[RelPath] =
    if (segments.isEmpty) None
    else Some(RelPath(segments.dropRight(1)))

  def ++(other: RelPath): RelPath =
    RelPath.normalize(segments ++ other.segments)

  def dropInitial(n: Int): RelPath =
    new RelPath(segments.drop(n))

  def prefixed(str: String): RelPath =
    RelPath(str +: segments)

  def asString: String =
    if (segments.isEmpty) "." else segments.mkString("/")

  def asStringWithInitialDot: String =
    segments.headOption match {
      case None             => "."
      case Some(".." | ".") => segments.mkString("/")
      case _                => segments.mkString("./", "/", "")
    }

  def /(other: RelPath): RelPath =
    this ++ other

  def /(other: String): RelPath =
    this ++ RelPath.force(other)

  def filter(f: String => Boolean): RelPath =
    RelPath(segments.filter(f))

  def withLast(f: String => String): RelPath =
    if (segments.isEmpty) this
    else {
      val newSegments = segments.clone()
      newSegments.update(segments.length - 1, f(segments.last))
      RelPath(newSegments)
    }

  override def toString: String =
    asString

  override def equals(obj: Any): Boolean =
    obj match {
      case that: RelPath => this.segments.sameElements(that.segments)
      case _             => false
    }

  override def hashCode: Int =
    MurmurHash3.arrayHash(segments)
}

object RelPath {
  val empty = new RelPath(Array.empty)

  def of(segments: String*): RelPath =
    RelPath(segments.toArray)

  def force(str: String): RelPath =
    apply(str) match {
      case Left(errMsg) => sys.error(errMsg)
      case Right(value) => value
    }

  def apply(str: String): Either[String, RelPath] =
    if (str.startsWith("/")) Left(s"$str should be a relative path")
    else Right(normalize(str.split("/")))

  def normalize(strings: Array[String]): RelPath = {
    val out = mutable.ArrayBuffer.empty[String]
    strings.foreach {
      case "" | "." => ()
      case ".." =>
        if (out.forall(_ == ".."))
          out += ".."
        else
          out.dropRightInPlace(1)
      case seg => out += seg
    }
    RelPath(out.toArray)
  }

  def relativeTo(shorter: Path, longer: Path): RelPath =
    force(shorter.relativize(longer).toString)

  implicit val decodesRelPath: Decoder[RelPath] =
    Decoder[String].emap(apply)

  implicit val encodesRelPath: Encoder[RelPath] =
    Encoder[String].contramap(_.asStringWithInitialDot)

  implicit val ordering: Ordering[RelPath] =
    Ordering.by(_.asString)
}
