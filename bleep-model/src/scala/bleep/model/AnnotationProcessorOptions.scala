package bleep.model

import io.circe.{Decoder, Encoder}

import scala.collection.immutable.SortedMap

/** `-A<key>=<value>` flags passed to javac for annotation processors. Modeled on [[EnvironmentVars]] — child overrides template on union, deterministic
  * ordering on iteration.
  */
case class AnnotationProcessorOptions(value: SortedMap[String, String]) extends SetLike[AnnotationProcessorOptions] {

  override def intersect(other: AnnotationProcessorOptions): AnnotationProcessorOptions =
    AnnotationProcessorOptions(value.filter { case (k, v) => other.value.get(k).contains(v) })

  override def removeAll(other: AnnotationProcessorOptions): AnnotationProcessorOptions =
    AnnotationProcessorOptions(value.filter { case (k, v) => !other.value.get(k).contains(v) })

  override def union(other: AnnotationProcessorOptions): AnnotationProcessorOptions =
    AnnotationProcessorOptions(other.value ++ value) // this wins over other (child overrides template)

  override def isEmpty: Boolean = value.isEmpty

  def toMap: Map[String, String] = value
}

object AnnotationProcessorOptions {
  val empty: AnnotationProcessorOptions = AnnotationProcessorOptions(SortedMap.empty)

  implicit val decodes: Decoder[AnnotationProcessorOptions] =
    Decoder.decodeOption(Decoder.decodeMap[String, String].map(m => AnnotationProcessorOptions(SortedMap.from(m)))).map(_.getOrElse(empty))

  implicit val encodes: Encoder[AnnotationProcessorOptions] =
    Encoder.encodeMap[String, String].contramap(_.value)
}
