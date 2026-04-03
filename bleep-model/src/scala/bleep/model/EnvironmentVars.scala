package bleep.model

import io.circe.{Decoder, Encoder}
import scala.collection.immutable.SortedMap

/** Environment variables as a sorted map. Implements SetLike for template merging. */
case class EnvironmentVars(value: SortedMap[String, String]) extends SetLike[EnvironmentVars] {

  override def intersect(other: EnvironmentVars): EnvironmentVars =
    EnvironmentVars(value.filter { case (k, v) => other.value.get(k).contains(v) })

  override def removeAll(other: EnvironmentVars): EnvironmentVars =
    EnvironmentVars(value.filter { case (k, v) => !other.value.get(k).contains(v) })

  override def union(other: EnvironmentVars): EnvironmentVars =
    EnvironmentVars(other.value ++ value) // this wins over other (child overrides template)

  override def isEmpty: Boolean = value.isEmpty

  def toMap: Map[String, String] = value
}

object EnvironmentVars {
  val empty: EnvironmentVars = EnvironmentVars(SortedMap.empty)

  implicit val decodes: Decoder[EnvironmentVars] =
    Decoder.decodeOption(Decoder.decodeMap[String, String].map(m => EnvironmentVars(SortedMap.from(m)))).map(_.getOrElse(empty))

  implicit val encodes: Encoder[EnvironmentVars] =
    Encoder.encodeMap[String, String].contramap(_.value)
}
