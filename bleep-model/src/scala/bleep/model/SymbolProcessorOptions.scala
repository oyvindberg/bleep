package bleep.model

import io.circe.{Decoder, Encoder}

import scala.collection.immutable.SortedMap

/** `apoption=<key>=<value>` flags passed to KSP via the kotlinc plugin option surface (`-P plugin:com.google.devtools.ksp.symbol-processing:apoption=<k>=<v>`).
  * Modeled on [[AnnotationProcessorOptions]]: child overrides template on union, deterministic ordering on iteration.
  */
case class SymbolProcessorOptions(value: SortedMap[String, String]) extends SetLike[SymbolProcessorOptions] {

  override def intersect(other: SymbolProcessorOptions): SymbolProcessorOptions =
    SymbolProcessorOptions(value.filter { case (k, v) => other.value.get(k).contains(v) })

  override def removeAll(other: SymbolProcessorOptions): SymbolProcessorOptions =
    SymbolProcessorOptions(value.filter { case (k, v) => !other.value.get(k).contains(v) })

  override def union(other: SymbolProcessorOptions): SymbolProcessorOptions =
    SymbolProcessorOptions(other.value ++ value) // this wins over other (child overrides template)

  override def isEmpty: Boolean = value.isEmpty

  def toMap: Map[String, String] = value
}

object SymbolProcessorOptions {
  val empty: SymbolProcessorOptions = SymbolProcessorOptions(SortedMap.empty)

  implicit val decodes: Decoder[SymbolProcessorOptions] =
    Decoder.decodeOption(Decoder.decodeMap[String, String].map(m => SymbolProcessorOptions(SortedMap.from(m)))).map(_.getOrElse(empty))

  implicit val encodes: Encoder[SymbolProcessorOptions] =
    Encoder.encodeMap[String, String].contramap(_.value)
}
