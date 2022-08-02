package bleep.model

import io.circe.{Decoder, Encoder, KeyDecoder, KeyEncoder}

case class JsonMap[K, V <: SetLike[V]](value: Map[K, V]) extends SetLike[JsonMap[K, V]] {
  def updated(k: K, v: V): JsonMap[K, V] =
    JsonMap(value.updated(k, v))

  override def intersect(other: JsonMap[K, V]): JsonMap[K, V] =
    JsonMap {
      value.flatMap { case (k, v1) =>
        for {
          v2 <- other.value.get(k)
          v <- v1.intersectDropEmpty(v2)
        } yield (k, v)
      }
    }

  override def removeAll(other: JsonMap[K, V]): JsonMap[K, V] =
    JsonMap {
      value.flatMap { case (k, v1) =>
        other.value.get(k) match {
          case Some(v2) => v1.removeAllDropEmpty(v2).map(v => (k, v))
          case None     => Some((k, v1))
        }
      }
    }

  override def union(other: JsonMap[K, V]): JsonMap[K, V] =
    JsonMap {
      (value.keys ++ other.value.keys).map { k =>
        val v = (value.get(k), other.value.get(k)) match {
          case (Some(v1), Some(v2)) => v1.union(v2)
          case (Some(v1), None)     => v1
          case (None, Some(v2))     => v2
          case (None, None)         => sys.error("unexpected")
        }
        (k, v)
      }.toMap
    }

  override def isEmpty: Boolean =
    value.isEmpty
}

object JsonMap {
  def empty[K, V <: SetLike[V]]: JsonMap[K, V] = JsonMap(Map.empty)

  implicit def decodes[K: KeyDecoder, V <: SetLike[V]: Decoder]: Decoder[JsonMap[K, V]] =
    Decoder.decodeOption(Decoder.decodeMap[K, V].map(JsonMap.apply)).map(_.getOrElse(empty))

  implicit def encodes[K: KeyEncoder, V <: SetLike[V]: Encoder]: Encoder[JsonMap[K, V]] =
    Encoder.encodeMap[K, V].contramap(_.value)
}
