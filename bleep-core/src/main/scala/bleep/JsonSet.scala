package bleep

import bleep.internal.SetLike
import io.circe.Decoder.Result
import io.circe.{Decoder, Encoder, Json}

import scala.collection.immutable.SortedSet

// T should not be a json array
//    "foo": null,
//    "foo": "a",
//    "foo": ["a"],
// are all allowed and put into a `SortedSet`
//
case class JsonSet[T](values: SortedSet[T]) extends Function[T, Boolean] with SetLike[JsonSet[T]] {
  override def apply(t: T): Boolean = values.apply(t)
  override def intersect(other: JsonSet[T]): JsonSet[T] = JsonSet(values.intersect(other.values))
  override def removeAll(other: JsonSet[T]): JsonSet[T] = JsonSet(values.filterNot(other.values))
  override def union(other: JsonSet[T]): JsonSet[T] = JsonSet(values.union(other.values))

  def isEmpty = values.isEmpty
  def size = values.size
  def ++(other: JsonSet[T]): JsonSet[T] = union(other)
  def filterNot(pred: T => Boolean): JsonSet[T] = new JsonSet[T](values.filterNot(pred))
  def map[U: Ordering](f: T => U): JsonSet[U] = new JsonSet[U](values.map(t => f(t)))
  def flatMap[U: Ordering](f: T => JsonSet[U]): JsonSet[U] = new JsonSet[U](values.flatMap(t => f(t).values))

  override def toString(): String =
    values.toString()
}

object JsonSet {
  def fromIterable[T: Ordering](values: Iterable[T]) = new JsonSet(SortedSet.empty[T] ++ values)
  def apply[T: Ordering](values: T*) = new JsonSet(SortedSet.empty[T] ++ values)
  def empty[T: Ordering]: JsonSet[T] = JsonSet()

  implicit def decodes[T: Decoder: Ordering]: Decoder[JsonSet[T]] = {
    val base = Decoder.instance(c =>
      for {
        json <- c.as[Json]
        ts <- json.fold[Result[JsonSet[T]]](
          Right(empty[T]),
          _ => c.as[T].map(t => JsonSet(SortedSet(t))),
          _ => c.as[T].map(t => JsonSet(SortedSet(t))),
          _ => c.as[T].map(t => JsonSet(SortedSet(t))),
          _ => c.as[List[T]].map(JsonSet.fromIterable(_)),
          _ => c.as[T].map(t => JsonSet(SortedSet(t)))
        )
      } yield ts
    )
    Decoder.decodeOption(base).map(_.getOrElse(empty))
  }

  implicit def encodes[T: Encoder]: Encoder[JsonSet[T]] =
    Encoder.instance[JsonSet[T]] { set =>
      set.values.toList match {
        case Nil         => Json.Null
        case head :: Nil => Encoder[T].apply(head)
        case values      => Json.fromValues(values.map(Encoder[T].apply))
      }
    }
}
