package bleep

import bleep.internal.SetLike
import cats.syntax.traverse._
import io.circe.Decoder.Result
import io.circe.{Decoder, Encoder, Json}

// T should not be a json array
//    "foo": null,
//    "foo": "a",
//    "foo": ["a"],
// are all allowed and put into a `List`
case class JsonList[T](values: List[T]) extends SetLike[JsonList[T]] {
  def isEmpty = values.isEmpty

  override def intersect(other: JsonList[T]): JsonList[T] = JsonList(values.intersect(other.values))
  override def removeAll(other: JsonList[T]): JsonList[T] = JsonList(values.filterNot(other.values.toSet))
  override def union(other: JsonList[T]): JsonList[T] = JsonList((values ++ other.values).distinct)

  def +(t: T): JsonList[T] =
    JsonList(values :+ t)
  override def toString(): String =
    values.toString()
}

object JsonList {
  def empty[T]: JsonList[T] = JsonList(Nil)

  implicit def decodes[T](implicit T: Decoder[T]): Decoder[JsonList[T]] = {
    val base = Decoder.instance(c =>
      for {
        json <- c.as[Json]
        ts <- json.fold[Result[JsonList[T]]](
          Right(JsonList(Nil)),
          _ => T(c).map(t => JsonList(List(t))),
          _ => T(c).map(t => JsonList(List(t))),
          _ => T(c).map(t => JsonList(List(t))),
          array => array.toList.traverse(T.decodeJson).map(JsonList.apply),
          _ => T(c).map(t => JsonList(List(t)))
        )
      } yield ts
    )
    Decoder.decodeOption(base).map(_.getOrElse(empty))
  }

  implicit def encodes[T: Encoder]: Encoder[JsonList[T]] =
    Encoder.instance {
      case JsonList(Nil)         => Json.Null
      case JsonList(head :: Nil) => Encoder[T].apply(head)
      case JsonList(values)      => Json.fromValues(values.map(Encoder[T].apply))
    }
}
