package bleep

import cats.syntax.traverse._
import io.circe.{Decoder, Json}

// T should not be a json array
//    "foo": null,
//    "foo": "a",
//    "foo": ["a"],
// are all allowed and put into a `List`
case class JsonList[T](values: List[T])

object JsonList {
  implicit class Flatten[T](ts: Option[JsonList[T]]) {
    def flat: List[T] = ts match {
      case Some(ts) => ts.values
      case None     => Nil
    }
  }

  implicit def decodes[T](implicit T: Decoder[T]): Decoder[JsonList[T]] =
    Decoder.instance(c =>
      for {
        json <- c.as[Json]
        ts <- json.fold[Decoder.Result[JsonList[T]]](
          Right(JsonList(Nil)),
          _ => T(c).map(t => JsonList(List(t))),
          _ => T(c).map(t => JsonList(List(t))),
          _ => T(c).map(t => JsonList(List(t))),
          array => array.toList.traverse(T.decodeJson).map(JsonList.apply),
          _ => T(c).map(t => JsonList(List(t)))
        )
      } yield ts
    )
}
