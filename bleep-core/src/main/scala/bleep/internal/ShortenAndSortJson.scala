package bleep.internal

import io.circe.{Json, JsonNumber, JsonObject}

object ShortenAndSortJson extends Json.Folder[Json] {
  override def onNull: Json = Json.Null
  override def onBoolean(value: Boolean): Json = Json.fromBoolean(value)
  override def onNumber(value: JsonNumber): Json = Json.fromJsonNumber(value)
  override def onString(value: String): Json = Json.fromString(value)
  override def onArray(value: Vector[Json]): Json = if (value.isEmpty) Json.Null else Json.fromValues(value)
  override def onObject(value: JsonObject): Json =
    value.mapValues(_.foldWith(ShortenAndSortJson)).filter { case (_, v) => !v.isNull } match {
      case empty if empty.isEmpty => Json.Null
      case nonEmpty               => Json.fromFields(nonEmpty.toList.sortBy(_._1))
    }
}
