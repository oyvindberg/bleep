package bleep.internal

import io.circe.{Json, JsonNumber, JsonObject}

case class ShortenAndSortJson(path: List[String]) extends Json.Folder[Json] {
  val shouldShorten = path match {
    case Nil                                     => false
    case "projects" :: Nil                       => false
    case _ :: "projects" :: Nil                  => false
    case _ :: "cross" :: _ :: "projects" :: Nil  => false
    case _ :: "templates" :: Nil                 => false
    case _ :: "cross" :: _ :: "templates" :: Nil => false
    case _                                       => true
  }

  override def onNull: Json = Json.Null
  override def onBoolean(value: Boolean): Json = Json.fromBoolean(value)
  override def onNumber(value: JsonNumber): Json = Json.fromJsonNumber(value)
  override def onString(value: String): Json = Json.fromString(value)
  override def onArray(value: Vector[Json]): Json = if (value.isEmpty) Json.Null else Json.fromValues(value)
  override def onObject(value: JsonObject): Json = {
    val newValues = value.toList.flatMap { case (key, value) =>
      val nextShorten = ShortenAndSortJson(key :: path)
      val value1 = value.foldWith(nextShorten)
      if (value1.isNull) None else Some((key, value1))
    }
    if (shouldShorten && newValues.isEmpty) Json.Null
    else Json.fromFields(newValues.sortBy(_._1))
  }
}
