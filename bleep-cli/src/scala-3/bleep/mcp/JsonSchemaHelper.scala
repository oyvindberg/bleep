package io.circe

import io.circe.{Json, JsonObject}

/** Minimal JSON Schema construction helpers for MCP tool input schemas.
  *
  * The scala-effect-mcp library expects `io.circe.JsonObject` for schemas. We build them manually rather than pulling in scala-json-schema as a dependency.
  */
object JsonSchema {

  /** Empty schema (no input parameters). */
  def empty: JsonObject = JsonObject(
    "type" -> Json.fromString("object"),
    "properties" -> Json.obj()
  )

  /** Schema for an object with the given properties. */
  def obj(properties: (String, JsonObject)*): JsonObject = {
    val props = properties.map { case (name, schema) =>
      name -> Json.fromJsonObject(schema)
    }
    JsonObject(
      "type" -> Json.fromString("object"),
      "properties" -> Json.obj(props*)
    )
  }

  /** Schema for an array of strings with a description. */
  def arrayOfStrings(description: String): JsonObject = JsonObject(
    "type" -> Json.fromString("array"),
    "description" -> Json.fromString(description),
    "items" -> Json.obj("type" -> Json.fromString("string"))
  )

  /** Schema for a string with a description. */
  def string(description: String): JsonObject = JsonObject(
    "type" -> Json.fromString("string"),
    "description" -> Json.fromString(description)
  )

  /** Schema for a string enum. */
  def stringEnum(values: String*): JsonObject = JsonObject(
    "type" -> Json.fromString("string"),
    "enum" -> Json.arr(values.map(Json.fromString)*)
  )

  /** Schema for an integer with a description. */
  def integer(description: String): JsonObject = JsonObject(
    "type" -> Json.fromString("integer"),
    "description" -> Json.fromString(description)
  )
}
