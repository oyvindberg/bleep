package bleep.mcp

import io.circe.{Decoder, DecodingFailure}
import _root_.mcp.protocol.JsonSchemaType
import _root_.mcp.server.{InputDef, InputSchema}

/** Typed input definitions for MCP tools.
  *
  * Uses InputDef.raw with case classes and circe Decoders. Named tuples would be cleaner but require -source 3.5+
  * and this project uses -source 3.4.
  *
  * All decoders are strict: they reject unknown fields and fail on wrong types instead of silently falling back to defaults.
  */

// TODO(scala-mcp-sdk): Remove once InputSchema[Unit] is provided by the SDK
given InputSchema[Unit] with {
  def jsonSchema: JsonSchemaType.ObjectSchema =
    JsonSchemaType.ObjectSchema(properties = Some(Map.empty))
  def decoder: Decoder[Unit] = Decoder.const(())
}

/** No input — uses the InputSchema[Unit] given above. */
type NoInput = Unit

// ========================================================================
// Shared decoder helpers
// ========================================================================

private def rejectUnknownFields(c: io.circe.HCursor, knownFields: Set[String]): Decoder.Result[Unit] = {
  val keys = c.keys.map(_.toSet).getOrElse(Set.empty)
  val unknown = keys -- knownFields
  if (unknown.nonEmpty) Left(DecodingFailure(s"Unknown fields: ${unknown.mkString(", ")}. Known fields: ${knownFields.mkString(", ")}", c.history))
  else Right(())
}

private def decodeOptional[A](c: io.circe.HCursor, field: String)(using Decoder[A]): Decoder.Result[Option[A]] = {
  val f = c.downField(field)
  if (f.failed) Right(None) else f.as[A].map(Some(_))
}

private def decodeList(c: io.circe.HCursor, field: String): Decoder.Result[List[String]] = {
  val f = c.downField(field)
  if (f.failed) Right(Nil) else f.as[List[String]]
}

private def decodeWithFallback[A](c: io.circe.HCursor, field: String, whenMissing: A)(using Decoder[A]): Decoder.Result[A] = {
  val f = c.downField(field)
  if (f.failed) Right(whenMissing) else f.as[A]
}

// ========================================================================
// Input types
// ========================================================================

/** Input for compile, test.suites, sourcegen, fmt, clean. */
case class ProjectsInput(projects: List[String], verbose: Boolean)
object ProjectsInput {
  private val knownFields = Set("projects", "verbose")
  given Decoder[ProjectsInput] = Decoder.instance { c =>
    for {
      _ <- rejectUnknownFields(c, knownFields)
      projects <- decodeList(c, "projects")
      verbose <- decodeWithFallback(c, "verbose", false)
    } yield ProjectsInput(projects, verbose)
  }
  given InputDef[ProjectsInput] = InputDef.raw[ProjectsInput](
    JsonSchemaType.ObjectSchema(
      properties = Some(
        Map(
          "projects" -> JsonSchemaType.ArraySchema(
            items = Some(JsonSchemaType.StringSchema()),
            description = Some("Project names. Omit or empty for all projects.")
          ),
          "verbose" -> JsonSchemaType.BooleanSchema(
            description = Some("Return full output instead of diff against previous run. Default: false.")
          )
        )
      )
    ),
    summon[Decoder[ProjectsInput]]
  )
}

/** Input for test (with test filtering). */
case class TestInput(projects: List[String], only: List[String], exclude: List[String], verbose: Boolean)
object TestInput {
  private val knownFields = Set("projects", "only", "exclude", "verbose")
  given Decoder[TestInput] = Decoder.instance { c =>
    for {
      _ <- rejectUnknownFields(c, knownFields)
      projects <- decodeList(c, "projects")
      only <- decodeList(c, "only")
      exclude <- decodeList(c, "exclude")
      verbose <- decodeWithFallback(c, "verbose", false)
    } yield TestInput(projects, only, exclude, verbose)
  }
  given InputDef[TestInput] = InputDef.raw[TestInput](
    JsonSchemaType.ObjectSchema(
      properties = Some(
        Map(
          "projects" -> JsonSchemaType.ArraySchema(
            items = Some(JsonSchemaType.StringSchema()),
            description = Some("Project names to test. Omit or empty for all test projects.")
          ),
          "only" -> JsonSchemaType.ArraySchema(
            items = Some(JsonSchemaType.StringSchema()),
            description = Some("Only run these test class names.")
          ),
          "exclude" -> JsonSchemaType.ArraySchema(
            items = Some(JsonSchemaType.StringSchema()),
            description = Some("Exclude these test class names.")
          ),
          "verbose" -> JsonSchemaType.BooleanSchema(
            description = Some("Return full output instead of diff against previous run. Default: false.")
          )
        )
      )
    ),
    summon[Decoder[TestInput]]
  )
}

/** Input for bleep.status (filtered view of last run). */
case class StatusInput(project: Option[String], limit: Option[Int], offset: Option[Int])
object StatusInput {
  private val knownFields = Set("project", "limit", "offset")
  given Decoder[StatusInput] = Decoder.instance { c =>
    for {
      _ <- rejectUnknownFields(c, knownFields)
      project <- decodeOptional[String](c, "project")
      limit <- decodeOptional[Int](c, "limit")
      offset <- decodeOptional[Int](c, "offset")
    } yield StatusInput(project, limit, offset)
  }
  given InputDef[StatusInput] = InputDef.raw[StatusInput](
    JsonSchemaType.ObjectSchema(
      properties = Some(
        Map(
          "project" -> JsonSchemaType.StringSchema(description = Some("Filter to a single project name.")),
          "limit" -> JsonSchemaType.IntegerSchema(description = Some("Max number of items (diagnostics for compile, failures for test) to return.")),
          "offset" -> JsonSchemaType.IntegerSchema(description = Some("Skip the first N items before applying limit."))
        )
      )
    ),
    summon[Decoder[StatusInput]]
  )
}

/** Input for bleep.build — reuses ProjectsInput. */
type BuildInput = ProjectsInput

/** Input for run (project/script execution). */
case class RunInput(name: String, args: List[String], mainClass: Option[String], timeoutSeconds: Option[Int])
object RunInput {
  private val knownFields = Set("name", "args", "mainClass", "timeoutSeconds")
  given Decoder[RunInput] = Decoder.instance { c =>
    for {
      _ <- rejectUnknownFields(c, knownFields)
      name <- c.downField("name").as[String]
      args <- decodeList(c, "args")
      mainClass <- decodeOptional[String](c, "mainClass")
      timeoutSeconds <- decodeOptional[Int](c, "timeoutSeconds")
    } yield RunInput(name, args, mainClass, timeoutSeconds)
  }
  given InputDef[RunInput] = InputDef.raw[RunInput](
    JsonSchemaType.ObjectSchema(
      properties = Some(
        Map(
          "name" -> JsonSchemaType.StringSchema(description = Some("Project or script name to run. Scripts are checked first, then projects.")),
          "args" -> JsonSchemaType.ArraySchema(
            items = Some(JsonSchemaType.StringSchema()),
            description = Some("Arguments to pass to the program.")
          ),
          "mainClass" -> JsonSchemaType.StringSchema(description = Some("Override main class (projects only). Not needed for scripts.")),
          "timeoutSeconds" -> JsonSchemaType.IntegerSchema(description = Some("Timeout in seconds. Default 60."))
        )
      ),
      required = Some(List("name"))
    ),
    summon[Decoder[RunInput]]
  )
}
