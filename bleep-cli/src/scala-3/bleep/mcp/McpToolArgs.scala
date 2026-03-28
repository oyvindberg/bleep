package bleep.mcp

import com.melvinlow.json.schema.JsonSchemaEncoder
import io.circe.{Decoder, DecodingFailure, Json}

/** Typed argument definitions for MCP tools.
  *
  * Each type has a Decoder (handling missing optional fields) and a JsonSchemaEncoder (generating the JSON Schema that MCP clients see for tool discovery).
  *
  * All decoders are strict: they reject unknown fields and fail on wrong types instead of silently falling back to defaults.
  */

private def schema(json: Json): JsonSchemaEncoder[Nothing] =
  new JsonSchemaEncoder[Nothing] { def schema: Json = json }

/** Reject unknown fields in the JSON object. */
private def rejectUnknownFields(c: io.circe.HCursor, knownFields: Set[String]): Decoder.Result[Unit] = {
  val keys = c.keys.map(_.toSet).getOrElse(Set.empty)
  val unknown = keys -- knownFields
  if (unknown.nonEmpty) Left(DecodingFailure(s"Unknown fields: ${unknown.mkString(", ")}. Known fields: ${knownFields.mkString(", ")}", c.history))
  else Right(())
}

/** Decode an optional field: missing → None, present → must decode correctly (no silent fallback). */
private def decodeOptional[A](c: io.circe.HCursor, field: String)(using Decoder[A]): Decoder.Result[Option[A]] = {
  val f = c.downField(field)
  if (f.failed) Right(None) else f.as[A].map(Some(_))
}

/** Decode a list field: missing → Nil, present → must be a valid List[String] (no silent fallback). */
private def decodeList(c: io.circe.HCursor, field: String): Decoder.Result[List[String]] = {
  val f = c.downField(field)
  if (f.failed) Right(Nil) else f.as[List[String]]
}

/** Decode a field with a default: missing → default value, present → must decode correctly (no silent fallback). */
private def decodeWithFallback[A](c: io.circe.HCursor, field: String, whenMissing: A)(using Decoder[A]): Decoder.Result[A] = {
  val f = c.downField(field)
  if (f.failed) Right(whenMissing) else f.as[A]
}

/** Args for compile, test.suites, sourcegen, fmt, clean. */
case class ProjectsArgs(projects: List[String], verbose: Boolean)
object ProjectsArgs {
  private val knownFields = Set("projects", "verbose")
  given Decoder[ProjectsArgs] = Decoder.instance { c =>
    for {
      _ <- rejectUnknownFields(c, knownFields)
      projects <- decodeList(c, "projects")
      verbose <- decodeWithFallback(c, "verbose", false)
    } yield ProjectsArgs(projects, verbose)
  }
  given JsonSchemaEncoder[ProjectsArgs] = schema(
    Json.obj(
      "type" -> Json.fromString("object"),
      "properties" -> Json.obj(
        "projects" -> Json.obj(
          "type" -> Json.fromString("array"),
          "items" -> Json.obj("type" -> Json.fromString("string")),
          "description" -> Json.fromString("Project names. Omit or empty for all projects.")
        ),
        "verbose" -> Json.obj(
          "type" -> Json.fromString("boolean"),
          "description" -> Json.fromString("Return full output instead of diff against previous run. Default: false.")
        )
      )
    )
  ).asInstanceOf[JsonSchemaEncoder[ProjectsArgs]]
}

/** Args for test (with test filtering). */
case class TestArgs(projects: List[String], only: List[String], exclude: List[String], verbose: Boolean)
object TestArgs {
  private val knownFields = Set("projects", "only", "exclude", "verbose")
  given Decoder[TestArgs] = Decoder.instance { c =>
    for {
      _ <- rejectUnknownFields(c, knownFields)
      projects <- decodeList(c, "projects")
      only <- decodeList(c, "only")
      exclude <- decodeList(c, "exclude")
      verbose <- decodeWithFallback(c, "verbose", false)
    } yield TestArgs(projects, only, exclude, verbose)
  }
  given JsonSchemaEncoder[TestArgs] = schema(
    Json.obj(
      "type" -> Json.fromString("object"),
      "properties" -> Json.obj(
        "projects" -> Json.obj(
          "type" -> Json.fromString("array"),
          "items" -> Json.obj("type" -> Json.fromString("string")),
          "description" -> Json.fromString("Project names to test. Omit or empty for all test projects.")
        ),
        "only" -> Json.obj(
          "type" -> Json.fromString("array"),
          "items" -> Json.obj("type" -> Json.fromString("string")),
          "description" -> Json.fromString("Only run these test class names.")
        ),
        "exclude" -> Json.obj(
          "type" -> Json.fromString("array"),
          "items" -> Json.obj("type" -> Json.fromString("string")),
          "description" -> Json.fromString("Exclude these test class names.")
        ),
        "verbose" -> Json.obj(
          "type" -> Json.fromString("boolean"),
          "description" -> Json.fromString("Return full output instead of diff against previous run. Default: false.")
        )
      )
    )
  ).asInstanceOf[JsonSchemaEncoder[TestArgs]]
}

/** Args for watch (compile or test mode with filtering). */
case class WatchArgs(projects: List[String], mode: String, only: List[String], exclude: List[String])
object WatchArgs {
  private val knownFields = Set("projects", "mode", "only", "exclude")
  given Decoder[WatchArgs] = Decoder.instance { c =>
    for {
      _ <- rejectUnknownFields(c, knownFields)
      projects <- decodeList(c, "projects")
      mode <- decodeWithFallback(c, "mode", "compile")
      only <- decodeList(c, "only")
      exclude <- decodeList(c, "exclude")
    } yield WatchArgs(projects, mode, only, exclude)
  }
  given JsonSchemaEncoder[WatchArgs] = schema(
    Json.obj(
      "type" -> Json.fromString("object"),
      "properties" -> Json.obj(
        "projects" -> Json.obj(
          "type" -> Json.fromString("array"),
          "items" -> Json.obj("type" -> Json.fromString("string")),
          "description" -> Json.fromString("Project names to watch.")
        ),
        "mode" -> Json.obj(
          "type" -> Json.fromString("string"),
          "enum" -> Json.arr(Json.fromString("compile"), Json.fromString("test")),
          "description" -> Json.fromString("Watch mode. Default: compile.")
        ),
        "only" -> Json.obj(
          "type" -> Json.fromString("array"),
          "items" -> Json.obj("type" -> Json.fromString("string")),
          "description" -> Json.fromString("Only run these test class names (test mode only).")
        ),
        "exclude" -> Json.obj(
          "type" -> Json.fromString("array"),
          "items" -> Json.obj("type" -> Json.fromString("string")),
          "description" -> Json.fromString("Exclude these test class names (test mode only).")
        )
      )
    )
  ).asInstanceOf[JsonSchemaEncoder[WatchArgs]]
}

/** Args for sync and watch.stop. */
case class JobIdArgs(jobId: Option[String])
object JobIdArgs {
  private val knownFields = Set("jobId")
  given Decoder[JobIdArgs] = Decoder.instance { c =>
    for {
      _ <- rejectUnknownFields(c, knownFields)
      jobId <- decodeOptional[String](c, "jobId")
    } yield JobIdArgs(jobId)
  }
  given JsonSchemaEncoder[JobIdArgs] = schema(
    Json.obj(
      "type" -> Json.fromString("object"),
      "properties" -> Json.obj(
        "jobId" -> Json.obj(
          "type" -> Json.fromString("string"),
          "description" -> Json.fromString("Watch job ID. Omit to affect all jobs.")
        )
      )
    )
  ).asInstanceOf[JsonSchemaEncoder[JobIdArgs]]
}

/** Args for bleep.status (filtered view of last run). */
case class StatusArgs(project: Option[String], limit: Option[Int], offset: Option[Int])
object StatusArgs {
  private val knownFields = Set("project", "limit", "offset")
  given Decoder[StatusArgs] = Decoder.instance { c =>
    for {
      _ <- rejectUnknownFields(c, knownFields)
      project <- decodeOptional[String](c, "project")
      limit <- decodeOptional[Int](c, "limit")
      offset <- decodeOptional[Int](c, "offset")
    } yield StatusArgs(project, limit, offset)
  }
  given JsonSchemaEncoder[StatusArgs] = schema(
    Json.obj(
      "type" -> Json.fromString("object"),
      "properties" -> Json.obj(
        "project" -> Json.obj(
          "type" -> Json.fromString("string"),
          "description" -> Json.fromString("Filter to a single project name.")
        ),
        "limit" -> Json.obj(
          "type" -> Json.fromString("integer"),
          "description" -> Json.fromString("Max number of items (diagnostics for compile, failures for test) to return.")
        ),
        "offset" -> Json.obj(
          "type" -> Json.fromString("integer"),
          "description" -> Json.fromString("Skip the first N items before applying limit.")
        )
      )
    )
  ).asInstanceOf[JsonSchemaEncoder[StatusArgs]]
}

/** Args for bleep.build (project config inspection). */
case class BuildArgs(projects: List[String])
object BuildArgs {
  private val knownFields = Set("projects")
  given Decoder[BuildArgs] = Decoder.instance { c =>
    for {
      _ <- rejectUnknownFields(c, knownFields)
      projects <- decodeList(c, "projects")
    } yield BuildArgs(projects)
  }
  given JsonSchemaEncoder[BuildArgs] = schema(
    Json.obj(
      "type" -> Json.fromString("object"),
      "properties" -> Json.obj(
        "projects" -> Json.obj(
          "type" -> Json.fromString("array"),
          "items" -> Json.obj("type" -> Json.fromString("string")),
          "description" -> Json.fromString("Project names to show. Omit or empty for all projects.")
        )
      )
    )
  ).asInstanceOf[JsonSchemaEncoder[BuildArgs]]
}

/** No arguments. */
case class NoArgs()
object NoArgs {
  given Decoder[NoArgs] = Decoder.instance { c =>
    for {
      _ <- rejectUnknownFields(c, Set.empty)
    } yield NoArgs()
  }
  given JsonSchemaEncoder[NoArgs] = schema(
    Json.obj(
      "type" -> Json.fromString("object"),
      "properties" -> Json.obj()
    )
  ).asInstanceOf[JsonSchemaEncoder[NoArgs]]
}

/** Args for run (project/script execution). */
case class RunArgs(name: String, args: List[String], mainClass: Option[String], timeoutSeconds: Option[Int])
object RunArgs {
  private val knownFields = Set("name", "args", "mainClass", "timeoutSeconds")
  given Decoder[RunArgs] = Decoder.instance { c =>
    for {
      _ <- rejectUnknownFields(c, knownFields)
      name <- c.downField("name").as[String]
      args <- decodeList(c, "args")
      mainClass <- decodeOptional[String](c, "mainClass")
      timeoutSeconds <- decodeOptional[Int](c, "timeoutSeconds")
    } yield RunArgs(name, args, mainClass, timeoutSeconds)
  }
  given JsonSchemaEncoder[RunArgs] = schema(
    Json.obj(
      "type" -> Json.fromString("object"),
      "properties" -> Json.obj(
        "name" -> Json.obj(
          "type" -> Json.fromString("string"),
          "description" -> Json.fromString("Project or script name to run. Scripts are checked first, then projects.")
        ),
        "args" -> Json.obj(
          "type" -> Json.fromString("array"),
          "items" -> Json.obj("type" -> Json.fromString("string")),
          "description" -> Json.fromString("Arguments to pass to the program.")
        ),
        "mainClass" -> Json.obj(
          "type" -> Json.fromString("string"),
          "description" -> Json.fromString("Override main class (projects only). Not needed for scripts.")
        ),
        "timeoutSeconds" -> Json.obj(
          "type" -> Json.fromString("integer"),
          "description" -> Json.fromString("Timeout in seconds. Default 60.")
        )
      ),
      "required" -> Json.arr(Json.fromString("name"))
    )
  ).asInstanceOf[JsonSchemaEncoder[RunArgs]]
}
