package bleep.mcp

import com.melvinlow.json.schema.JsonSchemaEncoder
import io.circe.{Decoder, Json}

/** Typed argument definitions for MCP tools.
  *
  * Each type has a Decoder (handling missing optional fields) and a JsonSchemaEncoder (generating the JSON Schema that MCP clients see for tool discovery).
  */

private def schema(json: Json): JsonSchemaEncoder[Nothing] =
  new JsonSchemaEncoder[Nothing] { def schema: Json = json }

/** Args for compile, test.suites, sourcegen, fmt, clean. */
case class ProjectsArgs(projects: List[String], verbose: Boolean)
object ProjectsArgs {
  given Decoder[ProjectsArgs] = Decoder.instance { c =>
    for {
      projects <- c.downField("projects").as[List[String]].orElse(Right(Nil))
      verbose <- c.downField("verbose").as[Boolean].orElse(Right(false))
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
  given Decoder[TestArgs] = Decoder.instance { c =>
    for {
      projects <- c.downField("projects").as[List[String]].orElse(Right(Nil))
      only <- c.downField("only").as[List[String]].orElse(Right(Nil))
      exclude <- c.downField("exclude").as[List[String]].orElse(Right(Nil))
      verbose <- c.downField("verbose").as[Boolean].orElse(Right(false))
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
  given Decoder[WatchArgs] = Decoder.instance { c =>
    for {
      projects <- c.downField("projects").as[List[String]].orElse(Right(Nil))
      mode <- c.downField("mode").as[String].orElse(Right("compile"))
      only <- c.downField("only").as[List[String]].orElse(Right(Nil))
      exclude <- c.downField("exclude").as[List[String]].orElse(Right(Nil))
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
  given Decoder[JobIdArgs] = Decoder.instance(c => Right(JobIdArgs(c.downField("jobId").as[String].toOption)))
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
  given Decoder[StatusArgs] = Decoder.instance { c =>
    Right(
      StatusArgs(
        project = c.downField("project").as[String].toOption,
        limit = c.downField("limit").as[Int].toOption,
        offset = c.downField("offset").as[Int].toOption
      )
    )
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
  given Decoder[BuildArgs] = Decoder.instance { c =>
    for {
      projects <- c.downField("projects").as[List[String]].orElse(Right(Nil))
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
  given Decoder[NoArgs] = Decoder.instance(_ => Right(NoArgs()))
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
  given Decoder[RunArgs] = Decoder.instance { c =>
    for {
      name <- c.downField("name").as[String]
      args <- c.downField("args").as[List[String]].orElse(Right(Nil))
    } yield RunArgs(
      name,
      args,
      mainClass = c.downField("mainClass").as[String].toOption,
      timeoutSeconds = c.downField("timeoutSeconds").as[Int].toOption
    )
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
