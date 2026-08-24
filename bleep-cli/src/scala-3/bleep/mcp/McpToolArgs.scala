package bleep.mcp

import com.melvinlow.json.schema.JsonSchemaEncoder
import io.circe.{Decoder, DecodingFailure, Json}

/** Typed argument definitions for MCP tools.
  *
  * Each type has a Decoder (handling missing optional fields) and a JsonSchemaEncoder (generating the JSON Schema that MCP clients see for tool discovery).
  *
  * All decoders are strict: they reject unknown fields and fail on wrong types instead of silently falling back to defaults.
  *
  * Every tool that acts on a build requires `directory`: one MCP server serves a whole Claude session including subagents in other git worktrees, so each call
  * must state which checkout it targets. There is no ambient workspace.
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

private val directoryProperty: (String, Json) =
  "directory" -> Json.obj(
    "type" -> Json.fromString("string"),
    "description" -> Json.fromString(
      "Absolute path of the checkout to act on — your current working directory works. Required on every call: this MCP server serves the whole session, including subagents working in other git worktrees. The target must be a bleep build (bleep.yaml there or in a parent directory); projects built with Maven/Gradle/sbt are out of scope — use their own build tool."
    )
  )

private val projectsProperty: (String, Json) =
  "projects" -> Json.obj(
    "type" -> Json.fromString("array"),
    "items" -> Json.obj("type" -> Json.fromString("string")),
    "description" -> Json.fromString("Project names. Omit or empty for all projects.")
  )

private val diffBaseProperty: (String, Json) =
  "diffBase" -> Json.obj(
    "type" -> Json.fromString("string"),
    "description" -> Json.fromString(
      "The agent inner loop: pass \"previous\" after an edit to get what CHANGED (newly failing/fixed tests, new/resolved diagnostics) instead of re-reading full results, or a numeric historyId to pin a specific base run. Same-mode only (compile diffs against compile, test against test); validated before the build starts, and the response gains a \"diff\" section with the mechanical diff."
    )
  )

/** Args for bleep.compile: projects plus the optional inner-loop `diffBase`. */
case class CompileArgs(directory: String, projects: List[String], diffBase: Option[String])
object CompileArgs {
  private val knownFields = Set("directory", "projects", "diffBase")
  given Decoder[CompileArgs] = Decoder.instance { c =>
    for {
      _ <- rejectUnknownFields(c, knownFields)
      directory <- c.downField("directory").as[String]
      projects <- decodeList(c, "projects")
      diffBase <- decodeOptional[String](c, "diffBase")
    } yield CompileArgs(directory, projects, diffBase)
  }
  given JsonSchemaEncoder[CompileArgs] = schema(
    Json.obj(
      "type" -> Json.fromString("object"),
      "properties" -> Json.obj(directoryProperty, projectsProperty, diffBaseProperty),
      "required" -> Json.arr(Json.fromString("directory"))
    )
  ).asInstanceOf[JsonSchemaEncoder[CompileArgs]]
}

/** Args for test.suites, sourcegen, fmt, clean. */
case class ProjectsArgs(directory: String, projects: List[String])
object ProjectsArgs {
  private val knownFields = Set("directory", "projects")
  given Decoder[ProjectsArgs] = Decoder.instance { c =>
    for {
      _ <- rejectUnknownFields(c, knownFields)
      directory <- c.downField("directory").as[String]
      projects <- decodeList(c, "projects")
    } yield ProjectsArgs(directory, projects)
  }
  given JsonSchemaEncoder[ProjectsArgs] = schema(
    Json.obj(
      "type" -> Json.fromString("object"),
      "properties" -> Json.obj(directoryProperty, projectsProperty),
      "required" -> Json.arr(Json.fromString("directory"))
    )
  ).asInstanceOf[JsonSchemaEncoder[ProjectsArgs]]
}

/** Args for test (with test filtering, plus the optional inner-loop `diffBase`). */
case class TestArgs(directory: String, projects: List[String], only: List[String], exclude: List[String], diffBase: Option[String])
object TestArgs {
  private val knownFields = Set("directory", "projects", "only", "exclude", "diffBase")
  given Decoder[TestArgs] = Decoder.instance { c =>
    for {
      _ <- rejectUnknownFields(c, knownFields)
      directory <- c.downField("directory").as[String]
      projects <- decodeList(c, "projects")
      only <- decodeList(c, "only")
      exclude <- decodeList(c, "exclude")
      diffBase <- decodeOptional[String](c, "diffBase")
    } yield TestArgs(directory, projects, only, exclude, diffBase)
  }
  given JsonSchemaEncoder[TestArgs] = schema(
    Json.obj(
      "type" -> Json.fromString("object"),
      "properties" -> Json.obj(
        directoryProperty,
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
        diffBaseProperty
      ),
      "required" -> Json.arr(Json.fromString("directory"))
    )
  ).asInstanceOf[JsonSchemaEncoder[TestArgs]]
}

/** Args for bleep.history.show: full transcript of a completed run by id. Requires `directory` like every other tool — history ids are per-workspace, so an id
  * means nothing without saying whose history to look in.
  */
case class HistoryShowArgs(directory: String, historyId: Option[Long], project: Option[String], query: Option[String], limit: Option[Int], offset: Option[Int])
object HistoryShowArgs {
  private val knownFields = Set("directory", "historyId", "project", "query", "limit", "offset")
  given Decoder[HistoryShowArgs] = Decoder.instance { c =>
    for {
      _ <- rejectUnknownFields(c, knownFields)
      directory <- c.downField("directory").as[String]
      historyId <- decodeOptional[Long](c, "historyId")
      project <- decodeOptional[String](c, "project")
      query <- decodeOptional[String](c, "query")
      limit <- decodeOptional[Int](c, "limit")
      offset <- decodeOptional[Int](c, "offset")
    } yield HistoryShowArgs(directory, historyId, project, query, limit, offset)
  }
  given JsonSchemaEncoder[HistoryShowArgs] = schema(
    Json.obj(
      "type" -> Json.fromString("object"),
      "properties" -> Json.obj(
        directoryProperty,
        "historyId" -> Json.obj(
          "type" -> Json.fromString("integer"),
          "description" -> Json.fromString("The historyId from a compile/test response. Omit for the workspace's most recent run.")
        ),
        "project" -> Json.obj(
          "type" -> Json.fromString("string"),
          "description" -> Json.fromString("Filter to a single project name.")
        ),
        "query" -> Json.obj(
          "type" -> Json.fromString("string"),
          "description" -> Json.fromString(
            "Search the transcript instead of paging through it: a case-insensitive regex matched against each diagnostic's message, rendered text and path (compile) or each failure's suite, test, message and stack trace (test). Only matching items are returned; summary counts still reflect the full run."
          )
        ),
        "limit" -> Json.obj(
          "type" -> Json.fromString("integer"),
          "description" -> Json.fromString("Max number of items (diagnostics for compile, failures for test) to return.")
        ),
        "offset" -> Json.obj(
          "type" -> Json.fromString("integer"),
          "description" -> Json.fromString("Skip the first N items before applying limit.")
        )
      ),
      "required" -> Json.arr(Json.fromString("directory"))
    )
  ).asInstanceOf[JsonSchemaEncoder[HistoryShowArgs]]
}

/** Args for bleep.build.effective / bleep.build.resolved (project config inspection). */
case class BuildArgs(directory: String, projects: List[String])
object BuildArgs {
  private val knownFields = Set("directory", "projects")
  given Decoder[BuildArgs] = Decoder.instance { c =>
    for {
      _ <- rejectUnknownFields(c, knownFields)
      directory <- c.downField("directory").as[String]
      projects <- decodeList(c, "projects")
    } yield BuildArgs(directory, projects)
  }
  given JsonSchemaEncoder[BuildArgs] = schema(
    Json.obj(
      "type" -> Json.fromString("object"),
      "properties" -> Json.obj(
        directoryProperty,
        "projects" -> Json.obj(
          "type" -> Json.fromString("array"),
          "items" -> Json.obj("type" -> Json.fromString("string")),
          "description" -> Json.fromString("Project names to show. Omit or empty for all projects.")
        )
      ),
      "required" -> Json.arr(Json.fromString("directory"))
    )
  ).asInstanceOf[JsonSchemaEncoder[BuildArgs]]
}

/** Args for bleep.copy-state: seed a fresh worktree with the parent worktree's compiled state. */
case class CopyStateArgs(directory: String, from: String)
object CopyStateArgs {
  private val knownFields = Set("directory", "from")
  given Decoder[CopyStateArgs] = Decoder.instance { c =>
    for {
      _ <- rejectUnknownFields(c, knownFields)
      directory <- c.downField("directory").as[String]
      from <- c.downField("from").as[String]
    } yield CopyStateArgs(directory, from)
  }
  given JsonSchemaEncoder[CopyStateArgs] = schema(
    Json.obj(
      "type" -> Json.fromString("object"),
      "properties" -> Json.obj(
        directoryProperty,
        "from" -> Json.obj(
          "type" -> Json.fromString("string"),
          "description" -> Json.fromString(
            "Absolute path of the worktree this one was forked from — the only place state is copied from. Typically the checkout the orchestrating session runs in."
          )
        )
      ),
      "required" -> Json.arr(Json.fromString("directory"), Json.fromString("from"))
    )
  ).asInstanceOf[JsonSchemaEncoder[CopyStateArgs]]
}

/** Args for bleep.history.diff / bleep.history.diff-timing: compare two completed runs. Both ids are required and explicit — no defaulting to "latest", because
  * diffing the wrong pair silently is worse than asking the caller to say what they mean. `directory` names the workspace whose history holds the ids;
  * `baseDirectory` optionally resolves `base` in a different worktree (the copy-state verification flow).
  */
case class DiffArgs(directory: String, base: Long, target: Long, limit: Option[Int], baseDirectory: Option[String])
object DiffArgs {
  private val knownFields = Set("directory", "base", "target", "limit", "baseDirectory")
  given Decoder[DiffArgs] = Decoder.instance { c =>
    for {
      _ <- rejectUnknownFields(c, knownFields)
      directory <- c.downField("directory").as[String]
      base <- c.downField("base").as[Long]
      target <- c.downField("target").as[Long]
      limit <- decodeOptional[Int](c, "limit")
      baseDirectory <- decodeOptional[String](c, "baseDirectory")
    } yield DiffArgs(directory, base, target, limit, baseDirectory)
  }
  given JsonSchemaEncoder[DiffArgs] = schema(
    Json.obj(
      "type" -> Json.fromString("object"),
      "properties" -> Json.obj(
        directoryProperty,
        "base" -> Json.obj(
          "type" -> Json.fromString("integer"),
          "description" -> Json.fromString("historyId of the run to compare FROM (the earlier/reference run).")
        ),
        "target" -> Json.obj(
          "type" -> Json.fromString("integer"),
          "description" -> Json.fromString("historyId of the run to compare TO (the later run).")
        ),
        "limit" -> Json.obj(
          "type" -> Json.fromString("integer"),
          "description" -> Json.fromString("bleep.history.diff-timing only: max entries per list (slower/faster/slowestInTarget). Default 15.")
        ),
        "baseDirectory" -> Json.obj(
          "type" -> Json.fromString("string"),
          "description" -> Json.fromString(
            "Resolve `base` in this other worktree's history instead of `directory`'s — e.g. the worktree this one was forked from, to verify a copy-state seed. Cross-worktree diagnostic identity is path-relativized automatically."
          )
        )
      ),
      "required" -> Json.arr(Json.fromString("directory"), Json.fromString("base"), Json.fromString("target"))
    )
  ).asInstanceOf[JsonSchemaEncoder[DiffArgs]]
}

/** Args for tools that need only a workspace: projects, programs, scripts. */
case class DirArgs(directory: String)
object DirArgs {
  private val knownFields = Set("directory")
  given Decoder[DirArgs] = Decoder.instance { c =>
    for {
      _ <- rejectUnknownFields(c, knownFields)
      directory <- c.downField("directory").as[String]
    } yield DirArgs(directory)
  }
  given JsonSchemaEncoder[DirArgs] = schema(
    Json.obj(
      "type" -> Json.fromString("object"),
      "properties" -> Json.obj(directoryProperty),
      "required" -> Json.arr(Json.fromString("directory"))
    )
  ).asInstanceOf[JsonSchemaEncoder[DirArgs]]
}

/** No arguments — only for process-level tools (restart). */
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
case class RunArgs(directory: String, name: String, args: List[String], mainClass: Option[String], timeoutSeconds: Option[Int])
object RunArgs {
  private val knownFields = Set("directory", "name", "args", "mainClass", "timeoutSeconds")
  given Decoder[RunArgs] = Decoder.instance { c =>
    for {
      _ <- rejectUnknownFields(c, knownFields)
      directory <- c.downField("directory").as[String]
      name <- c.downField("name").as[String]
      args <- decodeList(c, "args")
      mainClass <- decodeOptional[String](c, "mainClass")
      timeoutSeconds <- decodeOptional[Int](c, "timeoutSeconds")
    } yield RunArgs(directory, name, args, mainClass, timeoutSeconds)
  }
  given JsonSchemaEncoder[RunArgs] = schema(
    Json.obj(
      "type" -> Json.fromString("object"),
      "properties" -> Json.obj(
        directoryProperty,
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
      "required" -> Json.arr(Json.fromString("directory"), Json.fromString("name"))
    )
  ).asInstanceOf[JsonSchemaEncoder[RunArgs]]
}
