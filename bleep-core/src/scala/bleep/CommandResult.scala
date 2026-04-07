package bleep

import io.circe.{Encoder, Json}
import io.circe.syntax.*
import io.circe.generic.semiauto.deriveEncoder

/** Typed response envelope for request/response commands.
  *
  * When `--output json` is active, commands wrap their output in this envelope so that both success data and errors have a consistent JSON structure.
  */
sealed trait CommandResult

object CommandResult {
  case class Success(data: Json) extends CommandResult
  case class Failure(error: String) extends CommandResult

  implicit val encoder: Encoder[CommandResult] = Encoder.instance {
    case Success(data)  => Json.obj("success" -> true.asJson).deepMerge(data)
    case Failure(error) => Json.obj("success" -> false.asJson, "error" -> error.asJson)
  }

  def success[A: Encoder](a: A): CommandResult = Success(a.asJson)
  def failure(ex: BleepException): CommandResult = Failure(ex.message)

  def print(result: CommandResult): Unit =
    println(result.asJson.noSpaces)
}

// --- Per-command output types ---

case class ProjectList(projects: List[String])
object ProjectList {
  implicit val encoder: Encoder[ProjectList] = deriveEncoder
}

case class ProjectTests(project: String, testClasses: List[String])
object ProjectTests {
  implicit val encoder: Encoder[ProjectTests] = deriveEncoder
}

case class TestList(projects: List[ProjectTests])
object TestList {
  implicit val encoder: Encoder[TestList] = deriveEncoder
}

case class ProjectConfig(name: String, config: String)
object ProjectConfig {
  implicit val encoder: Encoder[ProjectConfig] = deriveEncoder
}

case class ProjectConfigs(projects: List[ProjectConfig])
object ProjectConfigs {
  implicit val encoder: Encoder[ProjectConfigs] = deriveEncoder
}

case class ProjectEvictions(project: String, warnings: List[String])
object ProjectEvictions {
  implicit val encoder: Encoder[ProjectEvictions] = deriveEncoder
}

case class EvictionReport(projects: List[ProjectEvictions])
object EvictionReport {
  implicit val encoder: Encoder[EvictionReport] = deriveEncoder
}

case class ProjectDiff(project: String, removed: Option[String], added: Option[String])
object ProjectDiff {
  implicit val encoder: Encoder[ProjectDiff] = deriveEncoder
}

case class DiffReport(projects: List[ProjectDiff])
object DiffReport {
  implicit val encoder: Encoder[DiffReport] = deriveEncoder
}

case class ConfigLocation(path: String)
object ConfigLocation {
  implicit val encoder: Encoder[ConfigLocation] = deriveEncoder
}

case class AuthEntry(uri: String, kind: String)
object AuthEntry {
  implicit val encoder: Encoder[AuthEntry] = deriveEncoder
}

case class AuthList(entries: List[AuthEntry])
object AuthList {
  implicit val encoder: Encoder[AuthList] = deriveEncoder
}
