package bleep.testing

import bleep.{yaml, BleepException}
import io.circe.Json

/** How much a generated build file says, as a score for template inference. Every scalar value counts once, array elements included, and so does every
  * `extends` reference. Templating moves settings out of projects and into templates, so a better inference gives a lower number for the same build.
  */
object TemplateStats {
  def render(bleepYaml: String): String = {
    val json = yaml.parse(bleepYaml) match {
      case Left(err)   => throw new BleepException.Text(s"Couldn't parse generated build file: ${err.message}")
      case Right(json) => json
    }

    def section(name: String): Iterable[Json] =
      json.hcursor.downField(name).focus.flatMap(_.asObject).map(_.values).getOrElse(Nil)

    val templates = section("templates")
    val projects = section("projects")
    val inTemplates = templates.map(countScalars).sum
    val inProjects = projects.map(countScalars).sum

    List(
      s"templates: ${templates.size}",
      s"projects: ${projects.size}",
      s"settings: ${inTemplates + inProjects} (${inProjects} in projects, $inTemplates in templates)"
    ).mkString("\n")
  }

  private def countScalars(json: Json): Int =
    json.fold(
      jsonNull = 0,
      jsonBoolean = _ => 1,
      jsonNumber = _ => 1,
      jsonString = _ => 1,
      jsonArray = _.map(countScalars).sum,
      jsonObject = _.values.map(countScalars).sum
    )
}
