package bleep
package commands

import bleep.internal.{Os, ShortenAndSortJson}
import io.circe.syntax._

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}

case class BuildReapplyTemplates(started: Started, opts: CommonOpts) extends BleepCommand {
  def go(p: model.Project): model.Project = {
    val recursed = p.copy(cross = JsonMap(p.cross.value.map { case (name, p) => (name, go(p)) }))

    p.`extends`.values.foldLeft(recursed) { case (acc, templateId) =>
      acc.removeAll(started.build.templates.get(templateId))
    }
  }
  override def run(): Unit = {
    val buildPaths = BuildPaths(Os.cwd / Defaults.BuildFileName)
    val newProjects = started.rawBuild.projects.map { case (name, p) => (name, go(p)) }
    val build = started.rawBuild.copy(projects = newProjects)
    Files.writeString(
      buildPaths.bleepJsonFile,
      build.asJson.foldWith(ShortenAndSortJson).spaces2,
      UTF_8
    )
  }
}
