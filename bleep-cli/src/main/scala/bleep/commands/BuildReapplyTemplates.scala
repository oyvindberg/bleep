package bleep
package commands

import bleep.internal.{FileUtils, ShortenAndSortJson, Templates}
import bleep.rewrites.normalizeBuild
import io.circe.syntax._
import io.circe.yaml.syntax.AsYaml

case class BuildReapplyTemplates(started: Started) extends BleepCommand {
  override def run(): Either[BuildException, Unit] = {
    val normalizedBuild = normalizeBuild(started.build)
    val build = Templates.reapply(normalizedBuild, started.rawBuild.templates)

    FileUtils.writeString(
      started.buildPaths.bleepYamlFile,
      build.asJson.foldWith(ShortenAndSortJson).asYaml.spaces2
    )
    Right(())
  }
}
