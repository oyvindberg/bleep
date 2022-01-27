package bleep
package commands

import bleep.internal.{importBloopFilesFromSbt, normalize, Os, ShortenAndSortJson, Templates}
import bleep.logging.Logger
import cats.implicits.catsSyntaxTuple2Semigroupal
import com.monovore.decline.Opts
import io.circe.syntax._

import java.nio.file.Files

object Import {
  case class Options(
      ignoreWhenInferringTemplates: Set[model.ProjectName],
      skipSbt: Boolean
  )

  val skipSbt: Opts[Boolean] =
    Opts.flag("skip-sbt", "use if you already have generated bloop files and want to reimport from them").orFalse

  val ignoreWhenInferringTemplates: Opts[List[model.ProjectName]] = Opts
    .options[String](
      "ignore-when-templating",
      "some projects may differ much from the rest, for instance documentation and examples. considering these when computing templates may negatively affect the result",
      "i"
    )
    .orEmpty
    .map(_.map(model.ProjectName.apply))

  val opts: Opts[Options] =
    (skipSbt, ignoreWhenInferringTemplates).mapN { case (skipSbt, ignoreWhenInferringTemplates) =>
      Options(ignoreWhenInferringTemplates.toSet, skipSbt = skipSbt)
    }

}
case class Import(logger: Logger, options: Import.Options) extends BleepCommand {

  override def run(): Unit = {
    val buildPaths = BuildPaths(Os.cwd / Defaults.BuildFileName)

    if (!options.skipSbt)
      cli("sbt 'set Global / bloopConfigDir := baseDirectory.value / s\".bleep/import/bloop-${scalaBinaryVersion.value}\"' +bloopInstall")(buildPaths.buildDir)

    val build0 = importBloopFilesFromSbt(logger, buildPaths)
    val normalizedBuild = normalize(build0)
    val build = Templates(normalizedBuild, options.ignoreWhenInferringTemplates)

    Files.writeString(
      buildPaths.bleepJsonFile,
      build.asJson.foldWith(ShortenAndSortJson).spaces2
    )

    logger.info(s"Imported ${build0.projects.size} cross targets for ${build.projects.value.size} projects")
  }
}
