package bleep
package sbtimport

import cats.syntax.apply._
import com.monovore.decline.Opts

case class ImportOptions(
    ignoreWhenInferringTemplates: Set[model.ProjectName],
    skipSbt: Boolean,
    skipGeneratedResourcesScript: Boolean
)

object ImportOptions {
  val ignoreWhenInferringTemplates: Opts[Set[model.ProjectName]] = Opts
    .options[String](
      "ignore-when-templating",
      "some projects may differ much from the rest, for instance documentation and examples. considering these when computing templates may negatively affect the result",
      "i"
    )
    .orEmpty
    .map(_.map(model.ProjectName.apply))
    .map(_.toSet)

  val skipSbt: Opts[Boolean] =
    Opts.flag("skip-sbt", "use if you already have generated bloop files and want to reimport from them").orFalse

  val skipGeneratedResourcesScript: Opts[Boolean] =
    Opts.flag("skip-generated-resources-script", "disable creating a script to regenerate discovered generated sources/resources ").orFalse

  val opts: Opts[ImportOptions] =
    (ignoreWhenInferringTemplates, skipSbt, skipGeneratedResourcesScript).mapN(ImportOptions.apply)
}
