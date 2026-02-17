package bleep
package mavenimport

import cats.data.NonEmptyList
import cats.syntax.apply.*
import com.monovore.decline.{Argument, Opts}

case class MavenImportOptions(
    ignoreWhenInferringTemplates: Set[model.ProjectName],
    skipMvn: Boolean,
    skipGeneratedResourcesScript: Boolean,
    mvnPath: Option[String],
    filtering: sbtimport.ImportFiltering
)

object MavenImportOptions {
  val ignoreWhenInferringTemplates: Opts[Set[model.ProjectName]] = Opts
    .options[String](
      "ignore-when-templating",
      "some projects may differ much from the rest, for instance documentation and examples. considering these when computing templates may negatively affect the result",
      "i"
    )
    .orEmpty
    .map(_.map(model.ProjectName.apply))
    .map(_.toSet)

  val skipMvn: Opts[Boolean] =
    Opts.flag("skip-mvn", "skip running mvn, use existing effective-pom.xml in .bleep/import/maven/").orFalse

  val skipGeneratedResourcesScript: Opts[Boolean] =
    Opts.flag("skip-generated-resources-script", "disable creating a script to regenerate discovered generated sources/resources").orFalse

  val mvnPath: Opts[Option[String]] =
    Opts
      .option[String]("mvn-path", "optional path to mvn executable")
      .orNone

  val excludeProjects: Opts[Set[model.ProjectName]] = Opts
    .options[String](
      "exclude-project",
      "exclude specific projects from import and all their downstream dependencies (can be specified multiple times)",
      "x"
    )
    .orEmpty
    .map(_.map(model.ProjectName.apply))
    .map(_.toSet)

  val possibleScalaVersions: Map[String, model.VersionScala] =
    List(model.VersionScala.Scala3, model.VersionScala.Scala213, model.VersionScala.Scala212).map(v => (v.binVersion.replace("\\.", ""), v)).toMap

  val filterPlatforms: Opts[Option[NonEmptyList[model.PlatformId]]] = Opts
    .options("platform", "only import projects for specified platform(s)", metavar = "platform", short = "p")(
      Argument.fromMap("platform", model.PlatformId.All.map(p => (p.value, p)).toMap)
    )
    .orNone

  val filterScalaVersions: Opts[Option[NonEmptyList[model.VersionScala]]] = Opts
    .options("scala", "only import projects for specified scala version(s)", "s", "scala version")(
      Argument.fromMap("scala version", possibleScalaVersions)
    )
    .orNone

  val opts: Opts[MavenImportOptions] =
    (ignoreWhenInferringTemplates, skipMvn, skipGeneratedResourcesScript, mvnPath, excludeProjects, filterPlatforms, filterScalaVersions).mapN {
      (ignore, skipMvn, skipScript, mvnPath, excludeProjects, filterPlatforms, filterScalaVersions) =>
        MavenImportOptions(
          ignore,
          skipMvn,
          skipScript,
          mvnPath,
          sbtimport.ImportFiltering(excludeProjects, filterPlatforms, filterScalaVersions)
        )
    }
}
