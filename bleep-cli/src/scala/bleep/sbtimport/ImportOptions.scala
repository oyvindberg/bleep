package bleep
package sbtimport

import cats.data.{NonEmptyList, Validated, ValidatedNel}
import cats.syntax.apply.*
import com.monovore.decline.{Argument, Opts}

case class ImportOptions(
    ignoreWhenInferringTemplates: Set[model.ProjectName],
    skipSbt: Boolean,
    skipGeneratedResourcesScript: Boolean,
    jvm: model.Jvm,
    sbtPath: Option[String],
    xmx: Option[String],
    filtering: ImportFiltering
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

  implicit val jvmArgument: Argument[model.Jvm] = new Argument[model.Jvm] {
    override def read(string: String): ValidatedNel[String, model.Jvm] = Validated.Valid(model.Jvm(string, None))
    override def defaultMetavar: String = "metavar-jvm"
  }

  val jvm: Opts[model.Jvm] =
    Opts
      .flagOption("jvm", "pick JVM to use for import. Valid nam in index file at https://github.com/coursier/jvm-index/raw/master/index.json")
      .withDefault(Some(model.Jvm.system))
      .map(_.get)

  val sbtPath: Opts[Option[String]] =
    Opts
      .option[String]("sbt-path", "optional path to sbt executable if sbt provided by coursier can not be used.")
      .orNone

  val xmx: Opts[Option[String]] =
    Opts
      .option[String]("xmx", "JVM heap size for sbt processes (e.g., '4g', '8g'). Default is '4g'.")
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

  val opts: Opts[ImportOptions] =
    (ignoreWhenInferringTemplates, skipSbt, skipGeneratedResourcesScript, jvm, sbtPath, xmx, excludeProjects, filterPlatforms, filterScalaVersions).mapN {
      (ignore, skipSbt, skipScript, jvm, sbtPath, xmx, excludeProjects, filterPlatforms, filterScalaVersions) =>
        ImportOptions(
          ignore,
          skipSbt,
          skipScript,
          jvm,
          sbtPath,
          xmx,
          ImportFiltering(excludeProjects, filterPlatforms, filterScalaVersions)
        )
    }

}
