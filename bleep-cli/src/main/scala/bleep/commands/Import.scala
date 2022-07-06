package bleep
package commands

import bleep.commands.Import.findGeneratedJsonFiles
import bleep.internal._
import bleep.logging.Logger
import bleep.rewrites.normalizeBuild
import cats.syntax.apply._
import com.monovore.decline.Opts

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters._

object Import {
  case class Options(
      ignoreWhenInferringTemplates: Set[model.ProjectName],
      skipSbt: Boolean,
      skipGeneratedResourcesScript: Boolean
  )

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

  val opts: Opts[Options] =
    (ignoreWhenInferringTemplates, skipSbt, skipGeneratedResourcesScript).mapN(Options.apply)
  def findGeneratedJsonFiles(under: Path): Iterable[Path] =
    Files
      .list(under)
      .filter(Files.isDirectory(_))
      .flatMap(dir => Files.list(dir).filter(x => Files.isRegularFile(x) && x.getFileName.toString.endsWith(".json")))
      .toList
      .asScala
}

// pardon the very imperative interface of the class with indirect flow through files. let's refactor later
case class Import(
    existingBuild: Option[model.Build],
    sbtBuildDir: Path,
    destinationPaths: BuildPaths,
    logger: Logger,
    options: Import.Options,
    bleepVersion: model.Version
) extends BleepCommand {
  override def run(): Either[BuildException, Unit] = {
    if (!options.skipSbt) {
      generateBloopAndDependencyFiles()
    }

    val bloopFiles = findGeneratedJsonFiles(destinationPaths.bleepImportBloopDir).map(GenBloopFiles.readAndParseBloopFile)
    val sbtExportFiles = findGeneratedJsonFiles(destinationPaths.bleepImportSbtExportDir).map { path =>
      val contents = Files.readString(path)
      ReadSbtExportFile.parse(path, contents)
    }
    val inputProjects = ImportInputProjects(bloopFiles, sbtExportFiles)
    val files = generateBuild(
      inputProjects,
      hackDropBleepDependency = false,
      skipGeneratedResourcesScript = options.skipGeneratedResourcesScript,
      existingBuild
    ).map { case (path, content) => (RelPath.relativeTo(destinationPaths.buildDir, path), content) }

    FileUtils.syncStrings(destinationPaths.buildDir, files, deleteUnknowns = FileUtils.DeleteUnknowns.No, soft = false)

    Right(())
  }

  def generateBloopAndDependencyFiles(): Unit = {
    val tempAddBloopPlugin = sbtBuildDir / "project" / "bleep-temp-add-bloop-plugin.sbt"

    FileUtils.writeString(
      tempAddBloopPlugin,
      s"""
addSbtPlugin("ch.epfl.scala" % "sbt-bloop" % "1.5.0")
addSbtPlugin("build.bleep" % "sbt-export-dependencies" % "0.1.0")
"""
    )

    try {
      // only accepts relative paths
      val importDir = RelPath.relativeTo(sbtBuildDir, destinationPaths.bleepImportDir)
      implicit val wd = sbtBuildDir
      cli(
        List(
          "sbt",
          s"""'set Global / bloopConfigDir := baseDirectory.value / s"$importDir/bloop/$${scalaBinaryVersion.value}"'""",
          "+bloopInstall",
          s"""'set ThisBuild / exportProjectsTo := baseDirectory.value / s"$importDir/sbt-export"'""",
          "+exportAllProjects"
        ).mkString(" "),
        logger
      )
    } finally Files.delete(tempAddBloopPlugin)
  }

  /** @param hackDropBleepDependency
    *   a bit of technical debt. For tests we cannot resolve the bleep dependency since it's not published anywhere. Let's get back to this
    * @return
    */
  def generateBuild(
      inputProjects: ImportInputProjects,
      hackDropBleepDependency: Boolean,
      skipGeneratedResourcesScript: Boolean,
      maybeExistingBleepJson: Option[model.Build]
  ): Map[Path, String] = {

    val build0 = importBloopFilesFromSbt(logger, sbtBuildDir, destinationPaths, inputProjects, bleepVersion)
    val normalizedBuild = normalizeBuild(build0)

    val build1 = Templates(logger, normalizedBuild, options.ignoreWhenInferringTemplates)

    val build =
      maybeExistingBleepJson match {
        case Some(existingBuild) => build1.copy(scripts = existingBuild.scripts)
        case None                => build1
      }

    // complain if we have done illegal rewrites during templating
    ExplodedBuild.diffProjects(Defaults.add(normalizedBuild), ExplodedBuild.of(build).dropTemplates) match {
      case empty if empty.isEmpty => ()
      case diffs =>
        logger.error("Project templating did illegal rewrites. Please report this as a bug")
        diffs.foreach { case (projectName, msg) => logger.withContext(projectName).error(msg) }
    }

    logger.info(s"Imported ${build0.projects.size} cross targets for ${build.projects.value.size} projects")

    val generatedFiles: Map[model.CrossProjectName, Vector[GeneratedFile]] =
      if (skipGeneratedResourcesScript) Map.empty else findGeneratedFiles(inputProjects)

    GeneratedFilesScript(generatedFiles) match {
      case Some((className, scriptSource)) =>
        // todo: find a project and use same scala config
        val scalaVersion = normalizedBuild.projects.values.flatMap(_.scala.flatMap(_.version)).maxByOption(_.scalaVersion).orElse(Some(Versions.Scala3))

        val scriptProjectName = model.CrossProjectName(model.ProjectName("scripts"), None)
        val scriptsProject = model.Project(
          `extends` = JsonSet.empty,
          cross = JsonMap.empty,
          folder = None,
          dependsOn = JsonSet.empty,
          `source-layout` = None,
          `sbt-scope` = None,
          sources = JsonSet.empty,
          resources = JsonSet.empty,
          dependencies =
            if (hackDropBleepDependency) JsonSet.empty
            else JsonSet(Dep.Scala("build.bleep", "bleep-tasks", constants.BleepVersionTemplate)),
          java = None,
          scala = Some(model.Scala(scalaVersion, Options.empty, None, JsonSet.empty)),
          platform = Some(model.Platform.Jvm(Options.empty, None, Options.empty)),
          isTestProject = None,
          testFrameworks = JsonSet.empty
        )

        val buildWithScript = build.copy(
          projects = build.projects.updated(scriptProjectName.name, scriptsProject),
          scripts = build.scripts.updated(model.ScriptName("generate-resources"), JsonList(List(model.ScriptDef(scriptProjectName, className))))
        )

        val scriptPath = destinationPaths.from(scriptProjectName, scriptsProject).dir / "src/scala/scripts/GenerateResources.scala"
        logger
          .withContext(scriptPath)
          .warn("Created a makeshift script to copy generated resources from sbt directories. You'll need to edit this file and make it generate your files")

        Map(
          scriptPath -> scriptSource,
          destinationPaths.bleepYamlFile -> asYamlString(buildWithScript)
        )
      case None =>
        Map(
          destinationPaths.bleepYamlFile -> asYamlString(build)
        )
    }
  }
}
