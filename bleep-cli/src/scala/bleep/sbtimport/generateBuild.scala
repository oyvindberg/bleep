package bleep
package sbtimport

import bleep.internal.{BleepTemplateLogger, GeneratedFilesScript}
import bleep.logging.Logger
import bleep.rewrites.{normalizeBuild, Defaults}
import bleep.templates.templatesInfer

import java.nio.file.Path

object generateBuild {
  def apply(
      sbtBuildDir: Path,
      destinationPaths: BuildPaths,
      logger: Logger,
      options: ImportOptions,
      bleepVersion: model.BleepVersion,
      inputData: ImportInputData,
      bleepTasksVersion: model.BleepVersion,
      maybeExistingBuildFile: Option[model.BuildFile]
  ): Map[Path, String] = {

    val build0 = buildFromBloopFiles(logger, sbtBuildDir, destinationPaths, inputData, bleepVersion)
    val normalizedBuild = normalizeBuild(build0)

    val buildFile = templatesInfer(new BleepTemplateLogger(logger), normalizedBuild, options.ignoreWhenInferringTemplates)

    val buildFile1 =
      maybeExistingBuildFile match {
        case Some(existingBuild) => buildFile.copy(scripts = existingBuild.scripts)
        case None                => buildFile
      }

    // complain if we have done illegal rewrites during templating
    model.Build.diffProjects(Defaults.add(normalizedBuild), model.Build.FileBacked(buildFile1).dropBuildFile.dropTemplates) match {
      case empty if empty.isEmpty => ()
      case diffs =>
        logger.error("Project templating did illegal rewrites. Please report this as a bug")
        diffs.foreach { case (projectName, msg) => logger.withContext(projectName).error(msg) }
    }

    logger.info(s"Imported ${build0.explodedProjects.size} cross targets for ${buildFile1.projects.value.size} projects")

    if (options.skipGeneratedResourcesScript) {
      Map(
        destinationPaths.bleepYamlFile -> yaml.encodeShortened(buildFile1)
      )
    } else {
      val (className, scriptSource) = GeneratedFilesScript(inputData.generatedFiles) // todo: find a project and use same scala config
      val scalaVersion =
        normalizedBuild.explodedProjects.values
          .flatMap(_.scala.flatMap(_.version))
          .maxByOption(_.scalaVersion)
          // avoid picking scala 3 versions lower than what is used to compile the bleep artifacts
          .filter {
            case x if x.is3 && x.scalaVersion < model.VersionScala.Scala3.scalaVersion => false
            case _                                                                     => true
          }
          .orElse(Some(model.VersionScala.Scala3))

      val scriptProjectName = model.CrossProjectName(model.ProjectName("scripts"), None)
      val scriptsProject = model.Project(
        `extends` = model.JsonSet.empty,
        cross = model.JsonMap.empty,
        folder = None,
        dependsOn = model.JsonSet.empty,
        `source-layout` = None,
        `sbt-scope` = None,
        sources = model.JsonSet.empty,
        resources = model.JsonSet.empty,
        dependencies = model.JsonSet(model.Dep.Scala("build.bleep", "bleep-core", bleepTasksVersion.value)),
        java = None,
        scala = Some(model.Scala(scalaVersion, model.Options.empty, None, model.JsonSet.empty, strict = None)),
        platform = Some(model.Platform.Jvm(model.Options.empty, None, model.Options.empty)),
        isTestProject = None,
        testFrameworks = model.JsonSet.empty,
        sourcegen = model.JsonSet.empty
      )

      val buildWithScript = buildFile1.copy(
        projects = buildFile1.projects.updated(scriptProjectName.name, scriptsProject),
        scripts = buildFile1.scripts
          .updated(model.ScriptName("generate-resources"), model.JsonList(List(model.ScriptDef.Main(scriptProjectName, className, model.JsonSet.empty))))
      )

      val scriptPath = destinationPaths.project(scriptProjectName, scriptsProject).dir / "src/scala/scripts/GenerateResources.scala"
      logger
        .withContext(scriptPath)
        .warn("Created a makeshift script to copy generated resources from sbt directories. You'll need to edit this file and make it generate your files")

      Map(
        scriptPath -> scriptSource,
        destinationPaths.bleepYamlFile -> yaml.encodeShortened(buildWithScript)
      )
    }
  }
}
