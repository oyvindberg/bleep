package bleep
package sbtimport

import bleep.internal.{BleepTemplateLogger, GeneratedFilesScript}
import bleep.rewrites.{normalizeBuild, Defaults}
import bleep.templates.templatesInfer
import ryddig.Logger

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

    // Apply project name and platform filtering
    val filteredBuild = applyFiltering(build0, options.filtering, logger)

    val normalizedBuild = normalizeBuild(filteredBuild, destinationPaths)

    val buildFile = templatesInfer(new BleepTemplateLogger(logger), normalizedBuild, options.ignoreWhenInferringTemplates)

    val buildFile1 =
      maybeExistingBuildFile match {
        case Some(existingBuild) => buildFile.copy(scripts = existingBuild.scripts)
        case None                => buildFile
      }

    // complain if we have done illegal rewrites during templating
    model.Build.diffProjects(Defaults.add(normalizedBuild, destinationPaths), model.Build.FileBacked(buildFile1).dropBuildFile.dropTemplates) match {
      case empty if empty.isEmpty => ()
      case diffs =>
        logger.error("Project templating did illegal rewrites. Please report this as a bug")
        diffs.foreach { case (projectName, msg) => logger.withContext("projectName", projectName.value).error(msg) }
    }

    logger.info(s"Imported ${filteredBuild.explodedProjects.size} cross targets for ${buildFile1.projects.value.size} projects")

    val scriptsPkg = List("scripts")

    val maybeGenerators =
      if (options.skipGeneratedResourcesScript || inputData.generatedFiles.isEmpty) None
      else Some(GeneratedFilesScript(scriptsPkg, inputData.generatedFiles))

    maybeGenerators match {
      case None => Map(destinationPaths.bleepYamlFile -> yaml.encodeShortened(buildFile1))
      case Some(generators) =>
        val scalaVersion =
          normalizedBuild.explodedProjects.values
            .flatMap(_.scala.flatMap(_.version))
            .maxByOption(_.scalaVersion)
            // avoid picking scala 3 versions lower than what is used to compile the bleep artifacts
            .filter {
              case x if x.is3 && x.scalaVersion < model.VersionScala.Scala3.scalaVersion => false
              case x if x.is212                                                          => false // we don't support 2.12 anymore
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
          sourcegen = model.JsonSet.empty,
          libraryVersionSchemes = model.JsonSet.empty,
          ignoreEvictionErrors = None
        )

        val buildWithScript = buildFile1.copy(
          projects = buildFile1.projects
            .map { case (name, p) =>
              val newP = generators.get(name) match {
                case Some(foundGenerator) =>
                  val scriptDef = model.ScriptDef.Main(scriptProjectName, foundGenerator.qname, model.JsonSet.empty)
                  p.copy(sourcegen = model.JsonSet(scriptDef))
                case None => p
              }
              (name, newP)
            }
            .updated(scriptProjectName.name, scriptsProject)
        )

        val genFiles: Map[Path, String] =
          generators.map { case (_, gen) =>
            destinationPaths.project(scriptProjectName, scriptsProject).dir / s"src/scala/scripts/${gen.className}.scala" -> gen.contents
          }

        logger
          .withContext("paths", genFiles.keySet)
          .warn(
            "Created makeshift (re)source generation scripts which replicates what was generated with sbt. You'll need to edit this file and make it generate your files"
          )

        genFiles.updated(destinationPaths.bleepYamlFile, yaml.encodeShortened(buildWithScript))
    }
  }

  private def applyFiltering(build: model.Build.Exploded, filtering: ImportFiltering, logger: Logger): model.Build.Exploded = {
    val originalCount = build.explodedProjects.size

    // Calculate all projects to exclude (direct + downstream dependencies)
    val allExcludedProjects = if (filtering.excludeProjects.nonEmpty) {
      calculateProjectsToExclude(build, filtering.excludeProjects, logger)
    } else {
      Set.empty[model.ProjectName]
    }

    val filteredProjects = build.explodedProjects.filter { case (crossProjectName, project) =>
      // Filter by excluded project names (including downstream dependencies)
      val isExcluded = allExcludedProjects.contains(crossProjectName.name)

      // Filter by Scala version
      val scalaVersionMatches = filtering.filterScalaVersions match {
        case None => true
        case Some(allowedVersions) =>
          project.scala.flatMap(_.version) match {
            case Some(projectScalaVersion) => allowedVersions.toList.contains(projectScalaVersion)
            case None                      => false // Exclude projects without Scala version when filtering is specified
          }
      }

      // Filter by platform
      val platformMatches = filtering.filterPlatforms match {
        case None => true
        case Some(allowedPlatforms) =>
          project.platform match {
            case Some(platform) =>
              val platformId = platform.name
              allowedPlatforms.toList.contains(platformId)
            case None =>
              // Default to JVM if no platform specified
              allowedPlatforms.toList.contains(model.PlatformId.Jvm)
          }
      }

      !isExcluded && scalaVersionMatches && platformMatches
    }

    val filteredCount = filteredProjects.size
    if (originalCount != filteredCount) {
      logger.info(s"Filtered projects: $originalCount -> $filteredCount")
    }

    build.copy(explodedProjects = filteredProjects)
  }

  private def calculateProjectsToExclude(build: model.Build.Exploded, excludeProjects: Set[model.ProjectName], logger: Logger): Set[model.ProjectName] = {
    // Calculate downstream dependencies using build's transitiveDependenciesFor
    val downstreamProjects = Set.newBuilder[model.ProjectName]

    // For each project in the build, check if it transitively depends on any excluded project
    build.explodedProjects.keys.foreach { crossProjectName =>
      val transitiveDeps = build.transitiveDependenciesFor(crossProjectName)
      val dependsOnExcluded = transitiveDeps.keys.exists(depCrossName => excludeProjects.contains(depCrossName.name))

      if (dependsOnExcluded) {
        downstreamProjects += crossProjectName.name
      }
    }

    val downstreamProjectNames = downstreamProjects.result()
    val allExcluded = excludeProjects ++ downstreamProjectNames

    // Log the exclusion details
    if (excludeProjects.nonEmpty) {
      logger.info(s"Directly excluded projects: ${excludeProjects.map(_.value).mkString(", ")}")

      if (downstreamProjectNames.nonEmpty) {
        logger.info(s"Projects excluded as downstream dependencies: ${downstreamProjectNames.map(_.value).mkString(", ")}")
      }

      logger.info(s"Total excluded projects: ${allExcluded.size}")
    }

    allExcluded
  }
}
