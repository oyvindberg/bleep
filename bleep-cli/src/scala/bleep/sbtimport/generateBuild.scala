package bleep
package sbtimport

import bleep.internal.{BleepTemplateLogger, GeneratedFilesScript}
import bleep.rewrites.normalizeBuild
import bleep.templates.{mineTemplates, templatesInfer}
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

    val build0 = buildFromBloopFiles(logger, sbtBuildDir, destinationPaths, inputData, bleepVersion, options.filtering.excludeProjects)

    // Apply project name and platform filtering
    val filteredBuild = applyFiltering(build0, options.filtering, logger)

    val normalizedBuild = normalizeBuild(filteredBuild, destinationPaths)

    val buildFile = templatesInfer(new BleepTemplateLogger(logger), normalizedBuild, options.ignoreWhenInferringTemplates, mineTemplates.Costs.default)

    val buildFile1 =
      maybeExistingBuildFile match {
        case Some(existingBuild) => buildFile.copy(scripts = existingBuild.scripts)
        case None                => buildFile
      }

    logger.info(s"Imported ${filteredBuild.explodedProjects.size} cross targets for ${buildFile1.projects.value.size} projects")

    if (options.skipGeneratedResourcesScript || inputData.generatedFiles.isEmpty) Map(destinationPaths.bleepYamlFile -> yaml.encodeShortened(buildFile1))
    else {
      val generated = GeneratedFilesScript(destinationPaths, bleepTasksVersion, normalizedBuild.explodedProjects.keySet, inputData.generatedFiles)

      val buildWithScript = buildFile1.copy(
        projects = buildFile1.projects
          .map { case (name, p) => (name, if (generated.projects(name)) p.copy(sourcegen = model.JsonSet(generated.scriptDef)) else p) }
          .updated(GeneratedFilesScript.projectName.name, generated.scriptsProject)
      )

      logger
        .withContext("projects", generated.projects.map(_.value).toList.sorted.mkString(", "))
        .warn(
          s"Files sbt generated are kept in the scripts project, and ${GeneratedFilesScript.className} copies them. You'll need to replace it with code which generates them"
        )

      generated.files.updated(destinationPaths.bleepYamlFile, yaml.encodeShortened(buildWithScript))
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
        case None                  => true
        case Some(allowedVersions) =>
          project.scala.flatMap(_.version) match {
            case Some(projectScalaVersion) => allowedVersions.toList.contains(projectScalaVersion)
            case None                      => false // Exclude projects without Scala version when filtering is specified
          }
      }

      // Filter by platform
      val platformMatches = filtering.filterPlatforms match {
        case None                   => true
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
