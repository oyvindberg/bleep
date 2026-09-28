package bleep
package mavenimport

import bleep.internal.{BleepTemplateLogger, GeneratedFilesScript}
import bleep.rewrites.normalizeBuild
import bleep.templates.{mineTemplates, templatesInfer}
import ryddig.Logger

import java.nio.file.Path

object generateBuildFromMaven {
  def apply(
      destinationPaths: BuildPaths,
      logger: Logger,
      options: MavenImportOptions,
      bleepVersion: model.BleepVersion,
      bleepTasksVersion: model.BleepVersion,
      fs: MavenFs,
      mavenProjects: List[MavenProject],
      dependencyList: Path
  ): Map[Path, String] = {

    val build0 = buildFromMavenPom(logger, fs, destinationPaths, mavenProjects, dependencyList, bleepVersion)

    val generatedFiles = buildFromMavenPom.discoverGeneratedFiles(logger, fs, mavenProjects)
    val nonEmptyGeneratedFiles = generatedFiles.filter { case (_, files) => files.nonEmpty }

    val filteredBuild = applyFiltering(build0, options.filtering, logger)

    val normalizedBuild = normalizeBuild(filteredBuild, destinationPaths)

    val buildFile1 = templatesInfer(new BleepTemplateLogger(logger), normalizedBuild, options.ignoreWhenInferringTemplates, mineTemplates.Costs.default)

    logger.info(s"Imported ${filteredBuild.explodedProjects.size} projects for ${buildFile1.projects.value.size} project definitions")

    val hasGeneratedFiles = !options.skipGeneratedResourcesScript && nonEmptyGeneratedFiles.nonEmpty

    if (!hasGeneratedFiles)
      Map(destinationPaths.bleepYamlFile -> yaml.encodeShortened(buildFile1))
    else {
      val generated = GeneratedFilesScript(destinationPaths, bleepTasksVersion, normalizedBuild.explodedProjects.keySet, nonEmptyGeneratedFiles)

      val buildWithScript = buildFile1.copy(
        projects = buildFile1.projects
          .map { case (name, p) => (name, if (generated.projects(name)) p.copy(sourcegen = model.JsonSet(generated.scriptDef)) else p) }
          .updated(GeneratedFilesScript.projectName.name, generated.scriptsProject)
      )

      logger
        .withContext("projects", generated.projects.map(_.value).toList.sorted.mkString(", "))
        .warn(
          s"Files Maven generated are kept in the scripts project, and ${GeneratedFilesScript.className} copies them. You'll need to replace it with code which generates them"
        )

      generated.files.updated(destinationPaths.bleepYamlFile, yaml.encodeShortened(buildWithScript))
    }
  }

  private def applyFiltering(build: model.Build.Exploded, filtering: sbtimport.ImportFiltering, logger: Logger): model.Build.Exploded = {
    val originalCount = build.explodedProjects.size

    val allExcludedProjects = if (filtering.excludeProjects.nonEmpty) {
      calculateProjectsToExclude(build, filtering.excludeProjects, logger)
    } else {
      Set.empty[model.ProjectName]
    }

    val filteredProjects = build.explodedProjects.filter { case (crossProjectName, project) =>
      val isExcluded = allExcludedProjects.contains(crossProjectName.name)

      val scalaVersionMatches = filtering.filterScalaVersions match {
        case None                  => true
        case Some(allowedVersions) =>
          project.scala.flatMap(_.version) match {
            case Some(projectScalaVersion) => allowedVersions.toList.contains(projectScalaVersion)
            case None                      => true // Keep Java projects when filtering by Scala version
          }
      }

      val platformMatches = filtering.filterPlatforms match {
        case None                   => true
        case Some(allowedPlatforms) =>
          project.platform match {
            case Some(platform) => platform.name.exists(allowedPlatforms.toList.contains)
            case None           => allowedPlatforms.toList.contains(model.PlatformId.Jvm)
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
    val downstreamProjects = Set.newBuilder[model.ProjectName]

    build.explodedProjects.keys.foreach { crossProjectName =>
      val transitiveDeps = build.transitiveDependenciesFor(crossProjectName)
      val dependsOnExcluded = transitiveDeps.keys.exists(depCrossName => excludeProjects.contains(depCrossName.name))

      if (dependsOnExcluded) {
        downstreamProjects += crossProjectName.name
      }
    }

    val downstreamProjectNames = downstreamProjects.result()
    val allExcluded = excludeProjects ++ downstreamProjectNames

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
