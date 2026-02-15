package bleep
package mavenimport

import bleep.internal.BleepTemplateLogger
import bleep.rewrites.{normalizeBuild, Defaults}
import bleep.templates.templatesInfer
import ryddig.Logger

import java.nio.file.Path

object generateBuildFromMaven {
  def apply(
      destinationPaths: BuildPaths,
      logger: Logger,
      options: MavenImportOptions,
      bleepVersion: model.BleepVersion,
      mavenProjects: List[MavenProject]
  ): Map[Path, String] = {

    val build0 = buildFromMavenPom(logger, destinationPaths, mavenProjects, bleepVersion)

    val filteredBuild = applyFiltering(build0, options.filtering, logger)

    val normalizedBuild = normalizeBuild(filteredBuild, destinationPaths)

    val buildFile = templatesInfer(new BleepTemplateLogger(logger), normalizedBuild, options.ignoreWhenInferringTemplates)

    // Validate no illegal rewrites occurred during templating
    model.Build.diffProjects(Defaults.add(normalizedBuild, destinationPaths), model.Build.FileBacked(buildFile).dropBuildFile.dropTemplates) match {
      case empty if empty.isEmpty => ()
      case diffs =>
        logger.error("Project templating did illegal rewrites. Please report this as a bug")
        diffs.foreach { case (projectName, msg) => logger.withContext("projectName", projectName.value).error(msg) }
    }

    logger.info(s"Imported ${filteredBuild.explodedProjects.size} projects for ${buildFile.projects.value.size} project definitions")

    Map(destinationPaths.bleepYamlFile -> yaml.encodeShortened(buildFile))
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
        case None => true
        case Some(allowedVersions) =>
          project.scala.flatMap(_.version) match {
            case Some(projectScalaVersion) => allowedVersions.toList.contains(projectScalaVersion)
            case None                      => true // Keep Java projects when filtering by Scala version
          }
      }

      val platformMatches = filtering.filterPlatforms match {
        case None => true
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
