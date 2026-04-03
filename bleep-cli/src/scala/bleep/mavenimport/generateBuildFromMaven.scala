package bleep
package mavenimport

import bleep.internal.{BleepTemplateLogger, GeneratedFilesScript}
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
      bleepTasksVersion: model.BleepVersion,
      mavenProjects: List[MavenProject]
  ): Map[Path, String] = {

    val build0 = buildFromMavenPom(logger, destinationPaths, mavenProjects, bleepVersion)

    val generatedFiles = buildFromMavenPom.discoverGeneratedFiles(logger, mavenProjects)
    val nonEmptyGeneratedFiles = generatedFiles.filter { case (_, files) => files.nonEmpty }

    val filteredBuild = applyFiltering(build0, options.filtering, logger)

    val normalizedBuild = normalizeBuild(filteredBuild, destinationPaths)

    val buildFile1 = templatesInfer(new BleepTemplateLogger(logger), normalizedBuild, options.ignoreWhenInferringTemplates)

    // Validate no illegal rewrites occurred during templating
    model.Build.diffProjects(Defaults.add(normalizedBuild, destinationPaths), model.Build.FileBacked(buildFile1).dropBuildFile.dropTemplates) match {
      case empty if empty.isEmpty => ()
      case diffs =>
        logger.error("Project templating did illegal rewrites. Please report this as a bug")
        diffs.foreach { case (projectName, msg) => logger.withContext("projectName", projectName.value).error(msg) }
    }

    logger.info(s"Imported ${filteredBuild.explodedProjects.size} projects for ${buildFile1.projects.value.size} project definitions")

    val scriptsPkg = List("scripts")

    if (options.skipGeneratedResourcesScript || nonEmptyGeneratedFiles.isEmpty)
      Map(destinationPaths.bleepYamlFile -> yaml.encodeShortened(buildFile1))
    else {
      val generators = GeneratedFilesScript(scriptsPkg, nonEmptyGeneratedFiles)

      // Scripts project must use a Scala 3 version compatible with bleep-core's TASTy files.
      // bleep-core is compiled with 3.7.4, so scripts need >= 3.7.4 to read its TASTy.
      val bleepScala3 = model.VersionScala("3.7.4")

      val scalaVersion =
        normalizedBuild.explodedProjects.values
          .flatMap(_.scala.flatMap(_.version))
          .maxByOption(_.scalaVersion)
          .filter {
            case x if x.is3 && x.scalaVersion < bleepScala3.scalaVersion => false
            case x if x.is212                                            => false
            case _                                                       => true
          }
          .orElse(Some(bleepScala3))

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
        kotlin = None,
        platform = Some(model.Platform.Jvm(model.Options.empty, None, model.Options.empty)),
        isTestProject = None,
        testFrameworks = model.JsonSet.empty[model.TestFrameworkName],
        sourcegen = model.JsonSet.empty[model.ScriptDef],
        libraryVersionSchemes = model.JsonSet.empty[model.LibraryVersionScheme],
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
          "Created makeshift (re)source generation scripts which replicates what was generated with Maven. You'll need to edit this file and make it generate your files"
        )

      genFiles.updated(destinationPaths.bleepYamlFile, yaml.encodeShortened(buildWithScript))
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
