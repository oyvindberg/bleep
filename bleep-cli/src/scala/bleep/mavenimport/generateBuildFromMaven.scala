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

    val build0 = buildFromMavenPom(logger, fs, destinationPaths, mavenProjects, dependencyList, bleepVersion, options.buildJvm)

    val generatedFiles = buildFromMavenPom.discoverGeneratedFiles(logger, fs, mavenProjects)
    val nonEmptyGeneratedFiles = generatedFiles.filter { case (_, files) => files.nonEmpty }

    val filteredBuild = applyFiltering(internal.dropUnsupportedScala(logger, build0), options.filtering, logger)

    val normalizedBuild = normalizeBuild(filteredBuild, destinationPaths)

    val buildFile1 = templatesInfer(new BleepTemplateLogger(logger), normalizedBuild, options.ignoreWhenInferringTemplates, mineTemplates.Costs.default)

    logger.info(s"Imported ${filteredBuild.explodedProjects.size} projects for ${buildFile1.projects.value.size} project definitions")

    val hasGeneratedFiles = !options.skipGeneratedResourcesScript && nonEmptyGeneratedFiles.nonEmpty

    // A project testing with @QuarkusTest needs the serialized application model that
    // bleep-plugin-quarkus generates as sourcegen, plus the two jvm options that deliver it to the
    // test fork. Detection mirrors QuarkusTestModelGen: a test project directly depending on
    // quarkus-junit5 (renamed quarkus-junit in 3.3x).
    val quarkusTestProjectNames: List[model.ProjectName] =
      filteredBuild.explodedProjects.toList
        .collect {
          case (crossName, p)
              if p.isTestProject.contains(true) &&
                p.dependencies.values
                  .exists(dep => dep.organization.value == "io.quarkus" && Set("quarkus-junit5", "quarkus-junit")(dep.baseModuleName.value)) =>
            crossName.name
        }
        .distinct
        .sorted

    if (!hasGeneratedFiles && quarkusTestProjectNames.isEmpty)
      Map(destinationPaths.bleepYamlFile -> yaml.encodeShortened(buildFile1))
    else {
      // The makeshift generated-files copy script, present only when Maven generated sources/resources. Quarkus still needs a scripts project even without it.
      val generatedOpt: Option[GeneratedFilesScript.Generated] =
        if (hasGeneratedFiles) Some(GeneratedFilesScript(destinationPaths, bleepTasksVersion, normalizedBuild.explodedProjects.keySet, nonEmptyGeneratedFiles))
        else None

      // Start from the generated-files script's project when it exists, else a bare JVM scripts project the same shape it builds, then add bleep-plugin-quarkus
      // when a Quarkus test project was detected — it carries QuarkusTestModelGen, referenced as sourcegen from the template below.
      val baseScriptsProject: model.Project =
        generatedOpt
          .map(_.scriptsProject)
          .getOrElse(
            model.Project.empty.copy(
              dependencies = model.JsonSet(model.Dep.Java("build.bleep", "bleepscript", bleepTasksVersion.value)),
              platform = Some(model.Platform.Jvm(model.Options.empty, None, model.Options.empty))
            )
          )
      val scriptsProject =
        if (quarkusTestProjectNames.isEmpty) baseScriptsProject
        else
          baseScriptsProject.copy(dependencies =
            baseScriptsProject.dependencies ++ model.JsonSet(model.Dep.Java("build.bleep", "bleep-plugin-quarkus", bleepTasksVersion.value))
          )

      val buildWithScript = buildFile1.copy(
        projects = buildFile1.projects
          .map { case (name, p) => (name, if (generatedOpt.exists(_.projects(name))) p.copy(sourcegen = model.JsonSet(generatedOpt.get.scriptDef)) else p) }
          .updated(GeneratedFilesScript.projectName.name, scriptsProject)
      )

      val buildWithQuarkus =
        if (quarkusTestProjectNames.isEmpty) buildWithScript
        else {
          val templateId = model.TemplateId("template-quarkus-test")
          // No platform block: the sourcegen declares the fork's JVM options (serialized-model path, jboss LogManager) by writing them to the project's
          // forkJvmOptions file. DevServices containers are labeled per test JVM, so suites sharing a booted application need maven's one-JVM semantics —
          // one shared per-project fork, suites sequential — which is bleep's per-project default made explicit.
          val quarkusTemplate = model.Project.empty.copy(
            maxConcurrentSuites = Some(1),
            testFork = Some(model.TestForkMode.PerProject),
            sourcegen = model.JsonSet[model.ScriptDef](
              model.ScriptDef.Main(
                GeneratedFilesScript.projectName,
                "bleep.plugin.quarkus.QuarkusTestModelGen",
                model.JsonSet.empty[bleep.RelPath],
                model.JsonSet.empty[model.CrossProjectName]
              )
            )
          )

          logger.info(
            s"Detected ${quarkusTestProjectNames.size} Quarkus test projects (${quarkusTestProjectNames.map(_.value).mkString(", ")}): wired bleep-plugin-quarkus via $templateId"
          )

          // bleep-plugin-quarkus also carries the dev-mode and packaging entry points. Register them as scripts so a Quarkus app can be run or packaged out of
          // the box: `bleep quarkus-dev <app>` (live-reload dev mode) and `bleep quarkus-package <app>` (fast-jar). Both take the app project as their argument.
          def quarkusScript(main: String): model.JsonList[model.ScriptDef] =
            model.JsonList(List[model.ScriptDef](model.ScriptDef.Main(GeneratedFilesScript.projectName, main, model.JsonSet.empty, model.JsonSet.empty)))

          buildWithScript.copy(
            templates = buildWithScript.templates.updated(templateId, quarkusTemplate),
            scripts = buildWithScript.scripts
              .updated(model.ScriptName("quarkus-dev"), quarkusScript("bleep.plugin.quarkus.QuarkusRun"))
              .updated(model.ScriptName("quarkus-package"), quarkusScript("bleep.plugin.quarkus.QuarkusPackage")),
            projects = buildWithScript.projects.map { case (name, p) =>
              if (quarkusTestProjectNames.contains(name)) (name, p.copy(`extends` = p.`extends` ++ model.JsonSet(templateId)))
              else (name, p)
            }
          )
        }

      generatedOpt match {
        case Some(generated) =>
          logger
            .withContext("projects", generated.projects.map(_.value).toList.sorted.mkString(", "))
            .warn(
              s"Files Maven generated are kept in the scripts project, and ${GeneratedFilesScript.className} copies them. You'll need to replace it with code which generates them"
            )
          generated.files.updated(destinationPaths.bleepYamlFile, yaml.encodeShortened(buildWithQuarkus))
        case None =>
          Map(destinationPaths.bleepYamlFile -> yaml.encodeShortened(buildWithQuarkus))
      }
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
