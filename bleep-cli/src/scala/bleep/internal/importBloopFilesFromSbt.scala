package bleep
package internal

import bleep.internal.ImportInputProjects.ProjectType
import bleep.logging.Logger
import bleep.{model, RelPath}
import bloop.config.Config
import coursier.core.Configuration
import io.github.davidgregory084.{DevMode, TpolecatPlugin}

import java.net.URI
import java.nio.file.{Path, Paths}
import scala.jdk.CollectionConverters._

object importBloopFilesFromSbt {
  val includedTestFramework: Set[String] =
    Config.Test.defaultConfiguration.frameworks.flatMap(_.names).toSet

  // we use this to remove last directory name in cross projects
  val crossProjectDirNames: Set[String] =
    Set("jvm", "js", "native").flatMap(str => List(str, s".$str"))

  private case class Sources(
      sourceLayout: model.SourceLayout,
      sources: model.JsonSet[RelPath],
      resources: model.JsonSet[RelPath]
  )

  def apply(
      logger: Logger,
      sbtBuildDir: Path,
      destinationPaths: BuildPaths,
      inputProjects: ImportInputProjects,
      bleepVersion: model.BleepVersion
  ): model.Build.Exploded = {

    val projects = inputProjects.values.map { case (crossName, inputProject) =>
      val bloopProject = inputProject.bloopFile.project

      val projectType = inputProject.projectType

      val directory =
        if (bloopProject.directory.startsWith(sbtBuildDir / ".sbt/matrix")) {
          def inferDirectory(sources: List[Path]) = {
            val src = Paths.get("src")
            def aboveSrc(p: Path): Option[Path] =
              if (p == null) None
              else if (p.getFileName == src) Some(p.getParent)
              else aboveSrc(p.getParent)

            sources.flatMap(aboveSrc).groupBy(identity).maxBy(_._2.length)._1
          }

          inferDirectory(bloopProject.sources)

        } else if (crossProjectDirNames(bloopProject.directory.getFileName.toString))
          bloopProject.directory.getParent
        else bloopProject.directory

      val folder: Option[RelPath] =
        RelPath.relativeTo(destinationPaths.buildDir, directory) match {
          case RelPath(List(crossName.name.value)) => None
          case relPath                             => Some(relPath)
        }

      val dependsOn: model.JsonSet[model.ProjectName] =
        model.JsonSet.fromIterable(bloopProject.dependencies.flatMap(inputProjects.byBloopName.get).map(_.name))

      val scalaVersion: Option[model.VersionScala] =
        bloopProject.scala.map(s => model.VersionScala(s.version))

      val originalTarget = findOriginalTargetDir.force(crossName, bloopProject)

      val replacements =
        model.Replacements.paths(sbtBuildDir, directory) ++
          model.Replacements.targetDir(originalTarget) ++
          model.Replacements.scope(projectType.sbtScope)

      val configuredPlatform: Option[model.Platform] =
        bloopProject.platform.map(translatePlatform(_, replacements, bloopProject.resolution))

      val versionCombo = model.VersionCombo.fromExplodedScalaAndPlatform(scalaVersion, configuredPlatform).orThrowTextWithContext(crossName)

      val replacementsWithVersions = replacements ++ model.Replacements.versions(None, versionCombo, includeEpoch = false, includeBinVersion = true)

      val sources: Sources = {
        val sourcesRelPaths = {
          val sources = bloopProject.sources.filterNot(_.startsWith(originalTarget))
          model.JsonSet.fromIterable(sources.map {
            // this case was needed for scalameta, where a stray "semanticdb/semanticdb" relative directory appeared.
            case relative if !relative.isAbsolute => RelPath.relativeTo(directory, bloopProject.workspaceDir.get.resolve(relative))
            case absoluteDir                      => RelPath.relativeTo(directory, absoluteDir)
          })
        }

        val resourcesRelPaths = {
          val resources = bloopProject.resources.getOrElse(Nil).filterNot(_.startsWith(originalTarget))
          model.JsonSet.fromIterable(resources.map {
            // this case was needed for scalameta, where a stray "semanticdb/semanticdb" relative directory appeared.
            case relative if !relative.isAbsolute => RelPath.relativeTo(directory, bloopProject.workspaceDir.get.resolve(relative))
            case absoluteDir                      => RelPath.relativeTo(directory, absoluteDir)
          })
        }

        val maybePlatformId = configuredPlatform.flatMap(_.name)

        val inferredSourceLayout: model.SourceLayout =
          model.SourceLayout.All.values.maxBy { layout =>
            val fromLayout = layout.sources(scalaVersion, maybePlatformId, Some(projectType.sbtScope))
            val fromProject = sourcesRelPaths
            val matching = fromLayout.intersect(fromProject).size
            val notMatching = fromLayout.removeAll(fromProject).size
            (matching, -notMatching)
          }

        val shortenedSourcesRelPaths =
          sourcesRelPaths
            .filterNot(inferredSourceLayout.sources(scalaVersion, maybePlatformId, Some(projectType.sbtScope)))
            .map(replacementsWithVersions.templatize.relPath)

        val shortenedResourcesRelPaths =
          resourcesRelPaths
            .filterNot(inferredSourceLayout.resources(scalaVersion, maybePlatformId, Some(projectType.sbtScope)))
            .map(replacementsWithVersions.templatize.relPath)

        Sources(inferredSourceLayout, shortenedSourcesRelPaths, shortenedResourcesRelPaths)
      }

      val depReplacements = model.Replacements.versions(None, versionCombo, includeEpoch = false, includeBinVersion = false)
      val (compilerPlugins, dependencies) = {
        val providedDeps = versionCombo.libraries(isTest = projectType.testLike)
        importDeps(logger, inputProject, crossName, configuredPlatform.flatMap(_.name), providedDeps, depReplacements)
      }

      val configuredJava: Option[model.Java] =
        bloopProject.java.map(translateJava(replacements))

      val configuredScala: Option[model.Scala] =
        bloopProject.scala.map(translateScala(compilerPlugins, replacements, versionCombo, depReplacements))

      val testFrameworks: model.JsonSet[model.TestFrameworkName] =
        if (projectType.testLike) {
          val names: List[String] =
            bloopProject.test.toList.flatMap(_.frameworks).flatMap(_.names).filterNot(includedTestFramework)

          model.JsonSet.fromIterable(names.map(model.TestFrameworkName.apply))
        } else model.JsonSet.empty

      crossName -> model.Project(
        `extends` = model.JsonSet.empty,
        cross = model.JsonMap.empty,
        folder = folder,
        dependsOn = dependsOn,
        sources = sources.sources,
        resources = sources.resources,
        dependencies = model.JsonSet.fromIterable(dependencies),
        java = configuredJava,
        scala = configuredScala,
        platform = configuredPlatform,
        `source-layout` = Some(sources.sourceLayout),
        `sbt-scope` = Some(projectType.sbtScope),
        isTestProject = if (projectType.testLike) Some(true) else None,
        testFrameworks = testFrameworks
      )
    }

    val buildResolvers: model.JsonList[model.Repository] =
      model.JsonList(
        inputProjects.values.values.toArray
          .flatMap(inputProject => inputProject.bloopFile.project.resolution)
          .flatMap(_.modules)
          .distinct
          .flatMap(resolverUsedFor)
          .filterNot(constants.DefaultRepos.contains)
          .distinct
          .toList
      )

    model.Build.Exploded(bleepVersion.latestRelease, projects, buildResolvers, jvm = None, scripts = Map.empty)
  }

  object Configs {
    val CompilerPlugin = Configuration("plugin->default(compile)")
    val CompilerPluginScalaJsTest = Configuration("scala-js-test-plugin")
    val It = Configuration("it")
    val OptionalDefault = Configuration("optional(default)")
  }

  def importDeps(
      logger: Logger,
      inputProject: ImportInputProjects.InputProject,
      crossName: model.CrossProjectName,
      platformName: Option[model.PlatformId],
      providedDeps: Seq[model.Dep],
      replacements: model.Replacements
  ): (Seq[model.Dep], Seq[model.Dep]) = {
    // compare by string to ignore things like configuration
    val providedDepReprs: Set[String] =
      providedDeps.map(_.repr).toSet

    val ctxLogger = logger.withContext(crossName)

    val all = inputProject.sbtExportFile.dependencies.flatMap { moduleId =>
      importModuleId(ctxLogger, moduleId, platformName) match {
        case Left(err) =>
          ctxLogger.warn(s"Couldn't import dependency $moduleId. Dropping. Reason: $err")
          None
        case Right(dep) if providedDepReprs(dep.repr) =>
          None
        case Right(dep) => Some(replacements.templatize.dep(dep))
      }
    }

    val plugins = all.collect {
      case dep
          if dep.configuration == Configs.CompilerPlugin || (dep.configuration == Configs.CompilerPluginScalaJsTest && inputProject.projectType.testLike) =>
        dep
          .withConfiguration(Configuration.empty)
          // always true for compiler plugins. this is really just aesthetic in the generated json file
          .mapScala(_.copy(forceJvm = false))
    }

    // bleep doesn't really do configurations much in the sense ivy does it.
    // the complete set of supported configurations are:
    // - Configuration.empty (compile scope)
    // - Configuration.provided
    // - Configuration.optional
    // test/it in particular are rewritten to Configuration.empty since the projects do not have scope
    val rewritten = all.flatMap { dep =>
      // more than one can be supplied
      val configurations =
        dep.configuration.value
          .split(";")
          .map(Configuration.apply)
          .sortBy(_.value)

      configurations match {
        // drop, treated specially above
        case cs if cs.contains(Configs.CompilerPlugin) || cs.contains(Configs.CompilerPluginScalaJsTest) =>
          None
        // drop. this is a special sbt scope, and scala jars are added in another manner in bleep
        case cs if cs.exists(c => c.value.startsWith("scala-tool") || c.value.startsWith("scala-doc-tool")) =>
          None

        // I'm sure there is a useful difference. whatever. empty is used for main scope
        case Array(Configuration.empty | Configuration.compile | Configuration.defaultCompile | Configuration.default) =>
          Some(dep.withConfiguration(Configuration.empty))

        // rewrite to main dependency if current project is test/it. test configuration doesn't exist after import
        case cs if cs.contains(Configuration.test) && inputProject.projectType.testLike =>
          Some(dep.withConfiguration(Configuration.empty))
        // drop if project is `main` and this dependency is only tagged as `test`
        case Array(Configuration.test) =>
          None

        // same logic for `it` as for test above
        case cs if cs.contains(Configs.It) && inputProject.projectType == ProjectType.It =>
          Some(dep.withConfiguration(Configuration.empty))
        // drop if project is not `it` and this dependency is only tagged as `it`
        case Array(Configs.It) =>
          None

        // just pick one of these, no idea what `optional;provided` is meant to mean
        // keep the dependency for all projects since it wont be inherited
        case cs if cs.contains(Configuration.optional) =>
          Some(dep.withConfiguration(Configuration.optional))
        case cs if cs.contains(Configs.OptionalDefault) =>
          Some(dep.withConfiguration(Configuration.optional))

        case cs if cs.contains(Configuration.provided) =>
          Some(dep.withConfiguration(Configuration.provided))

        // I have no idea why this is useful. lets simplify and say its main
        case cs if cs.contains(Configuration.runtime) =>
          Some(dep.withConfiguration(Configuration.empty))

        case cs =>
          ctxLogger.warn(s"dropping because unknown configuration '${cs.mkString(";")}': $dep")
          None
      }
    }

    (plugins, rewritten)
  }

  def resolverUsedFor(mod: Config.Module): Option[model.Repository] = {
    val https = Path.of("https")
    val jars = Path.of("jars")
    val allAfterHttps = mod.artifacts.head.path
      .iterator()
      .asScala
      .toList
      .dropWhile(_ != https)
      .drop(1)

    if (allAfterHttps.isEmpty) None
    // ivy pattern
    else if (allAfterHttps.contains(jars)) {
      val fullOrg = Path.of(mod.organization)
      val uri = URI.create(allAfterHttps.takeWhile(_ != fullOrg).map(_.toString).mkString("https://", "/", "/"))
      Some(model.Repository.Ivy(None, uri))
    } else {
      val initialOrg = Path.of(mod.organization.split("\\.").head)
      val uri = URI.create(allAfterHttps.takeWhile(_ != initialOrg).map(_.toString).mkString("https://", "/", ""))
      Some(model.Repository.Maven(None, uri))
    }
  }

  def translateJava(templateDirs: model.Replacements)(java: Config.Java): model.Java =
    model.Java(options = parseOptionsDropSemanticDb(java.options, Some(templateDirs)))

  def translatePlatform(platform: Config.Platform, templateDirs: model.Replacements, resolution: Option[Config.Resolution]): model.Platform =
    platform match {
      case Config.Platform.Js(config, mainClass) =>
        val jsVersion = {
          // blank for scala 3.
          val fromPlatform = Option(config.version).filterNot(_.isEmpty)
          // fallback, the original version may have been evicted for a newer one through a dependency, so it's a bit imprecise
          val fromDependencies = resolution.flatMap(_.modules.collectFirst {
            case mod if mod.organization == model.VersionScalaJs.org.value && mod.name.startsWith("scalajs-library") => mod.version
          })

          fromPlatform
            .orElse(fromDependencies)
            .map(version => model.VersionScalaJs(version))
            .toRight("Couldn't find scalajs-library jar to determine version")
            .orThrowText
        }

        val translatedPlatform = model.Platform.Js(
          jsVersion = jsVersion,
          jsMode = Some(conversions.linkerMode.to(config.mode)),
          jsKind = Some(conversions.moduleKindJS.to(config.kind)),
          jsEmitSourceMaps = Some(config.emitSourceMaps),
          jsJsdom = config.jsdom,
          jsNodeVersion = config.nodePath
            .map { nodePath =>
              import scala.sys.process._
              List(nodePath.toString, "--version").!!.drop(1) // initial v
            }
            .orElse(Some(constants.Node)),
//          output = config.output.map(output => RelPath.relativeTo(directory, output)),
          jsMainClass = mainClass
        )
        translatedPlatform
      case Config.Platform.Jvm(config, mainClass, runtimeConfig, classpath @ _, resources @ _) =>
        val translatedPlatform = model.Platform.Jvm(
          jvmOptions = parseOptionsDropSemanticDb(config.options, Some(templateDirs)),
          mainClass,
          jvmRuntimeOptions = runtimeConfig
            .map(rc => parseOptionsDropSemanticDb(rc.options, Some(templateDirs)))
            .getOrElse(model.Options.empty)
        )
        translatedPlatform
      case Config.Platform.Native(config, mainClass) =>
        val translatedPlatform = model.Platform.Native(
          nativeVersion = model.VersionScalaNative(config.version),
          nativeMode = Some(conversions.linkerMode.to(config.mode)),
          nativeGc = Some(config.gc),
          nativeMainClass = mainClass
        )
        translatedPlatform
    }

  def translateScala(compilerPlugins: Seq[model.Dep], replacements: model.Replacements, versionCombo: model.VersionCombo, depReplacements: model.Replacements)(
      s: Config.Scala
  ): model.Scala = {
    val options = parseOptionsDropSemanticDb(s.options, Some(replacements))

    val notCompilerPlugins = options.values.filter {
      case model.Options.Opt.Flag(name) if name.startsWith(constants.ScalaPluginPrefix) => false
      case _                                                                            => true
    }

    val filteredCompilerPlugins: Seq[model.Dep] =
      compilerOptionsDropSemanticDb {
        versionCombo.compilerPlugin.foldLeft(compilerPlugins) { case (all, fromPlatform) => all.filterNot(_ == depReplacements.templatize.dep(fromPlatform)) }
      }

    val (strict, remainingOptions) = {
      val tpolecat = new TpolecatPlugin(DevMode)

      val tpolecatOptions = model.Options.parse(tpolecat.scalacOptions(s.version).toList, None)
      if (tpolecatOptions.values.forall(notCompilerPlugins.contains))
        (Some(true), new model.Options(notCompilerPlugins -- tpolecatOptions.values))
      else
        (None, new model.Options(notCompilerPlugins))
    }

    model.Scala(
      version = Some(model.VersionScala(s.version)),
      options = remainingOptions,
      setup = s.setup.map(setup =>
        model.CompileSetup(
          order = Some(conversions.compileOrder.to(setup.order)),
          addLibraryToBootClasspath = Some(setup.addLibraryToBootClasspath),
          addCompilerToClasspath = Some(setup.addCompilerToClasspath),
          addExtraJarsToClasspath = Some(setup.addExtraJarsToClasspath),
          manageBootClasspath = Some(setup.manageBootClasspath),
          filterLibraryFromClasspath = Some(setup.filterLibraryFromClasspath)
        )
      ),
      compilerPlugins = model.JsonSet.fromIterable(filteredCompilerPlugins),
      strict = strict
    )
  }

  // semanticdb flags are added back when bleep is in IDE mode
  def parseOptionsDropSemanticDb(strings: List[String], maybeRelativize: Option[model.Replacements]) = {
    val opts = model.Options.parse(strings, maybeRelativize)
    val filtered = opts.values.filterNot(_.render.mkString.contains("semanticdb"))
    model.Options(filtered)
  }

  def compilerOptionsDropSemanticDb(deps: Seq[model.Dep]): Seq[model.Dep] =
    deps.filterNot(_.repr.contains("semanticdb-scalac"))
}
