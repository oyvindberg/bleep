package bleep
package internal

import bleep.internal.ImportInputProjects.ProjectType
import bleep.logging.Logger
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
      sourceLayout: SourceLayout,
      sources: JsonSet[RelPath],
      resources: JsonSet[RelPath]
  )

  def apply(logger: Logger, sbtBuildDir: Path, destinationPaths: BuildPaths, inputProjects: ImportInputProjects, bleepVersion: model.Version): ExplodedBuild = {

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

      val dependsOn: JsonSet[model.ProjectName] =
        JsonSet.fromIterable(bloopProject.dependencies.flatMap(inputProjects.byBloopName.get).map(_.name))

      val scalaVersion: Option[VersionScala] =
        bloopProject.scala.map(s => VersionScala(s.version))

      val originalTarget = internal.findOriginalTargetDir.force(crossName, bloopProject)

      val replacements =
        Replacements.paths(sbtBuildDir, directory) ++
          Replacements.targetDir(originalTarget) ++
          Replacements.scope(projectType.sbtScope)

      val replacementsWithVersions = replacements ++ Replacements.versions(scalaVersion, bloopProject.platform.map(_.name), includeEpoch = true)

      val configuredPlatform: Option[model.Platform] =
        bloopProject.platform.map(translatePlatform(_, replacements, bloopProject.resolution))

      val scalaPlatform = VersionScalaPlatform.fromExplodedScalaAndPlatform(scalaVersion, configuredPlatform) match {
        case Left(value)  => throw new BuildException.Text(crossName, value)
        case Right(value) => value
      }

      val sources: Sources = {
        val sourcesRelPaths = {
          val sources = bloopProject.sources.filterNot(_.startsWith(originalTarget))
          JsonSet.fromIterable(sources.map {
            // this case was needed for scalameta, where a stray "semanticdb/semanticdb" relative directory appeared.
            case relative if !relative.isAbsolute => RelPath.relativeTo(directory, bloopProject.workspaceDir.get.resolve(relative))
            case absoluteDir                      => RelPath.relativeTo(directory, absoluteDir)
          })
        }

        val resourcesRelPaths = {
          val resources = bloopProject.resources.getOrElse(Nil).filterNot(_.startsWith(originalTarget))
          JsonSet.fromIterable(resources.map {
            // this case was needed for scalameta, where a stray "semanticdb/semanticdb" relative directory appeared.
            case relative if !relative.isAbsolute => RelPath.relativeTo(directory, bloopProject.workspaceDir.get.resolve(relative))
            case absoluteDir                      => RelPath.relativeTo(directory, absoluteDir)
          })
        }

        val maybePlatformId = configuredPlatform.flatMap(_.name)

        val inferredSourceLayout: SourceLayout =
          SourceLayout.All.values.maxBy { layout =>
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

      val (compilerPlugins, dependencies) = {
        val providedDeps = scalaPlatform.libraries(isTest = projectType.testLike)
        importDeps(logger, inputProject, crossName, configuredPlatform.flatMap(_.name), providedDeps)
      }

      val configuredJava: Option[model.Java] =
        bloopProject.java.map(translateJava(replacements))

      val configuredScala: Option[model.Scala] =
        bloopProject.scala.map(translateScala(compilerPlugins, replacements, scalaPlatform))

      val testFrameworks: JsonSet[model.TestFrameworkName] =
        if (projectType.testLike) {
          val names: List[String] =
            bloopProject.test.toList.flatMap(_.frameworks).flatMap(_.names).filterNot(includedTestFramework)

          JsonSet.fromIterable(names.map(model.TestFrameworkName.apply))
        } else JsonSet.empty

      crossName -> model.Project(
        `extends` = JsonSet.empty,
        cross = JsonMap.empty,
        folder = folder,
        dependsOn = dependsOn,
        sources = sources.sources,
        resources = sources.resources,
        dependencies = JsonSet.fromIterable(dependencies),
        java = configuredJava,
        scala = configuredScala,
        platform = configuredPlatform,
        `source-layout` = Some(sources.sourceLayout),
        `sbt-scope` = Some(projectType.sbtScope),
        isTestProject = if (projectType.testLike) Some(true) else None,
        testFrameworks = testFrameworks
      )
    }

    val buildResolvers: JsonList[model.Repository] =
      JsonList(
        inputProjects.values.values.toArray
          .flatMap(inputProject => inputProject.bloopFile.project.resolution)
          .flatMap(_.modules)
          .distinct
          .flatMap(resolverUsedFor)
          .filterNot(constants.DefaultRepos.contains)
          .distinct
          .toList
      )

    ExplodedBuild(model.Build.empty(bleepVersion).copy(resolvers = buildResolvers), templates = Map.empty, projects = projects)
  }

  def importDeps(
      logger: Logger,
      inputProject: ImportInputProjects.InputProject,
      crossName: model.CrossProjectName,
      platformName: Option[model.PlatformId],
      providedDeps: Seq[Dep]
  ): (Seq[Dep], Seq[Dep]) = {
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
        case Right(dep) =>
          Some(dep)
      }
    }

    val CompilerPluginConfig = Configuration("plugin->default(compile)")
    val CompilerPluginScalaJsTest = Configuration("scala-js-test-plugin")
    val plugins = all.collect {
      case dep if dep.configuration == CompilerPluginConfig || (dep.configuration == CompilerPluginScalaJsTest && inputProject.projectType.testLike) =>
        dep.withConfiguration(Configuration.empty) match {
          case x: Dep.JavaDependency => x
          // always true for compiler plugins. this is really just aesthetic in the generated json file
          case x: Dep.ScalaDependency => x.copy(forceJvm = false)
        }
    }

    val ItConf = Configuration("it")
    val rewritten = all.flatMap { dep =>
      dep.configuration match {
        // treated specially above
        case CompilerPluginConfig => None
        // treated specially above
        case CompilerPluginScalaJsTest => None

        // main. keep for non-test projects
        case Configuration.empty =>
          if (inputProject.projectType.testLike) None else Some(dep)

        // keep for all projects since it wont be inherited
        case Configuration.optional | Configuration.provided =>
          Some(dep)

        // I have no idea why this is useful. lets simplify and say its main
        case Configuration.runtime =>
          Some(dep.withConfiguration(Configuration.empty))

        case Configuration.test =>
          // rewrite to main dependency if current project is test/it. test configuration doesn't exist after import
          if (inputProject.projectType.testLike) Some(dep.withConfiguration(Configuration.empty))
          // drop if it is main
          else None
        case ItConf =>
          // same logic as test
          if (inputProject.projectType == ProjectType.It) Some(dep.withConfiguration(Configuration.empty))
          else None

        // silently drop scala dependencies. we'll add them back later
        case Configuration(scalaTool) if scalaTool.startsWith("scala-tool->") || scalaTool.startsWith("scala-doc-tool->") =>
          None

        case Configuration(other) =>
          ctxLogger.warn(s"dropping because unknown configuration '$other': $dep")
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

  def translateJava(templateDirs: Replacements)(java: Config.Java): model.Java =
    model.Java(options = parseOptionsDropSemanticDb(java.options, Some(templateDirs)))

  def translatePlatform(platform: Config.Platform, templateDirs: Replacements, resolution: Option[Config.Resolution]): model.Platform =
    platform match {
      case Config.Platform.Js(config, mainClass) =>
        val jsVersion = {
          // blank for scala 3.
          val fromPlatform = Option(config.version).filterNot(_.isEmpty)
          // fallback, the original version may have been evicted for a newer one through a dependency, so it's a bit imprecise
          val fromDependencies = resolution.flatMap(_.modules.collectFirst {
            case mod if mod.organization == VersionScalaJs.org.value && mod.name.startsWith("scalajs-library") => mod.version
          })

          fromPlatform
            .orElse(fromDependencies)
            .map(version => VersionScalaJs(version))
            .getOrElse(throw new BuildException.Text("Couldn't find scalajs-library jar to determine version"))
        }

        val translatedPlatform = model.Platform.Js(
          jsVersion = jsVersion,
          jsMode = Some(config.mode),
          jsKind = Some(config.kind),
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
            .getOrElse(Options.empty)
        )
        translatedPlatform
      case Config.Platform.Native(config, mainClass) =>
        val translatedPlatform = model.Platform.Native(
          nativeVersion = Some(VersionScalaNative(config.version)),
          nativeMode = Some(config.mode),
          nativeGc = Some(config.gc),
          nativeMainClass = mainClass
        )
        translatedPlatform
    }

  def translateScala(compilerPlugins: Seq[Dep], replacements: Replacements, scalaPlatform: VersionScalaPlatform)(s: Config.Scala): model.Scala = {
    val options = parseOptionsDropSemanticDb(s.options, Some(replacements))

    val notCompilerPlugins = options.values.filter {
      case Options.Opt.Flag(name) if name.startsWith(constants.ScalaPluginPrefix) => false
      case _                                                                      => true
    }

    val filteredCompilerPlugins: Seq[Dep] =
      compilerOptionsDropSemanticDb {
        scalaPlatform.compilerPlugin.foldLeft(compilerPlugins) { case (all, fromPlatform) => all.filterNot(_ == fromPlatform) }
      }

    val (strict, remainingOptions) = {
      val tpolecat = new TpolecatPlugin(DevMode)

      val tpolecatOptions = Options.parse(tpolecat.scalacOptions(s.version).toList, None)
      if (tpolecatOptions.values.forall(notCompilerPlugins.contains))
        (Some(true), new Options(notCompilerPlugins -- tpolecatOptions.values))
      else
        (None, new Options(notCompilerPlugins))
    }

    model.Scala(
      version = Some(VersionScala(s.version)),
      options = remainingOptions,
      setup = s.setup.map(setup =>
        model.CompileSetup(
          order = Some(setup.order),
          addLibraryToBootClasspath = Some(setup.addLibraryToBootClasspath),
          addCompilerToClasspath = Some(setup.addCompilerToClasspath),
          addExtraJarsToClasspath = Some(setup.addExtraJarsToClasspath),
          manageBootClasspath = Some(setup.manageBootClasspath),
          filterLibraryFromClasspath = Some(setup.filterLibraryFromClasspath)
        )
      ),
      compilerPlugins = JsonSet.fromIterable(filteredCompilerPlugins),
      strict = strict
    )
  }

  // semanticdb flags are added back when bleep is in IDE mode
  def parseOptionsDropSemanticDb(strings: List[String], maybeRelativize: Option[Replacements]) = {
    val opts = Options.parse(strings, maybeRelativize)
    val filtered = opts.values.filterNot(_.render.mkString.contains("semanticdb"))
    Options(filtered)
  }

  def compilerOptionsDropSemanticDb(deps: Seq[Dep]): Seq[Dep] =
    deps.filterNot(_.repr.contains("semanticdb-scalac"))
}
