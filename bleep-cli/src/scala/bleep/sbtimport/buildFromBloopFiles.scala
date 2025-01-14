package bleep
package sbtimport

import bleep.internal.conversions
import bleep.nosbt.librarymanagement
import bloop.config.Config
import coursier.core.*
import org.typelevel.sbt.tpolecat.{DevMode, TpolecatPlugin}
import ryddig.Logger

import java.net.URI
import java.nio.file.{Path, Paths}
import scala.jdk.CollectionConverters.IteratorHasAsScala

object buildFromBloopFiles {
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
      inputProjects: ImportInputData,
      bleepVersion: model.BleepVersion
  ): model.Build.Exploded = {

    val projects = inputProjects.projects.map { case (crossName, inputProject) =>
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
          case RelPath(Array(crossName.name.value)) => None
          case relPath                              => Some(relPath)
        }

      val dependsOn: model.JsonSet[model.ProjectName] =
        model.JsonSet.fromIterable(bloopProject.dependencies.flatMap(inputProjects.byBloopName.get).map(_.name))

      val scalaVersion: Option[model.VersionScala] =
        bloopProject.scala.map(s => model.VersionScala(s.version))

      val originalTarget = findOriginalTargetDir.force(crossName, bloopProject)

      val replacements =
        model.Replacements.paths(sbtBuildDir) ++
          model.Replacements.projectPaths(directory) ++
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
        importDeps(
          logger,
          inputProject.sbtExportFile.dependencies,
          inputProject.projectType,
          crossName,
          configuredPlatform.flatMap(_.name),
          providedDeps,
          depReplacements
        )
      }

      val libraryVersionSchemes = {
        val (compilerPlugins, dependencies) = importDeps(
          logger,
          inputProject.sbtExportFile.libraryDependencySchemes,
          inputProject.projectType,
          crossName,
          configuredPlatform.flatMap(_.name),
          Nil,
          depReplacements
        )
        (compilerPlugins ++ dependencies).map(dep => model.LibraryVersionScheme.from(dep).orThrowText)
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
        testFrameworks = testFrameworks,
        sourcegen = model.JsonSet.empty,
        libraryVersionSchemes = model.JsonSet.fromIterable(libraryVersionSchemes)
      )
    }

    val buildResolvers: model.JsonList[model.Repository] =
      model.JsonList(
        inputProjects.projects.values.toArray
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
    val CompileTime = Configuration("compile-time")
  }

  def importDeps(
      logger: Logger,
      dependencies: Seq[librarymanagement.ModuleID],
      projectType: ImportInputData.ProjectType,
      crossName: model.CrossProjectName,
      platformName: Option[model.PlatformId],
      providedDeps: Seq[model.Dep],
      replacements: model.Replacements
  ): (Seq[model.Dep], Seq[model.Dep]) = {
    // compare by string to ignore things like configuration
    val providedDepReprs: Set[String] =
      providedDeps.map(_.repr).toSet

    val ctxLogger = logger.withContext("crossName", crossName.value)

    val all = dependencies.flatMap { moduleId =>
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
      case dep if dep.configuration == Configs.CompilerPlugin || (dep.configuration == Configs.CompilerPluginScalaJsTest && projectType.testLike) =>
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
        case Array(Configuration.empty | Configuration.compile | Configuration.defaultCompile | Configuration.default | Configs.CompileTime) =>
          Some(dep.withConfiguration(Configuration.empty))

        // rewrite to main dependency if current project is test/it. test configuration doesn't exist after import
        case cs if cs.contains(Configuration.test) && projectType.testLike =>
          Some(dep.withConfiguration(Configuration.empty))
        // drop if project is `main` and this dependency is only tagged as `test`
        case Array(Configuration.test) =>
          None

        // same logic for `it` as for test above
        case cs if cs.contains(Configs.It) && projectType == ImportInputData.ProjectType.It =>
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

  def importModuleId(logger: Logger, moduleID: librarymanagement.ModuleID, platformId: Option[model.PlatformId]): Either[String, model.Dep] = {
    val ctxLogger = logger.withContext("moduleIdName", moduleID.name)

    val configuration = moduleID.configurations.fold(Configuration.empty)(Configuration(_))

    moduleID.inclusions.foreach { rule =>
      ctxLogger.warn(s"Dropping inclusion rule $rule")
    }
    moduleID.branchName.foreach { branchName =>
      ctxLogger.warn(s"Dropping branchName $branchName")
    }

    val exclusions: model.JsonMap[Organization, model.JsonSet[ModuleName]] = {
      def mapRule(rule: librarymanagement.InclExclRule): Either[String, (Organization, ModuleName)] =
        if (rule.configurations.nonEmpty) Left(s"Configurations in rule not supported: ${rule.configurations}")
        else if (rule.artifact != "*") Left(s"Only artifact = '*' is supported, not ${rule.artifact}")
        else if (rule.crossVersion != librarymanagement.Disabled) Left(s"Only ModuleVersion.Disabled is supported: ${rule.crossVersion}")
        else Right((Organization(rule.organization), ModuleName(rule.name)))

      model.JsonMap {
        moduleID.exclusions
          .flatMap { rule =>
            mapRule(rule) match {
              case Left(msg) =>
                ctxLogger.warn(s"Dropping exclusion rule $rule: $msg")
                Nil
              case Right(tuple) => List(tuple)
            }
          }
          .groupBy { case (org, _) => org }
          .map { case (org, tuples) => (org, model.JsonSet.fromIterable(tuples.map(_._2))) }
      }
    }

    val publication =
      moduleID.explicitArtifacts.toList match {
        case a :: tail =>
          if (tail.nonEmpty)
            ctxLogger.warn(s"Dropping ${tail.length} explicitArtifacts")

          Publication(
            a.name,
            `type` = Type(a.`type`),
            ext = Extension(a.extension),
            classifier = a.classifier.fold(Classifier.empty)(Classifier.apply)
          )
        case Nil => Publication.empty
      }

    JavaOrScalaModule.parse(platformId, moduleID.organization, moduleID.name, moduleID.crossVersion).map {
      case x: JavaOrScalaModule.JavaModule =>
        val (isSbtPlugin, attrs) = extractIsSbt(moduleID.extraAttributes)
        model.Dep.JavaDependency(
          organization = x.module.organization,
          moduleName = x.module.name,
          version = moduleID.revision,
          attributes = attrs,
          configuration = configuration,
          exclusions = exclusions,
          publication = publication,
          transitive = moduleID.isTransitive,
          isSbtPlugin = isSbtPlugin
        )
      case x: JavaOrScalaModule.ScalaModule =>
        model.Dep.ScalaDependency(
          organization = x.baseModule.organization,
          baseModuleName = x.baseModule.name,
          version = moduleID.revision,
          fullCrossVersion = x.fullCrossVersion,
          forceJvm = x.forceJvm,
          for3Use213 = x.for3Use213,
          for213Use3 = x.for213Use3,
          attributes = moduleID.extraAttributes,
          configuration = configuration,
          exclusions = exclusions,
          publication = publication,
          transitive = moduleID.isTransitive
        )
    }
  }

  sealed trait JavaOrScalaModule

  object JavaOrScalaModule {
    case class JavaModule(module: Module) extends JavaOrScalaModule

    case class ScalaModule(baseModule: Module, fullCrossVersion: Boolean, forceJvm: Boolean, for3Use213: Boolean, for213Use3: Boolean) extends JavaOrScalaModule

    def parse(
        platform: Option[model.PlatformId],
        _org: String,
        _name: String,
        crossVersion: librarymanagement.CrossVersion
    ): Either[String, JavaOrScalaModule] = {
      val org = Organization(_org)
      val name = ModuleName(_name)

      def isForceJvm(prefix: String): Boolean =
        platform match {
          case Some(model.PlatformId.Js)     => prefix.isEmpty
          case Some(model.PlatformId.Native) => prefix.isEmpty
          case _                             => false
        }

      val mod = Module(org, name, Map.empty)

      crossVersion match {
        case librarymanagement.Disabled =>
          Right(JavaOrScalaModule.JavaModule(mod))
        case x: librarymanagement.Binary =>
          Right(JavaOrScalaModule.ScalaModule(mod, fullCrossVersion = false, forceJvm = isForceJvm(x.prefix), for3Use213 = false, for213Use3 = false))
        case x: librarymanagement.Full =>
          Right(JavaOrScalaModule.ScalaModule(mod, fullCrossVersion = true, forceJvm = isForceJvm(x.prefix), for3Use213 = false, for213Use3 = false))
        case _: librarymanagement.Patch =>
          Right(JavaOrScalaModule.ScalaModule(mod, fullCrossVersion = true, forceJvm = true, for3Use213 = false, for213Use3 = false))
        case x: librarymanagement.For2_13Use3 =>
          Right(JavaOrScalaModule.ScalaModule(mod, fullCrossVersion = false, forceJvm = isForceJvm(x.prefix), for3Use213 = false, for213Use3 = true))
        case x: librarymanagement.For3Use2_13 =>
          Right(JavaOrScalaModule.ScalaModule(mod, fullCrossVersion = false, forceJvm = isForceJvm(x.prefix), for3Use213 = true, for213Use3 = false))
        case x =>
          Left(s"CrossVersion $x is not supported")
      }
    }
  }

  def extractIsSbt(attrs: Map[String, String]): (Boolean, Map[String, String]) = {
    var hasSbtVersion = false
    var hasScalaVersion = false
    val b = Map.newBuilder[String, String]
    attrs.foreach { case (k, v) =>
      k.split(":").last match {
        case "sbtVersion"   => hasSbtVersion = true
        case "scalaVersion" => hasScalaVersion = true
        case _              => b += ((k, v))
      }
    }

    if (hasSbtVersion && hasScalaVersion) (true, b.result())
    else (false, attrs)
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
          jsKind = Some(conversions.moduleKindJS.to(config.kind)),
          jsSplitStyle = config.moduleSplitStyle.map(conversions.moduleSplitStyleJS.to),
          jsEmitSourceMaps = Some(config.emitSourceMaps),
          jsJsdom = config.jsdom,
          jsNodeVersion = config.nodePath
            .map { nodePath =>
              import scala.sys.process.*
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
          nativeGc = Some(config.gc),
          nativeMainClass = mainClass,
          nativeBuildTarget = config.buildTarget.map(conversions.nativeBuildTarget.to),
          nativeLinkerReleaseMode = config.nativeModeAndLTO.nativeLinkerReleaseMode.map(conversions.nativeLinkerReleaseMode.to),
          nativeLTO = config.nativeModeAndLTO.lto.map(conversions.nativeLTO.to),
          nativeMultithreading = config.nativeFlags.multithreading,
          nativeOptimize = Some(config.nativeFlags.optimize),
          nativeEmbedResources = Some(config.nativeFlags.embedResources),
          nativeUseIncrementalCompilation = Some(config.nativeFlags.useIncrementalCompilation)
        )
        translatedPlatform
    }

  def translateScala(compilerPlugins: Seq[model.Dep], replacements: model.Replacements, versionCombo: model.VersionCombo, depReplacements: model.Replacements)(
      s: Config.Scala
  ): model.Scala = {
    val options = parseOptionsDropSemanticDb(s.options, Some(replacements))
    val filteredOptions = options.removeAll(versionCombo.compilerOptions)

    val notCompilerPlugins = filteredOptions.values.filter {
      case model.Options.Opt.Flag(name) if name.startsWith(constants.ScalaPluginPrefix) => false
      case _                                                                            => true
    }

    val filteredCompilerPlugins: Seq[model.Dep] =
      compilerOptionsDropSemanticDb {
        versionCombo.compilerPlugin.foldLeft(compilerPlugins) { case (all, fromPlatform) => all.filterNot(_ == depReplacements.templatize.dep(fromPlatform)) }
      }

    val (strict, remainingOptions) = {
      val tpolecat = new TpolecatPlugin(DevMode)

      val tpolecatOptions = tpolecat.scalacOptions(s.version)
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
