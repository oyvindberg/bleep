package bleep
package internal

import bleep.logging.Logger
import bloop.config.Config
import coursier.core.Configuration

import java.net.URI
import java.nio.file.{Files, Path, Paths}
import scala.collection.mutable
import scala.jdk.CollectionConverters._

object importBloopFilesFromSbt {
  val includedTestFramework: Set[String] =
    Config.Test.defaultConfiguration.frameworks.flatMap(_.names).toSet

  // These correspond to the suffixes generated by the sbt-cross plugin
  val Suffixes = Set("JS", "JVM", "Native", "3", "2_13", "2_12", "2_11")

  // we use this to remove last directory name in cross projects
  val crossProjectDirNames: Set[String] =
    Set("jvm", "js", "native").flatMap(str => List(str, s".$str"))

  def projectName(name: String): model.ProjectName = {
    var ret = name
    var isTest = false
    if (ret.endsWith("-test")) {
      ret = ret.dropRight("-test".length)
      isTest = true
    }
    var isIt = false
    if (ret.endsWith("-it")) {
      ret = ret.dropRight("-it".length)
      isIt = true
    }

    var continue = true
    while (continue) {
      continue = false
      Suffixes.foreach { s =>
        if (ret.endsWith(s)) {
          continue = true
          ret = ret.dropRight(s.length)
        }
      }
    }

    model.ProjectName(if (isTest) s"$ret-test" else if (isIt) s"$ret-it" else ret)
  }

  def cachedFn[In, Out](f: In => Out): (In => Out) = {
    val cache = mutable.Map.empty[In, Out]
    in => cache.getOrElseUpdate(in, f(in))
  }

  def projectsWithSourceFilesByName(bloopFiles: Iterable[Config.File]): Map[model.CrossProjectName, Config.File] = {
    val hasSources: Path => Boolean =
      cachedFn { path =>
        def isSource(path: Path): Boolean =
          path.toString match {
            case p if p.endsWith(".scala") => true
            case p if p.endsWith(".java")  => true
            case _                         => false
          }

        Files.exists(path) && Files.walk(path).filter(isSource).findFirst().isPresent
      }

    bloopFiles
      .filter(bloopFile => bloopFile.project.sources.exists(hasSources.apply))
      .groupBy(file => projectName(file.project.name))
      .flatMap {
        case (name, one) if one.size == 1 =>
          List((model.CrossProjectName(name, None), one.head))
        case (name, files) =>
          files.map { file =>
            val maybeCrossId = model.CrossId.defaultFrom(
              maybeScalaVersion = file.project.scala.map(s => Versions.Scala(s.version)),
              maybePlatformId = file.project.platform.flatMap(p => model.PlatformId.fromName(p.name))
            )

            (model.CrossProjectName(name, maybeCrossId), file)
          }

      }
  }

  private case class Sources(
      sourceLayout: SourceLayout,
      sources: JsonSet[RelPath],
      resources: JsonSet[RelPath]
  )

  sealed abstract class ProjectType(val configuration: Configuration, val sbtScope: String, val testLike: Boolean)

  object ProjectType {
    case object Main extends ProjectType(Configuration.empty, "main", testLike = false)
    case object Test extends ProjectType(Configuration.test, "test", testLike = true)
    case object It extends ProjectType(Configuration("it"), "it", testLike = true)

    def of(projectName: model.ProjectName): ProjectType =
      if (projectName.value.endsWith("-test")) Test
      else if (projectName.value.endsWith("-it")) It
      else Main
  }

  def apply(
      logger: Logger,
      sbtBuildDir: Path,
      destinationPaths: BuildPaths,
      crossBloopProjectFiles: Map[model.CrossProjectName, Config.File],
      sbtExportFiles: Iterable[ReadSbtExportFile.ExportedProject]
  ): ExplodedBuild = {

    val projectNames = crossBloopProjectFiles.keys.map(_.name).toSet

    val projects = crossBloopProjectFiles.map { case (crossName, bloopFile) =>
      val bloopProject = bloopFile.project

      val sbtExportFile =
        sbtExportFiles.find(f => f.bloopName == bloopProject.name && bloopProject.scala.map(_.version).contains(f.scalaVersion.full)).getOrElse {
          sys.error(s"Expected to find a sbt dependency file for ${bloopProject.name}")
        }

      val projectType = ProjectType.of(crossName.name)

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
        JsonSet.fromIterable(bloopProject.dependencies.map(projectName).filter(projectNames))

      val scalaVersion: Option[Versions.Scala] =
        bloopProject.scala.map(s => Versions.Scala(s.version))

      val originalTarget = internal.findOriginalTargetDir.force(crossName, bloopProject)

      val replacementsTarget = Replacements.targetDir(originalTarget)
      val replacementsDirs = Replacements.paths(sbtBuildDir, directory) ++ replacementsTarget
      val replacementsVersions = Replacements.versions(scalaVersion, bloopProject.platform.map(_.name))
      val replacements = replacementsDirs ++ replacementsVersions

      val configuredPlatform: Option[model.Platform] =
        bloopProject.platform.map(translatePlatform(_, replacements, bloopProject.resolution))

      val scalaVersions = ScalaVersions.fromExplodedScalaAndPlatform(scalaVersion, configuredPlatform) match {
        case Left(value)  => throw new BuildException.Text(crossName, value)
        case Right(value) => value
      }

      val sources: Sources = {
        val sourcesRelPaths = {
          val sources = bloopProject.sources.filterNot(_.startsWith(originalTarget))
          JsonSet.fromIterable(sources.map(absoluteDir => RelPath.relativeTo(directory, absoluteDir)))
        }

        val resourcesRelPaths = {
          val resources = bloopProject.resources.getOrElse(Nil).filterNot(_.startsWith(originalTarget))
          JsonSet.fromIterable(resources.map(absoluteDir => RelPath.relativeTo(directory, absoluteDir)))
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
            .map(replacementsVersions.templatize.relPath)

        val shortenedResourcesRelPaths =
          resourcesRelPaths
            .filterNot(inferredSourceLayout.resources(scalaVersion, maybePlatformId, Some(projectType.sbtScope)))
            .map(replacementsVersions.templatize.relPath)

        Sources(inferredSourceLayout, shortenedSourcesRelPaths, shortenedResourcesRelPaths)
      }

      val (compilerPlugins, dependencies) = {
        val providedDeps = scalaVersions.libraries(isTest = projectType.testLike)
        importDeps(logger, projectType, sbtExportFile, crossName, configuredPlatform.flatMap(_.name), providedDeps)
      }

      val configuredJava: Option[model.Java] =
        bloopProject.java.map(translateJava(replacements))

      val configuredScala: Option[model.Scala] =
        bloopProject.scala.map(translateScala(compilerPlugins, replacementsDirs, replacementsVersions, scalaVersions))

      val testFrameworks: JsonSet[model.TestFrameworkName] =
        if (projectType.testLike) {
          val names: List[String] =
            bloopProject.test.toList.flatMap(_.frameworks).flatMap(_.names).filterNot(includedTestFramework)

          JsonSet.fromIterable(names.map(model.TestFrameworkName.apply))
        } else JsonSet.empty

      crossName -> model.Project(
        `extends` = JsonList.empty,
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
        crossBloopProjectFiles.toArray
          .flatMap { case (_, bloopFile) => bloopFile.project.resolution }
          .flatMap(_.modules)
          .distinct
          .flatMap(resolverUsedFor)
          .filterNot(constants.DefaultRepos.contains)
          .distinct
          .toList
      )

    ExplodedBuild(Map.empty, Map.empty, resolvers = buildResolvers, projects, Map.empty)
  }

  def importDeps(
      logger: Logger,
      projectType: ProjectType,
      sbtExportFile: ReadSbtExportFile.ExportedProject,
      crossName: model.CrossProjectName,
      platformName: Option[model.PlatformId],
      providedDeps: Seq[Dep]
  ): (Seq[Dep], Seq[Dep]) = {
    // compare by string to ignore things like configuration
    val providedDepReprs: Set[String] =
      providedDeps.map(_.repr).toSet

    val ctxLogger = logger.withContext(crossName)

    val all = sbtExportFile.dependencies.flatMap { moduleId =>
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
    val plugins = all.collect {
      case dep if dep.configuration == CompilerPluginConfig =>
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

        // main. keep for non-test projects
        case Configuration.empty =>
          if (projectType.testLike) None else Some(dep)

        // keep for all projects since it wont be inherited
        case Configuration.optional | Configuration.provided =>
          Some(dep)

        // I have no idea why this is useful. lets simplify and say its main
        case Configuration.runtime =>
          Some(dep.withConfiguration(Configuration.empty))

        case Configuration.test =>
          // rewrite to main dependency if current project is test/it. test configuration doesn't exist after import
          if (projectType.testLike) Some(dep.withConfiguration(Configuration.empty))
          // drop if it is main
          else None
        case ItConf =>
          // same logic as test
          if (projectType == ProjectType.It) Some(dep.withConfiguration(Configuration.empty))
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
    model.Java(options = Options.parse(java.options, Some(templateDirs)))

  def translatePlatform(platform: Config.Platform, templateDirs: Replacements, resolution: Option[Config.Resolution]): model.Platform =
    platform match {
      case Config.Platform.Js(config, mainClass) =>
        // note, this lives in config.version, but it's blank for scala 3.
        val jsVersion = resolution
          .flatMap(_.modules.find(mod => mod.organization == Versions.scalaJsOrganization.value && mod.name.startsWith("scalajs-library")))
          .map(mod => Versions.ScalaJs(mod.version))
          .getOrElse(throw new BuildException.Text("Couldn't find scalajs-library jar to determine version"))

        val translatedPlatform = model.Platform.Js(
          jsVersion = jsVersion,
          jsMode = Some(config.mode),
          jsKind = Some(config.kind),
          jsEmitSourceMaps = Some(config.emitSourceMaps),
          jsJsdom = config.jsdom,
//          output = config.output.map(output => RelPath.relativeTo(directory, output)),
          jsMainClass = mainClass
        )
        translatedPlatform
      case Config.Platform.Jvm(config, mainClass, runtimeConfig, classpath @ _, resources @ _) =>
        val translatedPlatform = model.Platform.Jvm(
          jvmOptions = Options.parse(config.options, Some(templateDirs)),
          mainClass,
          jvmRuntimeOptions = runtimeConfig.map(rc => Options.parse(rc.options, Some(templateDirs))).getOrElse(Options.empty)
        )
        translatedPlatform
      case Config.Platform.Native(config, mainClass) =>
        val translatedPlatform = model.Platform.Native(
          nativeVersion = Some(Versions.ScalaNative(config.version)),
          nativeMode = Some(config.mode),
          nativeGc = Some(config.gc),
          nativeMainClass = mainClass
        )
        translatedPlatform
    }

  def translateScala(
      compilerPlugins: Seq[Dep],
      replacementsDirs: Replacements,
      replacementsVersions: Replacements,
      scalaVersions: ScalaVersions
  )(s: Config.Scala): model.Scala = {
    val options = Options.parse(s.options, Some(replacementsDirs))

    val notCompilerPlugins = options.values.filter {
      case Options.Opt.Flag(name) if name.startsWith(constants.ScalaPluginPrefix) => false
      case _                                                                      => true
    }

    val filteredCompilerPlugins =
      scalaVersions.compilerPlugin.foldLeft(compilerPlugins) { case (all, fromPlatform) =>
        all.filterNot(_ == fromPlatform)
      }

    model.Scala(
      version = Some(Versions.Scala(s.version)),
      options = replacementsVersions.templatize.opts(new Options(notCompilerPlugins)),
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
      compilerPlugins = JsonSet.fromIterable(filteredCompilerPlugins)
    )
  }
}
