package bleep

import bleep.Options.{Opt, TemplateDirs}
import bleep.internal.{CachingPomReader, ScalaVersions}
import bleep.logging.Logger
import bloop.config.Config
import coursier.core.Configuration
import coursier.{Dependency, Module, ModuleName, Organization}

import java.net.URI
import java.nio.file.{Files, Path, Paths}
import java.util.stream.Collectors
import scala.jdk.CollectionConverters._

object importBloopFilesFromSbt {
  // I'm sure there is a useful difference, but it completely escapes me.
  val DefaultConfigs: Set[Configuration] =
    Set(Configuration.empty, Configuration.compile, Configuration.default)

  def apply(logger: Logger, buildPaths: BuildPaths): model.Build = {
    val projectNames: List[model.ProjectName] =
      Files
        .list(buildPaths.dotBloopDir)
        .filter(x => Files.isRegularFile(x))
        .map(path => model.ProjectName(path.getFileName.toString.replace(".json", "")))
        .collect(Collectors.toList[model.ProjectName])
        .asScala
        .toList

    val bloopProjectFiles: Map[model.ProjectName, Config.File] =
      projectNames
        .map(name => name -> readBloopFile(buildPaths.dotBloopDir, name))
        .toMap
        .filter { case (_, bloopFile) =>
          def isSource(path: Path): Boolean =
            path.toString match {
              case p if p.endsWith(".scala") => true
              case p if p.endsWith(".java")  => true
              case _                         => false
            }
          def hasFiles(path: Path): Boolean = Files.exists(path) && Files.walk(path).filter(isSource).findFirst().isPresent
          (bloopFile.project.sources ++ bloopFile.project.resources.getOrElse(Nil)) exists hasFiles
        }

    val pomReader = CachingPomReader()

    val projects = bloopProjectFiles.map { case (projectName, bloopFile) =>
      val bloopProject = bloopFile.project

      val directory =
        if (bloopProject.directory.startsWith(buildPaths.buildDir / ".sbt/matrix")) {
          def inferDirectory(sources: List[Path]) = {
            val src = Paths.get("src")
            def aboveSrc(p: Path): Option[Path] =
              if (p == null) None
              else if (p.getFileName == src) Some(p.getParent)
              else aboveSrc(p.getParent)

            sources.flatMap(aboveSrc).groupBy(identity).maxBy(_._2.length)._1
          }

          inferDirectory(bloopProject.sources)

        } else bloopProject.directory

      val folder: Option[RelPath] =
        RelPath.relativeTo(buildPaths.buildDir, directory) match {
          case RelPath(List(projectName.value)) => None
          case relPath                          => Some(relPath)
        }

      val dependsOn: JsonSet[model.ProjectName] =
        JsonSet.fromIterable(bloopProject.dependencies.map(model.ProjectName.apply).filter(bloopProjectFiles.contains))

      val scalaVersion: Option[Versions.Scala] =
        bloopProject.scala.map(s => Versions.Scala(s.version))

      val isTest = projectName.value.endsWith("-test")
      val scope = if (isTest) "test" else "main"

      val (sourceLayout, sources, resources) = {
        val sourcesRelPaths: JsonSet[RelPath] =
          JsonSet.fromIterable(bloopProject.sources.map(absoluteDir => RelPath.relativeTo(directory, absoluteDir)))

        val resourcesRelPaths: JsonSet[RelPath] =
          JsonSet.fromIterable(bloopProject.resources.getOrElse(Nil).map(absoluteDir => RelPath.relativeTo(directory, absoluteDir)))

        val inferredSourceLayout: SourceLayout =
          SourceLayout.All.values.maxBy { layout =>
            val fromLayout = layout.sources(scalaVersion, Some(scope))
            val fromProject = sourcesRelPaths
            val matching = fromLayout.intersect(fromProject).size
            val notMatching = fromLayout.removeAll(fromProject).size
            (matching, -notMatching)
          }

        val shortenedSources = sourcesRelPaths.filterNot(inferredSourceLayout.sources(scalaVersion, Some(scope)))
        val shortenedResources = resourcesRelPaths.filterNot(inferredSourceLayout.resources(scalaVersion, Some(scope)))

        (inferredSourceLayout, shortenedSources, shortenedResources)
      }

      val resolution = bloopProject.resolution
        .getOrElse(sys.error(s"Expected bloop file for ${projectName.value} to have resolution"))

      val templateDirs = Options.TemplateDirs(buildPaths.buildDir, directory)

      val configuredPlatform: Option[model.Platform] =
        bloopProject.platform.map(translatePlatform(_, templateDirs))

      val versions: ScalaVersions =
        ScalaVersions.fromExplodedScalaAndPlatform(scalaVersion, configuredPlatform) match {
          case Left(err)    => throw new BuildException.Text(projectName, err)
          case Right(value) => value
        }

      val dependencies: List[Dep] = {
        val parsed: List[ParsedDependency] =
          resolution.modules.map(mod => ParsedDependency(logger, pomReader, versions, mod))

        val allDeps: Set[(Module, String)] =
          parsed.flatMap { case ParsedDependency(_, deps) =>
            deps.collect { case (conf, d) if DefaultConfigs(conf) => d.moduleVersion }
          }.toSet

        // only keep those not referenced by another dependency
        parsed.flatMap { case ParsedDependency(dep, _) =>
          val keep: Boolean =
            dep.dependency(versions) match {
              case Left(err)  => throw new BuildException.Text(projectName, err)
              case Right(dep) => !allDeps.contains(dep.moduleVersion)
            }
          if (keep) Some(dep) else None
        }
      }

      val configuredJava: Option[model.Java] =
        bloopProject.java.map(translateJava(templateDirs))

      val configuredScala: Option[model.Scala] =
        bloopProject.scala.map { bloopScala =>
          versions match {
            case ScalaVersions.Java =>
              throw new BuildException.Text(projectName, "Need a scala version to import scala project")
            case withScala: ScalaVersions.WithScala =>
              translateScala(withScala, pomReader, logger, templateDirs, configuredPlatform)(bloopScala)
          }
        }

      val testFrameworks: JsonSet[model.TestFrameworkName] =
        if (isTest) JsonSet.fromIterable(bloopProject.test.toList.flatMap(_.frameworks).flatMap(_.names).map(model.TestFrameworkName.apply))
        else JsonSet.empty

      projectName -> model.Project(
        `extends` = JsonList.empty,
        folder = folder,
        dependsOn = dependsOn,
        sources = sources,
        resources = resources,
        dependencies = JsonSet.fromIterable(dependencies),
        java = configuredJava,
        scala = configuredScala,
        platform = configuredPlatform,
        `source-layout` = Some(sourceLayout),
        `sbt-scope` = Some(scope),
        testFrameworks = testFrameworks
      )
    }

    val templates = Templates.inferFromExistingProjects(projects.values.toList)

    val shortenedProjects = projects.map { case (projectName, project) =>
      val shortened = Templates.applyTemplates(templates, project)
      (projectName, shortened)
    }

    val buildResolvers: JsonSet[URI] =
      JsonSet.fromIterable(
        bloopProjectFiles
          .flatMap { case (projectName, bloopFile) =>
            bloopFile.project.resolution
              .getOrElse(sys.error(s"Expected bloop file for ${projectName.value} to have resolution"))
              .modules
              .flatMap { mod =>
                val initialOrg = Paths.get(mod.organization.split("\\.").head)
                val uriFragments = mod.artifacts.head.path.iterator().asScala.dropWhile(_ != Paths.get("https")).drop(1).takeWhile(_ != initialOrg)
                if (uriFragments.isEmpty) None
                else Some(URI.create(uriFragments.map(_.toString).mkString("https://", "/", "")))
              }
          }
          .filterNot(_ == Defaults.MavenCentral)
      )

    val templatesMassaged: Option[Map[model.TemplateId, model.Project]] =
      Some(templates.map { case (templateDef, templateProject) => (templateDef.templateName, templateProject) }).filter(_.nonEmpty)

    model.Build("1", templatesMassaged, None, resolvers = buildResolvers, shortenedProjects)
  }

  case class ParsedDependency(dep: Dep, directDeps: List[(Configuration, Dependency)])

  object ParsedDependency {
    case class Variant(needsScala: Boolean, fullCrossVersion: Boolean, forceJvm: Boolean, for3Use213: Boolean, for213Use3: Boolean)

    val ScalaVariants = List(
      Variant(needsScala = true, fullCrossVersion = false, forceJvm = false, for3Use213 = false, for213Use3 = false),
      Variant(needsScala = true, fullCrossVersion = false, forceJvm = false, for3Use213 = false, for213Use3 = true),
      Variant(needsScala = true, fullCrossVersion = false, forceJvm = false, for3Use213 = true, for213Use3 = false),
      Variant(needsScala = true, fullCrossVersion = false, forceJvm = true, for3Use213 = false, for213Use3 = false),
      Variant(needsScala = true, fullCrossVersion = false, forceJvm = true, for3Use213 = false, for213Use3 = true),
      Variant(needsScala = true, fullCrossVersion = false, forceJvm = true, for3Use213 = true, for213Use3 = false),
      Variant(needsScala = true, fullCrossVersion = true, forceJvm = false, for3Use213 = false, for213Use3 = false),
      Variant(needsScala = true, fullCrossVersion = true, forceJvm = true, for3Use213 = false, for213Use3 = false)
    )

    def apply(logger: Logger, pomReader: CachingPomReader, versions: ScalaVersions, mod: Config.Module): ParsedDependency = {
      val variantBySuffix: List[(String, Variant)] =
        ScalaVariants
          .flatMap { case v @ Variant(needsScala, needsFullCrossVersion, forceJvm, for3Use213, for213use3) =>
            versions.fullSuffix(needsScala, needsFullCrossVersion, forceJvm, for3Use213, for213use3).map(s => (s, v))
          }
          .sortBy(-_._1.length) // longest suffix first

      val chosen = variantBySuffix.collectFirst { case (suffix, variant) if mod.name.endsWith(suffix) => (mod.name.dropRight(suffix.length), variant) }

      val dep: Dep = chosen match {
        case None =>
          Dep.Java(mod.organization, mod.name, mod.version)
        case Some((modName, scalaVariant)) =>
          Dep.ScalaDependency(
            Organization(mod.organization),
            ModuleName(modName),
            mod.version,
            fullCrossVersion = scalaVariant.fullCrossVersion,
            forceJvm = scalaVariant.forceJvm,
            for3Use213 = scalaVariant.for3Use213,
            for213Use3 = scalaVariant.for213Use3
          )
      }

      val dependencies: List[(Configuration, Dependency)] =
        mod.artifacts.flatMap {
          case a if a.classifier.nonEmpty => Nil
          case a =>
            val pomPath = findPomPath(a.path)

            pomReader(pomPath) match {
              case Left(errMsg) =>
                logger.warn(s"Couldn't determine dependencies for $mod: $errMsg")
                Nil
              case Right(project) =>
                project.dependencies
            }
        }

      ParsedDependency(dep, dependencies)
    }
  }

  private def findPomPath(jar: Path) = {
    val isIvy = jar.getParent.getFileName.toString == "jars"

    if (isIvy)
      jar.getParent.getParent / "poms" / jar.getFileName.toString.replace(".jar", ".pom")
    else
      jar.getParent / jar.getFileName.toString.replace(".jar", ".pom")
  }

  def translateJava(templateDirs: TemplateDirs)(java: Config.Java): model.Java =
    model.Java(options = Options.parse(java.options, Some(templateDirs)))

  def translatePlatform(platform: Config.Platform, templateDirs: Options.TemplateDirs): model.Platform =
    platform match {
      case Config.Platform.Js(config, mainClass) =>
        val translatedPlatform = model.Platform.Js(
          jsVersion = Some(config.version).filterNot(_.isEmpty).map(Versions.ScalaJs),
          jsMode = Some(config.mode),
          jsKind = Some(config.kind),
          jsEmitSourceMaps = Some(config.emitSourceMaps),
          jsJsdom = config.jsdom,
//          output = config.output.map(output => RelPath.relativeTo(directory, output)),
          jsMainClass = mainClass
        )
        translatedPlatform
      case Config.Platform.Jvm(config, mainClass, runtimeConfig, classpath, resources) =>
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
      versions: ScalaVersions.WithScala,
      pomReader: CachingPomReader,
      logger: Logger,
      templateDirs: Options.TemplateDirs,
      platform: Option[model.Platform]
  )(s: Config.Scala): model.Scala = {
    val options = Options.parse(s.options, Some(templateDirs))

    val (plugins, rest) = options.values.partition {
      case Options.Opt.Flag(name) if name.startsWith(Defaults.ScalaPluginPrefix) => true
      case _                                                                     => false
    }

    val compilerPlugins = plugins.collect { case Opt.Flag(pluginStr) =>
      val jarPath = Paths.get(pluginStr.dropWhile(_ != '/'))
      val pomPath = findPomPath(jarPath)
      val Right(pom) = pomReader(pomPath)
      ParsedDependency(logger, pomReader, versions.asJvm, Config.Module(pom.module.organization.value, pom.module.name.value, pom.version, None, Nil)).dep
    }

    val filteredCompilerPlugins =
      platform.flatMap(_.compilerPlugin).foldLeft(compilerPlugins) { case (all, fromPlatform) =>
        all.filterNot(_ == fromPlatform)
      }

    model.Scala(
      version = Some(Versions.Scala(s.version)),
      options = new Options(rest),
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
