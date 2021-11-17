package bleep

import bleep.Options.{Opt, TemplateDirs}
import bleep.model.orderingDep
import bloop.config.Config
import coursier.core.compatibility.xmlParseSax
import coursier.core.{Configuration, Project}
import coursier.maven.PomParser
import coursier.parse.JavaOrScalaDependency
import coursier.{Dependency, Module, ModuleName, Organization}

import java.net.URI
import java.nio.file.{Files, Path, Paths}
import java.util.stream.Collectors
import scala.jdk.CollectionConverters._

object importBloopFilesFromSbt {
  def platformName(platform: Config.Platform): model.PlatformId = model.PlatformId(platform.name)
  def scalaName(version: Versions.Scala): model.ScalaId = model.ScalaId(version.binVersion)

  def apply(buildPaths: BuildPaths): model.Build = {
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
          def hasFiles(path: Path): Boolean = Files.exists(path) && Files.walk(path).findFirst().isPresent
          (bloopFile.project.sources ++ bloopFile.project.resources.getOrElse(Nil)) exists hasFiles
        }

    val projects = bloopProjectFiles.map { case (projectName, bloopFile) =>
      val bloopProject = bloopFile.project

      val folder: Option[RelPath] = {
        RelPath.relativeTo(buildPaths.buildDir, bloopProject.directory) match {
          case RelPath(List(projectName.value)) => None
          case relPath                          => Some(relPath)
        }
      }

      val dependsOn: JsonSet[model.ProjectName] =
        JsonSet.fromIterable(bloopProject.dependencies.map(model.ProjectName.apply).filter(bloopProjectFiles.contains))

      val scalaVersion: Option[Versions.Scala] =
        bloopProject.scala.map(s => Versions.Scala(s.version))

      val isTest = projectName.value.endsWith("-test")
      val scope = if (isTest) "test" else "main"

      val (sourceLayout, sources, resources) = {
        val sourcesRelPaths: JsonSet[RelPath] =
          JsonSet.fromIterable(bloopProject.sources.map(absoluteDir => RelPath.relativeTo(bloopProject.directory, absoluteDir)))

        val resourcesRelPaths: JsonSet[RelPath] =
          JsonSet.fromIterable(bloopProject.resources.getOrElse(Nil).map(absoluteDir => RelPath.relativeTo(bloopProject.directory, absoluteDir)))

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

      val templateDirs = Options.TemplateDirs(buildPaths.buildDir, bloopProject.directory)

      val configuredPlatform: Option[model.Platform] =
        bloopProject.platform.map(translatePlatform(_, templateDirs))

      val platformSuffix =
        configuredPlatform match {
          case Some(x: model.Platform.Js)     => s"sjs${x.version.fold("1")(_.scalaJsBinVersion)}"
          case Some(x: model.Platform.Native) => s"native${x.version.get.scalaNativeBinVersion}"
          case _                              => ""
        }

      val dependencies: List[JavaOrScalaDependency] = {
        val parsed: List[ParsedDependency] =
          resolution.modules.map(mod => ParsedDependency(scalaVersion, mod))

        val activeConfigs: Set[Configuration] =
          Set(Configuration.empty, Configuration.compile, Configuration.default)

        val allDeps: Set[(Module, String)] =
          parsed.flatMap { case ParsedDependency(_, deps) =>
            deps.collect { case (conf, d) if activeConfigs(conf) => d.moduleVersion }
          }.toSet

        // only keep those not referenced by another dependency
        parsed.flatMap { case ParsedDependency(javaOrScalaDependency, _) =>
          val keep: Boolean =
            (scalaVersion, javaOrScalaDependency) match {
              case (Some(scalaVersion), scalaDep: JavaOrScalaDependency.ScalaDependency) =>
                !allDeps.contains(scalaDep.dependency(scalaVersion.binVersion, scalaVersion.scalaVersion, platformSuffix).moduleVersion)
              case (None, _: JavaOrScalaDependency.ScalaDependency) =>
                true
              case (_, javaDep: JavaOrScalaDependency.JavaDependency) =>
                !allDeps(javaDep.dependency.moduleVersion)
            }
          if (keep) Some(javaOrScalaDependency) else None
        }
      }

      val configuredJava: Option[model.Java] =
        bloopProject.java.map(translateJava(templateDirs))

      val configuredScala: Option[model.Scala] =
        bloopProject.scala.map(translateScala(templateDirs, configuredPlatform))

      projectName -> model.Project(
        folder = folder,
        dependsOn = dependsOn,
        sources = sources,
        resources = resources,
        dependencies = JsonSet.fromIterable(dependencies),
        java = configuredJava,
        scala = configuredScala,
        platform = configuredPlatform,
        `source-layout` = Some(sourceLayout),
        `sbt-scope` = Some(scope)
      )
    }

    val buildJava: Option[model.Java] =
      projects.values.flatMap(_.java).toList.distinct.reduceOption(_ intersect _)

    val buildPlatforms: Map[model.PlatformId, model.Platform] = {
      val allPlatforms: List[model.Platform] =
        projects.values.flatMap(_.platform).toList.distinct

      List(
        allPlatforms.collect { case x: model.Platform.Jvm => x }.reduceOption(_.intersectJvm(_)).map(_.copy(`extends` = None)),
        allPlatforms.collect { case x: model.Platform.Js => x }.reduceOption(_.intersectJs(_)).map(_.copy(`extends` = None)),
        allPlatforms.collect { case x: model.Platform.Native => x }.reduceOption(_.intersectNative(_)).map(_.copy(`extends` = None))
      ).flatten.map(x => x.name -> x).toMap
    }

    val buildScalas: Map[model.ScalaId, model.Scala] =
      projects
        .flatMap { case (_, p) => p.scala }
        .groupBy(scala => scala.version.map(scalaName))
        .flatMap {
          case (None, _)                     => Map.empty
          case (Some(scalaId), scalaConfigs) => Map(scalaId -> scalaConfigs.reduce(_.intersect(_)).copy(`extends` = None))
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

    model.Build("1", Some(buildPlatforms), Some(buildScalas), buildJava, None, resolvers = buildResolvers, projects)
  }

  case class ParsedDependency(dep: JavaOrScalaDependency, directDeps: List[(Configuration, Dependency)])

  object ParsedDependency {
    def apply(scalaVersion: Option[Versions.Scala], mod: Config.Module): ParsedDependency = {
      def withConf(dep: Dependency): Dependency =
        mod.configurations.foldLeft(dep)((dep, c) => dep.withConfiguration(Configuration(c)))

      def java = Deps.Java(mod.organization, mod.name, mod.version)

      def parseArtifact(scalaVersion: Versions.Scala): JavaOrScalaDependency = {
        val full = mod.name.indexOf("_" + scalaVersion.scalaVersion)
        val scala = mod.name.indexOf("_" + scalaVersion.binVersion)
        val platform = {
          val sjs1 = mod.name.indexOf("_sjs1")
          val sjs06 = mod.name.indexOf("_sjs0.6")
          if (sjs1 != -1) sjs1 else sjs06
        }

        List(full, scala, platform).filterNot(_ == -1).minOption match {
          case None => java
          case Some(modNameEndIdx) =>
            val dep = Dependency(Module(Organization(mod.organization), ModuleName(mod.name.take(modNameEndIdx))), mod.version)
            JavaOrScalaDependency.ScalaDependency(
              withConf(dep),
              fullCrossVersion = full != -1,
              withPlatformSuffix = platform != -1,
              Set.empty
            )
        }
      }

      val sdep: JavaOrScalaDependency = scalaVersion match {
        case Some(scalaVersion) => parseArtifact(scalaVersion)
        case None               => java
      }

      val dependencies: List[(Configuration, Dependency)] =
        mod.artifacts.flatMap {
          case a if a.classifier.nonEmpty => Nil
          case a =>
            val pomPath = findPomPath(a.path)

            val parsedPom: Either[String, Project] =
              if (Files.exists(pomPath)) xmlParseSax(Files.readString(pomPath), new PomParser).project
              else Left(s"$pomPath doesn't exist")

            parsedPom match {
              case Left(errMsg) =>
                println(s"Couldn't determine dependencies for $mod: $errMsg")
                Nil
              case Right(project) =>
                project.dependencies
            }
        }

      // todo: investigate native-image bug around here
      import io.circe.syntax._
      import model.{decodesDep, encodesDep}
      io.circe.parser.decode[JavaOrScalaDependency](sdep.asJson.spaces2) match {
        case Left(x)  => throw x
        case Right(x) => ParsedDependency(x, dependencies)
      }
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
          `extends` = Some(platformName(platform)),
          version = Some(config.version).filterNot(_.isEmpty).map(Versions.ScalaJs),
          mode = Some(config.mode),
          kind = Some(config.kind),
          emitSourceMaps = Some(config.emitSourceMaps),
          jsdom = config.jsdom,
//          output = config.output.map(output => RelPath.relativeTo(bloopProject.directory, output)),
          mainClass = mainClass
        )
        translatedPlatform
      case Config.Platform.Jvm(config, mainClass, runtimeConfig, classpath, resources) =>
        val translatedPlatform = model.Platform.Jvm(
          `extends` = Some(platformName(platform)),
          options = Options.parse(config.options, Some(templateDirs)),
          mainClass,
          runtimeOptions = runtimeConfig.map(rc => Options.parse(rc.options, Some(templateDirs))).getOrElse(Options.empty)
        )
        translatedPlatform
      case Config.Platform.Native(config, mainClass) =>
        val translatedPlatform = model.Platform.Native(
          `extends` = Some(platformName(platform)),
          version = Some(Versions.ScalaNative(config.version)),
          mode = Some(config.mode),
          gc = Some(config.gc),
          mainClass = mainClass
        )
        translatedPlatform
    }

  def translateScala(templateDirs: Options.TemplateDirs, platform: Option[model.Platform])(s: Config.Scala): model.Scala = {
    val options = Options.parse(s.options, Some(templateDirs))

    val (plugins, rest) = options.values.partition {
      case Options.Opt.Flag(name) if name.startsWith(Defaults.ScalaPluginPrefix) => true
      case _                                                                     => false
    }

    val version = Versions.Scala(s.version)
    val compilerPlugins = plugins.collect { case Opt.Flag(pluginStr) =>
      val jarPath = Paths.get(pluginStr.dropWhile(_ != '/'))
      val pomPath = findPomPath(jarPath)
      val Right(pom) = xmlParseSax(Files.readString(pomPath), new PomParser).project
      ParsedDependency(Some(version), Config.Module(pom.module.organization.value, pom.module.name.value, pom.version, None, Nil)).dep
    }

    val filteredCompilerPlugins =
      platform.flatMap(_.compilerPlugin).foldLeft(compilerPlugins) { case (all, fromPlatform) =>
        all.filterNot(_ == fromPlatform)
      }

    model.Scala(
      `extends` = Some(scalaName(version)),
      version = Some(version),
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
