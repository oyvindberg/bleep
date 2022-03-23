package bleep

import bleep.internal.{dependencyOrdering, rewriteDependentData, FileUtils, Lazy, Replacements, ScalaVersions}
import bleep.logging.Logger
import bloop.config.{Config, ConfigCodecs}
import com.github.plokhotnyuk.jsoniter_scala.core.{readFromString, writeToString, WriterConfig}
import coursier.core.{Configuration, Extension}
import coursier.{Classifier, Dependency}

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}
import scala.collection.immutable.SortedMap
import scala.util.Try

sealed trait GenBloopFiles {
  def apply(logger: Logger, buildPaths: BuildPaths, lazyResolver: Lazy[CoursierResolver], explodedBuild: ExplodedBuild): GenBloopFiles.Files
}

object GenBloopFiles {
  type Files = SortedMap[model.CrossProjectName, Lazy[Config.File]]

  case object InMemory extends GenBloopFiles {
    override def apply(
        logger: Logger,
        buildPaths: BuildPaths,
        lazyResolver: Lazy[CoursierResolver],
        explodedBuild: ExplodedBuild
    ): GenBloopFiles.Files =
      rewriteDependentData(explodedBuild.projects) { (crossName, project, getDep) =>
        translateProject(
          lazyResolver.forceGet,
          buildPaths,
          crossName,
          project,
          explodedBuild,
          getBloopProject = depName => getDep(depName).forceGet(s"${crossName.value} => ${depName.value}")
        )
      }
  }

  case object SyncToDisk extends GenBloopFiles {
    override def apply(logger: Logger, buildPaths: BuildPaths, lazyResolver: Lazy[CoursierResolver], explodedBuild: ExplodedBuild): GenBloopFiles.Files = {
      val currentHash = explodedBuild.projects.toVector.sortBy(_._1).hashCode().toString
      val oldHash = Try(Files.readString(buildPaths.digestFile, UTF_8)).toOption

      if (oldHash.contains(currentHash)) {
        logger.debug(s"${buildPaths.bleepBloopDir} up to date")

        val map = explodedBuild.projects.map { case (crossProjectName, _) =>
          val load = Lazy(readAndParseBloopFile(buildPaths.bleepBloopDir.resolve(crossProjectName.value + ".json")))
          (crossProjectName, load)
        }
        SortedMap.empty[model.CrossProjectName, Lazy[Config.File]] ++ map
      } else {
        logger.warn(s"Refreshing ${buildPaths.bleepBloopDir}...")

        val bloopFiles =
          InMemory(logger, buildPaths, lazyResolver, explodedBuild)

        val fileMap = encodedFiles(buildPaths, bloopFiles).updated(buildPaths.digestFile, currentHash)

        val synced = FileUtils.syncPaths(
          folder = buildPaths.bleepBloopDir,
          fileMap = fileMap,
          deleteUnknowns = FileUtils.DeleteUnknowns.Yes(maxDepth = Some(1)),
          soft = true
        )

        val syncDetails = synced
          .groupBy { case (_, synced) => synced }
          .map { case (synced, files) => s"$synced: (${files.size})" }
          .mkString(", ")

        logger.warn(s"Wrote ${bloopFiles.size} files to ${buildPaths.bleepBloopDir}: $syncDetails")
        bloopFiles
      }
    }
  }

  def readAndParseBloopFile(file: Path): Config.File = {
    val contents = Files.readString(file)
    parseBloopFile(contents)
  }

  def parseBloopFile(contents: String): Config.File =
    readFromString(contents)(ConfigCodecs.codecFile)

  def encodedFiles(buildPaths: BuildPaths, files: Files): Map[Path, String] =
    files.map { case (projectName, bloopFile) =>
      val string = writeToString(bloopFile.forceGet, WriterConfig.withIndentionStep(2))(ConfigCodecs.codecFile)
      val file = buildPaths.bleepBloopDir / (projectName.value + ".json")
      (file, string)
    }

  def translateProject(
      resolver: CoursierResolver,
      buildPaths: BuildPaths,
      crossName: model.CrossProjectName,
      explodedProject: model.Project,
      build: ExplodedBuild,
      getBloopProject: model.CrossProjectName => Config.File
  ): Config.File = {

    val projectPaths: ProjectPaths =
      buildPaths.from(crossName, explodedProject)

    val allTransitiveTranslated: Map[model.CrossProjectName, Config.File] = {
      val builder = Map.newBuilder[model.CrossProjectName, Config.File]

      def go(cn: model.CrossProjectName): Unit = {
        val p = getBloopProject(cn)
        if (cn == crossName) sys.error(s"project ${crossName.value} transitively depends on itself")
        builder += ((cn, p))
        build.resolvedDependsOn(cn).foreach(go)
      }

      build.resolvedDependsOn(crossName).foreach(go)

      builder.result()
    }

    val maybeScala: Option[model.Scala] =
      explodedProject.scala

    val explodedJava: Option[model.Java] =
      explodedProject.java

    val scalaVersion: Option[Versions.Scala] =
      maybeScala.flatMap(_.version)

    val explodedPlatform: Option[model.Platform] =
      explodedProject.platform.map {
        case model.Platform.Jvm(platform) => platform.union(Defaults.Jvm)
        case platform                     => platform
      }

    val versions: ScalaVersions =
      ScalaVersions.fromExplodedProject(explodedProject) match {
        case Left(err)       => throw new BuildException.Text(crossName, err)
        case Right(versions) => versions
      }

    val templateDirs =
      Replacements.paths(buildPaths.buildDir, projectPaths.dir) ++
        Replacements.targetDir(projectPaths.targetDir) ++
        Replacements.versions(maybeScala.flatMap(_.version), explodedPlatform.flatMap(_.name).map(_.value))

    val configuredPlatform: Option[Config.Platform] =
      explodedPlatform.map {
        case model.Platform.Js(platform) =>
          Config.Platform.Js(
            Config.JsConfig(
              version = platform.jsVersion match {
                case Some(value)                             => value.scalaJsVersion
                case None if scalaVersion.fold(false)(_.is3) => ""
                case None                                    => sys.error("missing `version`")
              },
              mode = platform.jsMode.getOrElse(Config.JsConfig.empty.mode),
              kind = platform.jsKind.getOrElse(Config.JsConfig.empty.kind),
              emitSourceMaps = platform.jsEmitSourceMaps.getOrElse(Config.JsConfig.empty.emitSourceMaps),
              jsdom = platform.jsJsdom,
              output = None,
              nodePath = None,
              toolchain = Nil
            ),
            platform.jsMainClass
          )
        case model.Platform.Jvm(platform) =>
          Config.Platform.Jvm(
            config = Config.JvmConfig(
              home = None,
              options = templateDirs.fill.opts(platform.jvmOptions).render
            ),
            mainClass = platform.jvmMainClass,
            runtimeConfig = Some(Config.JvmConfig(home = None, options = templateDirs.fill.opts(platform.jvmRuntimeOptions).render)),
            classpath = None,
            resources = None
          )
        case model.Platform.Native(platform @ _) => ???
        case other                               => sys.error(s"unexpected: $other")
      }

    val resolvedDependencies: CoursierResolver.Result = {
      val transitiveDeps: JsonSet[Dep] =
        explodedProject.dependencies.union(JsonSet.fromIterable(build.transitiveDependenciesFor(crossName).flatMap { case (_, p) => p.dependencies.values }))

      val concreteDeps: JsonSet[Dependency] =
        transitiveDeps.map { dep =>
          dep.dependency(versions) match {
            case Left(err)    => throw new BuildException.Text(crossName, err)
            case Right(value) => value
          }
        }

      resolver(concreteDeps) match {
        case Left(coursierError) => throw BuildException.ResolveError(coursierError, crossName)
        case Right(value)        => value
      }
    }

    val resolution: Config.Resolution = {
      val modules: List[Config.Module] =
        resolvedDependencies.detailedArtifacts
          .groupBy { case (dep, _, _, _) => dep.module }
          .map { case (module, files) =>
            val (dep, _, _, _) = files.head

            Config.Module(
              organization = module.organization.value,
              name = module.name.value,
              version = dep.version,
              configurations = if (dep.configuration == Configuration.empty) None else Some(dep.configuration.value),
              artifacts = files
                .collect { case (_, pub, _, file) =>
                  Config.Artifact(
                    dep.module.name.value,
                    if (pub.classifier == Classifier.empty) None else Some(pub.classifier.value),
                    checksum = None,
                    file.toPath
                  )
                }
                .toList
                .distinct
            )
          }
          .toList

      Config.Resolution(modules)
    }

    val classPath: JsonSet[Path] =
      JsonSet.fromIterable(allTransitiveTranslated.values.map(_.project.classesDir) ++ resolvedDependencies.jars)

    val configuredScala: Option[Config.Scala] =
      scalaVersion.map { scalaVersion =>
        val scalaCompiler: Dependency =
          scalaVersion.compiler.forceDependency(ScalaVersions.Jvm(scalaVersion))

        val resolvedScalaCompiler: List[Path] =
          resolver(JsonSet(scalaCompiler)) match {
            case Left(coursierError) => throw BuildException.ResolveError(coursierError, crossName)
            case Right(res)          => res.jars
          }

        val setup = {
          val provided = maybeScala.flatMap(_.setup).getOrElse(Defaults.DefaultCompileSetup)
          Config.CompileSetup(
            order = provided.order.get,
            addLibraryToBootClasspath = provided.addLibraryToBootClasspath.get,
            addCompilerToClasspath = provided.addCompilerToClasspath.get,
            addExtraJarsToClasspath = provided.addExtraJarsToClasspath.get,
            manageBootClasspath = provided.manageBootClasspath.get,
            filterLibraryFromClasspath = provided.filterLibraryFromClasspath.get
          )
        }

        val compilerPlugins: Options = {
          val fromPlatform = explodedPlatform.flatMap(_.compilerPlugin)
          val fromScala = maybeScala.fold(JsonSet.empty[Dep])(_.compilerPlugins)
          val specified: JsonSet[Dep] =
            fromPlatform.foldLeft(fromScala) { case (all, dep) => all ++ JsonSet(dep) }

          val deps: JsonSet[Dependency] =
            specified.map(_.forceDependency(ScalaVersions.Jvm(scalaVersion)))

          val jars: Seq[Path] =
            resolver(deps) match {
              case Left(coursierError) => throw BuildException.ResolveError(coursierError, crossName)
              case Right(res) =>
                res.fullDetailedArtifacts.collect {
                  case (_, pub, _, Some(file)) if pub.classifier.isEmpty && pub.ext == Extension.jar => file.toPath
                }
            }

          val relevantJars: Seq[Path] =
            jars.filterNot(resolvedScalaCompiler.toSet)

          Options.fromIterable(relevantJars.map(p => Options.Opt.Flag(s"${constants.ScalaPluginPrefix}:$p")))
        }

        val scalacOptions: Options =
          maybeScala.fold(Options.empty)(_.options).union(compilerPlugins)

        Config.Scala(
          organization = scalaCompiler.module.organization.value,
          name = scalaCompiler.module.name.value,
          version = scalaCompiler.version,
          options = templateDirs.fill.opts(scalacOptions).render,
          jars = resolvedScalaCompiler,
          analysis = Some(projectPaths.incrementalAnalysis),
          setup = Some(setup)
        )
      }

    def sourceLayout = explodedProject.`source-layout` match {
      case Some(sourceLayout) => sourceLayout
      case None               => if (scalaVersion.isDefined) SourceLayout.Normal else SourceLayout.Java
    }
    val replacementsVersions = Replacements.versions(scalaVersion, configuredPlatform.map(_.name))

    val maybePlatform = explodedPlatform.flatMap(_.name)

    val sources: JsonSet[Path] = {
      val fromSourceLayout = sourceLayout.sources(scalaVersion, maybePlatform, explodedProject.`sbt-scope`)
      val fromJson = JsonSet.fromIterable(explodedProject.sources.values.map(replacementsVersions.fill.relPath))
      (fromSourceLayout ++ fromJson).map(projectPaths.dir / _)
    }

    val resources: JsonSet[Path] = {
      val fromJson = JsonSet.fromIterable(explodedProject.resources.values.map(replacementsVersions.fill.relPath))
      val fromSourceLayout = sourceLayout.resources(scalaVersion, maybePlatform, explodedProject.`sbt-scope`)
      (fromSourceLayout ++ fromJson).map(projectPaths.dir / _)
    }

    Config.File(
      "1.4.0",
      Config.Project(
        crossName.value,
        projectPaths.dir,
        Some(buildPaths.buildDir),
        sources = (sources + projectPaths.generatedSourcesDir).values.toList,
        sourcesGlobs = None,
        sourceRoots = None,
        dependencies = JsonSet.fromIterable(allTransitiveTranslated.keys.map(_.value)).values.toList,
        classpath = classPath.values.toList,
        out = projectPaths.targetDir,
        classesDir = projectPaths.classes,
        resources = Some((resources + projectPaths.generatedResourcesDir).values.toList),
        scala = configuredScala,
        java = Some(Config.Java(options = templateDirs.fill.opts(explodedJava.map(_.options).getOrElse(Options.empty)).render)),
        sbt = None,
        test = explodedProject.testFrameworks.values.toList match {
          case Nil   => None
          case names => Some(new Config.Test(List(Config.TestFramework(names.map(_.value))), Config.TestOptions(Nil, Nil)))
        },
        platform = configuredPlatform,
        resolution = Some(resolution),
        tags = None
      )
    )
  }
}
