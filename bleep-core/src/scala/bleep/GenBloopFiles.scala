package bleep

import bleep.internal.{dependencyOrdering, rewriteDependentData, FileUtils, Replacements, ScalaVersions}
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
  def apply(
      logger: Logger,
      buildPaths: BuildPaths,
      lazyResolver: Lazy[CoursierResolver],
      explodedBuild: ExplodedBuild,
      fetchNode: FetchNode
  ): GenBloopFiles.Files
}

object GenBloopFiles {
  type Files = SortedMap[model.CrossProjectName, Lazy[Config.File]]

  case object InMemory extends GenBloopFiles {
    override def apply(
        logger: Logger,
        buildPaths: BuildPaths,
        lazyResolver: Lazy[CoursierResolver],
        explodedBuild: ExplodedBuild,
        fetchNode: FetchNode
    ): GenBloopFiles.Files =
      rewriteDependentData(explodedBuild.projects).apply { (crossName, project, getDep) =>
        translateProject(
          lazyResolver.forceGet,
          buildPaths,
          crossName,
          project,
          explodedBuild,
          getBloopProject = depName => getDep(depName).forceGet(s"${crossName.value} => ${depName.value}"),
          fetchNode
        )
      }
  }

  case object SyncToDisk extends GenBloopFiles {
    override def apply(
        logger: Logger,
        buildPaths: BuildPaths,
        lazyResolver: Lazy[CoursierResolver],
        explodedBuild: ExplodedBuild,
        fetchNode: FetchNode
    ): GenBloopFiles.Files = {
      val currentHash = List(
        explodedBuild.build.$version,
        explodedBuild.projects.toVector.sortBy(_._1)
      ).hashCode().toString

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
          InMemory(logger, buildPaths, lazyResolver, explodedBuild, fetchNode)

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
      getBloopProject: model.CrossProjectName => Config.File,
      fetchNode: FetchNode
  ): Config.File = {

    val projectPaths: ProjectPaths =
      buildPaths.project(crossName, explodedProject)

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
      Replacements.paths(buildPaths.buildDir, projectPaths.dir) ++ Replacements.targetDir(projectPaths.targetDir)

    val configuredPlatform: Option[Config.Platform] =
      explodedPlatform.map {
        case model.Platform.Js(platform) =>
          Config.Platform.Js(
            Config.JsConfig(
              version = platform.jsVersion match {
                case _ if scalaVersion.fold(false)(_.is3) => ""
                case Some(value)                          => value.scalaJsVersion
                case None                                 => sys.error("missing `version`")
              },
              mode = platform.jsMode.getOrElse(Config.JsConfig.empty.mode),
              kind = platform.jsKind.getOrElse(Config.JsConfig.empty.kind),
              emitSourceMaps = platform.jsEmitSourceMaps.getOrElse(Config.JsConfig.empty.emitSourceMaps),
              jsdom = platform.jsJsdom,
              output = None,
              nodePath = platform.jsNodeVersion.map(fetchNode.apply),
              toolchain = Nil
            ),
            platform.mainClass
          )
        case model.Platform.Jvm(platform) =>
          Config.Platform.Jvm(
            config = Config.JvmConfig(
              home = None,
              options = templateDirs.fill.opts(platform.jvmOptions).render
            ),
            mainClass = platform.mainClass,
            runtimeConfig = Some(Config.JvmConfig(home = None, options = templateDirs.fill.opts(platform.jvmRuntimeOptions).render)),
            classpath = None,
            resources = None
          )
        case model.Platform.Native(platform @ _) => ???
        case other                               => sys.error(s"unexpected: $other")
      }

    val (resolvedDependencies, resolvedRuntimeDependencies) = {
      val fromScalaVersion =
        versions.libraries(isTest = explodedProject.isTestProject.getOrElse(false))

      val inherited =
        build.transitiveDependenciesFor(crossName).flatMap { case (_, p) => p.dependencies.values }

      def providedOrOptional(dep: Dep): Boolean =
        dep.configuration == Configuration.provided || dep.configuration == Configuration.optional

      def resolve(all: JsonSet[Dep]) = {
        val concreteDeps: JsonSet[Dependency] =
          all.map { dep =>
            dep.dependency(versions) match {
              case Left(err)    => throw new BuildException.Text(crossName, err)
              case Right(value) => value
            }
          }

        resolver(concreteDeps, forceScalaVersion = scalaVersion) match {
          case Left(coursierError) => throw BuildException.ResolveError(coursierError, crossName)
          case Right(value)        => value
        }
      }

      // drop provided/optional from inherited deps
      val filteredInherited = inherited.filterNot(providedOrOptional)

      val normal =
        resolve(explodedProject.dependencies.union(JsonSet.fromIterable(filteredInherited ++ fromScalaVersion)))

      val runtime =
        if (explodedProject.dependencies.values.exists(providedOrOptional) || inherited.size != filteredInherited.size) {
          // include optional and provided for deps for this project
          val (optionalsFromProject, restFromProject) =
            explodedProject.dependencies.values.partition(providedOrOptional)

          val noLongerOptionalsFromProject =
            optionalsFromProject.map(_.withConfiguration(Configuration.empty))

          resolve {
            JsonSet.fromIterable(filteredInherited ++ restFromProject ++ noLongerOptionalsFromProject ++ fromScalaVersion)
          }
        } else normal

      (normal, runtime)
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
      JsonSet.fromIterable(allTransitiveTranslated.values.map(_.project.classesDir) ++ resolvedRuntimeDependencies.jars)

    val configuredScala: Option[Config.Scala] =
      scalaVersion.map { scalaVersion =>
        val scalaCompiler: Dependency =
          scalaVersion.compiler.forceDependency(ScalaVersions.Jvm(scalaVersion))

        val resolvedScalaCompiler: List[Path] =
          resolver(JsonSet(scalaCompiler), forceScalaVersion = Some(scalaVersion)) match {
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
          val fromPlatform = versions.compilerPlugin
          val fromScala = maybeScala.fold(JsonSet.empty[Dep])(_.compilerPlugins)
          val specified: JsonSet[Dep] =
            fromPlatform.foldLeft(fromScala) { case (all, dep) => all ++ JsonSet(dep) }

          val deps: JsonSet[Dependency] =
            specified.map(_.forceDependency(ScalaVersions.Jvm(scalaVersion)))

          val jars: Seq[Path] =
            resolver(deps, forceScalaVersion = Some(scalaVersion)) match {
              case Left(coursierError) => throw BuildException.ResolveError(coursierError, crossName)
              case Right(res) =>
                res.fullDetailedArtifacts.collect {
                  case (_, pub, _, Some(file)) if pub.classifier != Classifier.sources && pub.ext == Extension.jar => file.toPath
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

    Config.File(
      "1.4.0",
      Config.Project(
        crossName.value,
        projectPaths.dir,
        Some(buildPaths.buildDir),
        sources = projectPaths.sourcesDirs.all.toList,
        sourcesGlobs = None,
        sourceRoots = None,
        dependencies = JsonSet.fromIterable(allTransitiveTranslated.keys.map(_.value)).values.toList,
        classpath = classPath.values.toList,
        out = projectPaths.targetDir,
        classesDir = projectPaths.classes,
        resources = Some(projectPaths.resourcesDirs.all.toList),
        scala = configuredScala,
        java = Some(Config.Java(options = templateDirs.fill.opts(explodedJava.map(_.options).getOrElse(Options.empty)).render)),
        sbt = None,
        test = if (explodedProject.isTestProject.getOrElse(false)) {
          val default = Config.Test.defaultConfiguration
          val extended = explodedProject.testFrameworks.values.toList match {
            case Nil   => default
            case names => new Config.Test(default.frameworks ++ List(Config.TestFramework(names.map(_.value))), default.options)
          }
          Some(extended)
        } else None,
        platform = configuredPlatform,
        resolution = Some(resolution),
        tags = None
      )
    )
  }
}
