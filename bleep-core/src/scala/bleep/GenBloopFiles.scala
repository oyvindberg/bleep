package bleep

import bleep.internal.{conversions, rewriteDependentData}
import bleep.logging.Logger
import bleep.rewrites.Defaults
import bloop.config.{Config, ConfigCodecs}
import com.github.plokhotnyuk.jsoniter_scala.core.{readFromString, writeToString, WriterConfig}
import coursier.Classifier
import coursier.core.{Configuration, Extension}
import io.github.davidgregory084.{DevMode, TpolecatPlugin}

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}
import scala.collection.immutable.SortedMap
import scala.util.Try

trait GenBloopFiles {
  def apply(
      logger: Logger,
      buildPaths: BuildPaths,
      lazyResolver: Lazy[CoursierResolver],
      build: model.Build,
      fetchNode: FetchNode
  ): GenBloopFiles.Files
}

object GenBloopFiles {
  type Files = SortedMap[model.CrossProjectName, Lazy[Config.File]]

  val InMemory: GenBloopFiles = (_, buildPaths, lazyResolver, build, fetchNode) =>
    rewriteDependentData(build.explodedProjects).apply { (crossName, project, eval) =>
      translateProject(
        lazyResolver.forceGet,
        buildPaths,
        crossName,
        project,
        build,
        getBloopProject = depName => eval(depName).forceGet(s"${crossName.value} => ${depName.value}"),
        fetchNode
      )
    }

  val SyncToDisk: GenBloopFiles = (logger, buildPaths, lazyResolver, build, fetchNode) => {
    val currentHash = List(
      build.$version,
      build.explodedProjects.toVector.sortBy(_._1)
    ).hashCode().toString

    val oldHash = Try(Files.readString(buildPaths.digestFile, UTF_8)).toOption

    if (oldHash.contains(currentHash)) {
      logger.debug(s"${buildPaths.bleepBloopDir} up to date")

      val map = build.explodedProjects.map { case (crossProjectName, _) =>
        val load = Lazy(readAndParseBloopFile(buildPaths.bleepBloopDir.resolve(crossProjectName.value + ".json")))
        (crossProjectName, load)
      }
      SortedMap.empty[model.CrossProjectName, Lazy[Config.File]] ++ map
    } else {
      logger.warn(s"Refreshing ${buildPaths.bleepBloopDir}...")

      val bloopFiles =
        InMemory(logger, buildPaths, lazyResolver, build, fetchNode)

      val fileMap = encodedFiles(buildPaths, bloopFiles).updated(buildPaths.digestFile, currentHash)

      val synced = FileSync.syncPaths(
        folder = buildPaths.bleepBloopDir,
        fileMap = fileMap,
        deleteUnknowns = FileSync.DeleteUnknowns.Yes(maxDepth = Some(1)),
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
      build: model.Build,
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

    val scalaVersion: Option[model.VersionScala] =
      maybeScala.flatMap(_.version)

    val explodedPlatform: Option[model.Platform] =
      explodedProject.platform.map {
        case model.Platform.Jvm(platform) => platform.union(Defaults.Jvm)
        case platform                     => platform
      }

    val versionCombo: model.VersionCombo =
      model.VersionCombo.fromExplodedProject(explodedProject).orThrowTextWithContext(crossName)

    val templateDirs =
      model.Replacements.paths(buildPaths.buildDir, projectPaths.dir) ++
        model.Replacements.targetDir(projectPaths.targetDir) ++
        model.Replacements.versions(Some(build.$version), versionCombo, includeEpoch = true, includeBinVersion = true)

    def require[T](ot: Option[T], name: String): T =
      ot.toRight(s"missing platform field `$name`").orThrowText

    val configuredPlatform: Option[Config.Platform] =
      explodedPlatform.map {
        case model.Platform.Js(platform) =>
          Config.Platform.Js(
            Config.JsConfig(
              version = require(platform.jsVersion, "version").scalaJsVersion,
              mode = platform.jsMode.fold(Config.JsConfig.empty.mode)(conversions.linkerMode.from),
              kind = platform.jsKind.fold(Config.JsConfig.empty.kind)(conversions.moduleKindJS.from),
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
        case model.Platform.Native(platform @ _) =>
          val empty = Config.NativeConfig.empty
          Config.Platform.Native(
            config = Config.NativeConfig(
              version = require(platform.nativeVersion, "version").scalaNativeVersion,
              mode = require(platform.nativeMode.map(conversions.linkerMode.from), "nativeMode"),
              gc = require(platform.nativeGc, "nativeGc"),
              targetTriple = empty.targetTriple,
              clang = empty.clang,
              clangpp = empty.clangpp,
              toolchain = empty.toolchain,
              options = empty.options,
              linkStubs = empty.linkStubs,
              check = empty.check,
              dump = empty.dump,
              output = empty.output
            ),
            platform.mainClass
          )

        case other => sys.error(s"unexpected: $other")
      }

    val (resolvedDependencies, resolvedRuntimeDependencies) = {
      val fromPlatform =
        versionCombo.libraries(isTest = explodedProject.isTestProject.getOrElse(false))

      val inherited =
        build.transitiveDependenciesFor(crossName).flatMap { case (_, p) => p.dependencies.values }

      def providedOrOptional(dep: model.Dep): Boolean =
        dep.configuration == Configuration.provided || dep.configuration == Configuration.optional

      // drop provided/optional from inherited deps
      val filteredInherited = inherited.filterNot(providedOrOptional)

      val deps = explodedProject.dependencies.values ++ (filteredInherited ++ fromPlatform)
      val normal = resolver.force(deps, versionCombo, crossName.value)

      val runtime =
        if (explodedProject.dependencies.values.exists(providedOrOptional) || inherited.size != filteredInherited.size) {
          // include optional and provided for deps for this project
          val (optionalsFromProject, restFromProject) =
            explodedProject.dependencies.values.partition(providedOrOptional)

          val noLongerOptionalsFromProject =
            optionalsFromProject.map(_.withConfiguration(Configuration.empty))

          val deps = (filteredInherited ++ restFromProject ++ noLongerOptionalsFromProject ++ fromPlatform).toSet
          resolver.force(deps, versionCombo, crossName.value)
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

    val classPath: model.JsonSet[Path] =
      model.JsonSet.fromIterable(allTransitiveTranslated.values.map(_.project.classesDir) ++ resolvedRuntimeDependencies.jars)

    val configuredScala: Option[Config.Scala] =
      scalaVersion.map { scalaVersion =>
        val compiler = scalaVersion.compiler.mapScala(_.copy(forceJvm = true)).asJava(versionCombo).getOrElse(sys.error("unexpected"))

        val resolvedScalaCompiler: List[Path] =
          resolver.force(Set(compiler), versionCombo = versionCombo, crossName.value).jars

        val setup = {
          val provided = maybeScala.flatMap(_.setup).getOrElse(Defaults.DefaultCompileSetup)
          Config.CompileSetup(
            order = conversions.compileOrder.from(provided.order.get),
            addLibraryToBootClasspath = provided.addLibraryToBootClasspath.get,
            addCompilerToClasspath = provided.addCompilerToClasspath.get,
            addExtraJarsToClasspath = provided.addExtraJarsToClasspath.get,
            manageBootClasspath = provided.manageBootClasspath.get,
            filterLibraryFromClasspath = provided.filterLibraryFromClasspath.get
          )
        }

        val compilerPlugins: model.Options = {
          val deps: Set[model.Dep] =
            (versionCombo.compilerPlugin.toSet ++ maybeScala.fold(Set.empty[model.Dep])(_.compilerPlugins.values))
              .map(_.withTransitive(false).mapScala(_.copy(forceJvm = true)))

          val jars: Seq[Path] =
            resolver
              .force(deps, versionCombo, crossName.value)
              .fullDetailedArtifacts
              .collect { case (_, pub, _, Some(file)) if pub.classifier != Classifier.sources && pub.ext == Extension.jar => file.toPath }

          val relevantJars: Seq[Path] =
            jars.filterNot(resolvedScalaCompiler.toSet)

          model.Options.fromIterable(relevantJars.map(p => model.Options.Opt.Flag(s"${constants.ScalaPluginPrefix}:$p")))
        }

        val scalacOptions: model.Options =
          maybeScala match {
            case Some(scala) =>
              val base = scala.options.union(compilerPlugins)
              if (scala.strict.getOrElse(false)) {
                val tpolecat = model.Options.parse(new TpolecatPlugin(DevMode).scalacOptions(scalaVersion.scalaVersion).toList, None)
                base.union(tpolecat)
              } else base
            case None => model.Options.empty
          }

        Config.Scala(
          organization = compiler.organization.value,
          name = compiler.moduleName.value,
          version = compiler.version,
          options = templateDirs.fill.opts(scalacOptions).render,
          jars = resolvedScalaCompiler,
          analysis = Some(projectPaths.incrementalAnalysis),
          setup = Some(setup)
        )
      }

    Config.File(
      "1.4.0",
      Config.Project(
        name = crossName.value,
        directory = projectPaths.dir,
        workspaceDir = Some(buildPaths.buildDir),
        sources = projectPaths.sourcesDirs.all.toList,
        sourcesGlobs = None,
        sourceRoots = None,
        dependencies = model.JsonSet.fromIterable(allTransitiveTranslated.keys.map(_.value)).values.toList,
        classpath = classPath.values.toList,
        out = projectPaths.targetDir,
        classesDir = projectPaths.classes,
        resources = Some(projectPaths.resourcesDirs.all.toList),
        scala = configuredScala,
        java = Some(Config.Java(options = templateDirs.fill.opts(explodedJava.map(_.options).getOrElse(model.Options.empty)).render)),
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
