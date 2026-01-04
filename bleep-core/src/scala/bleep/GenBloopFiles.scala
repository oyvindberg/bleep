package bleep

import bleep.internal.{conversions, rewriteDependentData}
import bleep.nosbt.librarymanagement.ScalaArtifacts
import bleep.rewrites.Defaults
import bloop.config.{Config, ConfigCodecs}
import com.github.plokhotnyuk.jsoniter_scala.core.{writeToString, WriterConfig}
import coursier.{Classifier, ModuleName}
import coursier.core.{Configuration, Extension, Organization}
import org.typelevel.sbt.tpolecat.{DevMode, TpolecatPlugin}

import java.io.File
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}
import scala.collection.immutable.{ListSet, SortedMap, SortedSet}
import scala.collection.mutable
import scala.util.Try

trait GenBloopFiles {
  def apply(prebootstrapped: Prebootstrapped, resolver: CoursierResolver, build: model.Build): GenBloopFiles.Files
}

object GenBloopFiles {
  type Files = SortedMap[model.CrossProjectName, Lazy[Config.File]]

  object InMemory extends GenBloopFiles {
    override def apply(pre: Prebootstrapped, resolver: CoursierResolver, build: model.Build): Files =
      rewriteDependentData(build.explodedProjects).apply { (crossName, project, eval) =>
        def get(depName: model.CrossProjectName): Config.File =
          eval(depName).forceGet(s"${crossName.value} => ${depName.value}")

        translateProject(pre, resolver, crossName, project, build, getBloopProject = get)
      }
  }

  /** For integration tests we want to write builds which depend on the version of bleep you're working on, and we want to use the normal syntax (dependency
    * with version = `${BLEEP_VERSION}`).
    *
    * This takes a bootstrapped build of the bleep you're working on, and replaces references to bleep dependencies with the corresponding class directories.
    */
  case class ReplaceBleepDependencies(bleepBuild: Lazy[Started]) extends GenBloopFiles {
    def isBleepDep(dep: model.Dep): Boolean = dep.organization.value == "build.bleep"

    override def apply(pre: Prebootstrapped, resolver: CoursierResolver, build: model.Build): Files = {
      val explodedBuild = build.dropBuildFile

      // save some state from step one to step two
      val b = mutable.Map.empty[model.CrossProjectName, Set[model.CrossProjectName]]

      // first step:
      // rewrite projects to replace dependencies on bleep projects (if any) with just their (external) dependencies
      val rewrittenBuild = {
        val newProjects = explodedBuild.explodedProjects.map { case (crossName, p) =>
          val (bleepDependencies, restDependencies) = p.dependencies.values.partition(isBleepDep)

          def sameBinVersion(p1: model.Project, p2: model.Project): Boolean =
            p1.scala.flatMap(_.version).map(_.binVersion) == p2.scala.flatMap(_.version).map(_.binVersion)

          val transitiveBleepProjectNames: SortedSet[model.CrossProjectName] =
            bleepDependencies.flatMap { bleepDep =>
              val bleepProjectName = bleepBuild.forceGet.build
                .explodedProjectsByName(model.ProjectName(bleepDep.baseModuleName.value))
                .collectFirst { case (bleepProjectName, bp) if sameBinVersion(bp, p) => bleepProjectName }
                .toRight(s"couldn't find $bleepDep in bleep build")
                .orThrowTextWithContext(crossName)

              bleepBuild.forceGet.build.resolvedDependsOn(bleepProjectName) ++ List(bleepProjectName)
            }

          if (transitiveBleepProjectNames.nonEmpty) b(crossName) = transitiveBleepProjectNames

          // include all dependencies inherited from bleep projects
          val newDeps: SortedSet[model.Dep] =
            restDependencies ++ transitiveBleepProjectNames.view.flatMap(pn =>
              bleepBuild.forceGet.build.explodedProjects(pn).dependencies.values.filterNot(isBleepDep)
            )

          (crossName, p.copy(dependencies = model.JsonSet(newDeps)))
        }

        explodedBuild.copy(explodedProjects = newProjects)
      }

      // ensure requested bleep projects are compiled before we run tests
      b.values.flatten.toList.distinct match {
        case Nil      => ()
        case nonEmpty => new Commands(bleepBuild.forceGet).compile(nonEmpty)
      }

      rewriteDependentData(rewrittenBuild.explodedProjects).apply { (crossName, project, eval) =>
        val bloopFile = translateProject(
          pre,
          resolver,
          crossName,
          project,
          build,
          getBloopProject = depName => eval(depName).forceGet(s"${crossName.value} => ${depName.value}")
        )

        // step 2: add classpath for all (transitive) bleep dependencies from the bleep build
        val newClassPath = b.getOrElse(crossName, Nil).toList.map(bleepBuild.forceGet.projectPaths).map(_.classes)
        bloopFile.copy(project = bloopFile.project.copy(classpath = newClassPath ++ bloopFile.project.classpath))
      }
    }
  }

  val SyncToDisk: GenBloopFiles =
    SyncToDiskWith(InMemory)

  case class SyncToDiskWith(next: GenBloopFiles) extends GenBloopFiles {
    override def apply(pre: Prebootstrapped, resolver: CoursierResolver, build: model.Build): Files = {

      val currentHash: Checksums.Digest =
        Checksums.compute(Checksums.Algorithm.Md5) { md =>
          md.update(build.$version.value.getBytes(UTF_8))
          build.explodedProjects.toVector.sortBy { case (cn, _) => cn }.foreach { case (cn, p) =>
            import io.circe.syntax.*
            md.update(cn.value.getBytes(UTF_8))
            md.update(p.asJson.noSpacesSortKeys.getBytes(UTF_8))
          }
        }

      val oldHash = Try(Files.readString(pre.buildPaths.digestFile, UTF_8)).toOption

      if (oldHash.contains(currentHash.hexString)) {
        pre.logger.debug(s"${pre.buildPaths.bleepBloopDir} up to date")
        next(pre, resolver, build)
      } else {
        pre.logger.warn(s"Refreshing ${pre.buildPaths.bleepBloopDir}...")

        val bloopFiles = next(pre, resolver, build)

        val fileMap = encodedFiles(pre.buildPaths, bloopFiles).updated(pre.buildPaths.digestFile, currentHash.hexString)

        FileSync
          .syncPaths(
            folder = pre.buildPaths.bleepBloopDir,
            fileMap = fileMap,
            deleteUnknowns = FileSync.DeleteUnknowns.Yes(maxDepth = Some(1)),
            soft = true
          )
          .log(pre.logger, "wrote bloop files")

        bloopFiles
      }
    }
  }

  def encodedFiles(buildPaths: BuildPaths, files: Files): Map[Path, String] =
    files.map { case (projectName, bloopFile) =>
      val string = writeToString(bloopFile.forceGet, WriterConfig.withIndentionStep(2))(ConfigCodecs.codecFile)
      val file = buildPaths.bloopFile(projectName)
      (file, string)
    }

  def translateProject(
      pre: Prebootstrapped,
      resolver: CoursierResolver,
      crossName: model.CrossProjectName,
      explodedProject: model.Project,
      build: model.Build,
      getBloopProject: model.CrossProjectName => Config.File
  ): Config.File = {

    val projectPaths: ProjectPaths =
      pre.buildPaths.project(crossName, explodedProject)

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
      model.Replacements.paths(build = pre.buildPaths.buildDir) ++
        model.Replacements.projectPaths(project = projectPaths.dir) ++
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
              mode = Config.LinkerMode.Debug,
              kind = platform.jsKind.fold(Config.JsConfig.empty.kind)(conversions.moduleKindJS.from),
              emitSourceMaps = platform.jsEmitSourceMaps.getOrElse(Config.JsConfig.empty.emitSourceMaps),
              jsdom = platform.jsJsdom,
              output = None,
              nodePath = platform.jsNodeVersion.map(pre.fetchNode.apply),
              toolchain = Nil,
              moduleSplitStyle = platform.jsSplitStyle.map(conversions.moduleSplitStyleJS.from)
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
            runtimeConfig = Some(Config.JvmConfig(home = None, options = templateDirs.fill.opts(platform.jvmRuntimeOptions).render)).filter(_.options.nonEmpty),
            classpath = None,
            resources = None
          )
        case model.Platform.Native(platform @ _) =>
          val empty = Config.NativeConfig.empty
          val nativeModeAndLTO = Config.NativeModeAndLTO.empty.copy(
            nativeLinkerReleaseMode = platform.nativeLinkerReleaseMode.map(conversions.nativeLinkerReleaseMode.from),
            lto = platform.nativeLTO.map(conversions.nativeLTO.from)
          )
          val nativeFlags = Config.NativeFlags.empty.copy(
            multithreading = platform.nativeMultithreading,
            optimize = platform.nativeOptimize.getOrElse(true),
            useIncrementalCompilation = platform.nativeUseIncrementalCompilation.getOrElse(true),
            embedResources = platform.nativeEmbedResources.getOrElse(false)
          )
          Config.Platform.Native(
            config = Config.NativeConfig(
              version = require(platform.nativeVersion, "version").scalaNativeVersion,
              mode = Config.LinkerMode.Debug,
              gc = require(platform.nativeGc, "nativeGc"),
              targetTriple = empty.targetTriple,
              clang = empty.clang,
              clangpp = empty.clangpp,
              toolchain = empty.toolchain,
              options = empty.options,
              linkStubs = empty.linkStubs,
              check = empty.check,
              dump = empty.dump,
              output = empty.output,
              buildTarget = platform.nativeBuildTarget.map(conversions.nativeBuildTarget.from),
              nativeModeAndLTO = nativeModeAndLTO,
              nativeFlags = nativeFlags
            ),
            platform.mainClass
          )

        case other => sys.error(s"unexpected: $other")
      }

    val (resolvedDependencies, resolvedRuntimeDependencies) = {
      // SIP-51: For Scala 2.13/3, don't add scala-library as an explicit dependency.
      // Let it come from transitive dependencies so Coursier can upgrade it when needed.
      val fromPlatform = versionCombo match {
        case scala: model.VersionCombo.Scala if scala.scalaVersion.is3Or213 =>
          // For 2.13/3: Only include platform libraries OTHER than scala-library
          // (e.g., scala3-library, scalajs libraries, native libraries, test frameworks)
          versionCombo
            .libraries(isTest = explodedProject.isTestProject.getOrElse(false))
            .filterNot(dep => dep.organization == Organization("org.scala-lang") && dep.baseModuleName == ModuleName("scala-library"))
        case _ =>
          // For other Scala versions and Java: include all platform libraries as before
          versionCombo.libraries(isTest = explodedProject.isTestProject.getOrElse(false))
      }

      val inherited =
        build.transitiveDependenciesFor(crossName).flatMap { case (_, p) => p.dependencies.values }

      def providedOrOptional(dep: model.Dep): Boolean =
        dep.configuration == Configuration.provided || dep.configuration == Configuration.optional

      // drop provided/optional from inherited deps
      val filteredInherited = inherited.filterNot(providedOrOptional)

      val deps = explodedProject.dependencies.values ++ (filteredInherited ++ fromPlatform)
      val normal = resolver.force(
        deps,
        versionCombo,
        explodedProject.libraryVersionSchemes.values,
        crossName.value,
        explodedProject.ignoreEvictionErrors.getOrElse(model.IgnoreEvictionErrors.No)
      )

      val runtime =
        if (explodedProject.dependencies.values.exists(providedOrOptional) || inherited.size != filteredInherited.size) {
          // include optional and provided for deps for this project
          val (optionalsFromProject, restFromProject) =
            explodedProject.dependencies.values.partition(providedOrOptional)

          val noLongerOptionalsFromProject =
            optionalsFromProject.map(_.withConfiguration(Configuration.empty))

          val deps = (filteredInherited ++ restFromProject ++ noLongerOptionalsFromProject ++ fromPlatform).toSet
          resolver.force(
            deps,
            versionCombo,
            explodedProject.libraryVersionSchemes.values,
            crossName.value,
            explodedProject.ignoreEvictionErrors.getOrElse(model.IgnoreEvictionErrors.No)
          )
        } else normal

      (normal, runtime)
    }

    val resolution: Config.Resolution = {
      val modules: List[Config.Module] =
        resolvedDependencies.detailedArtifacts
          .groupByOrderedUnique { case (dep, _, _, _) => dep.module }
          .map { case (module, files) =>
            val (dep, _, _, _) = files.head

            Config.Module(
              organization = module.organization.value,
              name = module.name.value,
              version = dep.version,
              configurations = if (dep.configuration == Configuration.empty) None else Some(dep.configuration.value),
              artifacts = files
                .map { case (_, pub, _, file) =>
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
      model.JsonSet.fromIterable(
        allTransitiveTranslated.values.map(_.project.classesDir) ++ resolvedRuntimeDependencies.jars
      )

    val configuredScala: Option[Config.Scala] =
      scalaVersion.map { scalaVersion =>
        val compiler = scalaVersion.compiler.mapScala(_.copy(forceJvm = true)).asJava(versionCombo).getOrElse(sys.error("unexpected"))

        val resolvedScalaCompiler: List[Path] =
          resolver
            .force(Set(compiler), versionCombo = versionCombo, libraryVersionSchemes = SortedSet.empty, crossName.value, model.IgnoreEvictionErrors.No)
            .jars

        val setup = {
          // SIP-51 (Drop Forwards Binary Compatibility) for Scala 2.13 and 3:
          //
          // When scala-library is on the bootclasspath (addLibraryToBootClasspath=true) and
          // filtered from the regular classpath (filterLibraryFromClasspath=true), dependency
          // resolution cannot upgrade it. This breaks when dependencies compiled with newer
          // 2.13.x need methods not present in scala.version's 2.13.y (where y < x), causing
          // NoSuchMethodError at runtime.
          //
          // Solution: Place scala-library on the regular classpath where dependency resolution
          // can upgrade it to match what dependencies need. The compiler version stays at
          // scala.version, but the library version can float higher, implementing backwards-only
          // binary compatibility: code compiled with 2.13.x works with library 2.13.y where y >= x.
          //
          // This matches sbt's behavior (https://github.com/sbt/sbt/pull/7480):
          // - Use ClasspathOptionsUtil.noboot instead of .boot for 2.13/3
          // - This sets autoBoot=false (our addLibraryToBootClasspath=false)
          // - And filterLibrary=false (our filterLibraryFromClasspath=false)
          //
          // See: https://docs.scala-lang.org/sips/drop-stdlib-forwards-bin-compat.html
          def defaultSetup =
            if (scalaVersion.is3Or213)
              Defaults.DefaultCompileSetup.copy(
                addLibraryToBootClasspath = Some(false),
                filterLibraryFromClasspath = Some(false)
              )
            else
              Defaults.DefaultCompileSetup

          val provided = maybeScala.flatMap(_.setup).getOrElse(defaultSetup)
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
              .map(_.mapScala(_.copy(forceJvm = true)))

          model.Options.fromIterable(
            deps.toSeq.map { dep =>
              model.Options.Opt.Flag(
                s"${constants.ScalaPluginPrefix}:" +
                  resolver
                    .force(Set(dep), versionCombo, explodedProject.libraryVersionSchemes.values, crossName.value, model.IgnoreEvictionErrors.No)
                    .fullDetailedArtifacts
                    .collect { case (_, pub, _, Some(file)) if pub.classifier != Classifier.sources && pub.ext == Extension.jar => file.toPath }
                    .filterNot(resolvedScalaCompiler.toSet)
                    .mkString(File.pathSeparator)
              )
            }
          )
        }

        val scalacOptions: model.Options =
          maybeScala match {
            case Some(scala) =>
              val base = scala.options.union(compilerPlugins).union(versionCombo.compilerOptions)
              if (scala.strict.getOrElse(false)) {
                val tpolecat = new TpolecatPlugin(DevMode).scalacOptions(scalaVersion.scalaVersion)
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
          setup = Some(setup),
          bridgeJars = None
        )
      }

    // note: this matters for intellij when working with projects with maven layout (src/$scope/scala)
    // intellij will combine main and test bleep projects into one intellij project and use this tag to
    // determine main and test class path
    val tag =
      explodedProject.isTestProject.getOrElse(false) match {
        case true if crossName.name.value.endsWith("-it") => bloop.config.Tag.IntegrationTest
        case true                                         => bloop.config.Tag.Test
        case _                                            => bloop.config.Tag.Library
      }

    val annotationProcessingGenSourcesDir = explodedJava.flatMap(_.annotationProcessing) match {
      case Some(model.AnnotationProcessing(true)) =>
        Some(pre.buildPaths.generatedSourcesDir(crossName, "annotations"))
      case _ =>
        None
    }

    Config.File(
      "1.4.0",
      Config.Project(
        name = crossName.value,
        directory = projectPaths.targetDir,
        workspaceDir = Some(pre.buildPaths.buildDir),
        sources = projectPaths.sourcesDirs.all.toList ++ annotationProcessingGenSourcesDir.toList,
        sourcesGlobs = None,
        sourceRoots = None,
        dependencies = model.JsonSet.fromIterable(allTransitiveTranslated.keys.map(_.value)).values.toList,
        classpath = classPath.values.toList,
        out = projectPaths.targetDir,
        classesDir = projectPaths.classes,
        resources = Some(projectPaths.resourcesDirs.all.toList),
        scala = configuredScala,
        java = Some {
          val baseOptions = explodedJava.map(_.options).getOrElse(model.Options.empty)

          // Validate conflicting options
          explodedJava.flatMap(_.annotationProcessing).foreach { _ =>
            val renderedOptions = baseOptions.values.mkString(" ")
            if (renderedOptions.contains("-proc:") || renderedOptions.contains(" -s ")) {
              sys.error(s"Cannot use manual -proc or -s options when java.annotationProcessing is configured for project ${crossName.value}")
            }
          }

          val options = explodedJava.flatMap(_.annotationProcessing) match {
            case Some(model.AnnotationProcessing(true)) =>
              // Annotation processing enabled - add generated sources directory
              val genSourcesDir = pre.buildPaths.generatedSourcesDir(crossName, "annotations")
              baseOptions.union(model.Options.parse(List("-s", genSourcesDir.toString), maybeRelativize = None))
            case _ =>
              // Annotation processing disabled (default) - explicitly disable it
              baseOptions.union(model.Options.parse(List("-proc:none"), maybeRelativize = None))
          }
          Config.Java(options = templateDirs.fill.opts(options).render)
        },
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
        tags = Some(List(tag)),
        sourceGenerators = None
      )
    )
  }

  // https://stackoverflow.com/questions/9594431/scala-groupby-preserving-insertion-order/9608800#9608800
  implicit class GroupByOrderedImplicitImpl[A](val t: Iterable[A]) extends AnyVal {
    def groupByOrderedUnique[K](f: A => K): Map[K, ListSet[A]] =
      groupByGen(ListSet.newBuilder[A])(f)

    def groupByOrdered[K](f: A => K): Map[K, List[A]] =
      groupByGen(List.newBuilder[A])(f)

    def groupByGen[K, C[_]](makeBuilder: => mutable.Builder[A, C[A]])(f: A => K): Map[K, C[A]] = {
      val map = mutable.LinkedHashMap[K, mutable.Builder[A, C[A]]]()
      for (i <- t) {
        val key = f(i)
        val builder = map.get(key) match {
          case Some(existing) => existing
          case None =>
            val newBuilder = makeBuilder
            map(key) = newBuilder
            newBuilder
        }
        builder += i
      }
      map.map { case (k, v) => (k, v.result()) }.toMap
    }
  }
}
