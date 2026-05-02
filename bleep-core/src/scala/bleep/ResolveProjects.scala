package bleep

import bleep.internal.rewriteDependentData
import bleep.rewrites.Defaults
import coursier.Classifier
import coursier.core.{Configuration, Extension}
import org.typelevel.sbt.tpolecat.{DevMode, TpolecatPlugin}

import java.io.File
import java.nio.file.Path
import scala.collection.immutable.{ListSet, SortedMap, SortedSet}
import scala.collection.mutable

/** Resolves bleep projects to fully expanded ResolvedProject instances.
  *
  * This handles dependency resolution via Coursier and expands all template variables, compiler plugins, etc.
  *
  * Returns both the potentially-modified build and the resolved projects. Some implementations (like ReplaceBleepDependencies) modify the build model during
  * resolution (e.g., removing build.bleep:* dependencies).
  */
trait ResolveProjects {
  def apply(prebootstrapped: Prebootstrapped, resolver: CoursierResolver, build: model.Build): ResolveProjects.Result
}

object ResolveProjects {
  type Projects = SortedMap[model.CrossProjectName, Lazy[ResolvedProject]]

  case class Result(build: model.Build, projects: Projects, bspServerClasspathSource: bsp.BspServerClasspathSource)

  /** When `jarPath` declares JSR 269 annotation processors via ServiceLoader registration (i.e. has a `META-INF/services/javax.annotation.processing.Processor`
    * entry), returns the parsed list of fully-qualified class names. `None` means the jar has no service file. `Some(Nil)` means the file exists but is empty /
    * comment-only.
    */
  def annotationProcessorClasses(jarPath: Path): Option[List[String]] =
    if (!java.nio.file.Files.isRegularFile(jarPath)) None
    else {
      val zip = new java.util.zip.ZipFile(jarPath.toFile)
      try {
        val entry = zip.getEntry("META-INF/services/javax.annotation.processing.Processor")
        if (entry == null) None
        else {
          val src = scala.io.Source.fromInputStream(zip.getInputStream(entry), "UTF-8")
          try {
            val classes = src
              .getLines()
              .map(line => line.indexOf('#') match { case -1 => line; case i => line.substring(0, i) })
              .map(_.trim)
              .filter(_.nonEmpty)
              .toList
            Some(classes)
          } finally src.close()
        }
      } finally zip.close()
    }

  object InMemory extends ResolveProjects {
    override def apply(pre: Prebootstrapped, resolver: CoursierResolver, build: model.Build): Result = {
      val projects: Projects = rewriteDependentData(build.explodedProjects).apply[ResolvedProject] { (crossName, project, eval) =>
        def get(depName: model.CrossProjectName): ResolvedProject =
          eval(depName).forceGet(s"${crossName.value} => ${depName.value}")

        resolveProject(pre, resolver, crossName, project, build, getResolvedProject = get)
      }
      Result(build, projects, bspServerClasspathSource = bsp.BspServerClasspathSource.FromCoursier(resolver))
    }
  }

  /** For integration tests we want to write builds which depend on the version of bleep you're working on.
    */
  case class ReplaceBleepDependencies(bleepBuild: Lazy[Started], bspServerClasspathSource: bsp.BspServerClasspathSource) extends ResolveProjects {
    def isBleepDep(dep: model.Dep): Boolean = dep.organization.value == "build.bleep"

    override def apply(pre: Prebootstrapped, resolver: CoursierResolver, build: model.Build): Result = {
      val explodedBuild = build.dropBuildFile

      val b = mutable.Map.empty[model.CrossProjectName, Set[model.CrossProjectName]]

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

          val newDeps: SortedSet[model.Dep] =
            restDependencies ++ transitiveBleepProjectNames.view.flatMap(pn =>
              bleepBuild.forceGet.build.explodedProjects(pn).dependencies.values.filterNot(isBleepDep)
            )

          (crossName, p.copy(dependencies = model.JsonSet(newDeps)))
        }

        explodedBuild.copy(explodedProjects = newProjects)
      }

      // Verify that all required bleep project class directories exist.
      // They should already be compiled since bleep-tests depends on these projects.
      b.values.flatten.toList.distinct.foreach { crossName =>
        val classesDir = bleepBuild.forceGet.projectPaths(crossName).classes
        if (!java.nio.file.Files.isDirectory(classesDir))
          sys.error(s"Expected compiled classes at $classesDir for ${crossName.value}, but directory does not exist. Compile bleep first.")
      }

      val projects: Projects = rewriteDependentData(rewrittenBuild.explodedProjects).apply[ResolvedProject] { (crossName, project, eval) =>
        val resolved = resolveProject(
          pre,
          resolver,
          crossName,
          project,
          build,
          getResolvedProject = depName => eval(depName).forceGet(s"${crossName.value} => ${depName.value}")
        )

        val newClassPath = b.getOrElse(crossName, Nil).toList.map(bleepBuild.forceGet.projectPaths).map(_.classes)
        resolved.copy(classpath = newClassPath ++ resolved.classpath)
      }

      Result(rewrittenBuild, projects, bspServerClasspathSource)
    }
  }

  def resolveProject(
      pre: Prebootstrapped,
      resolver: CoursierResolver,
      crossName: model.CrossProjectName,
      explodedProject: model.Project,
      build: model.Build,
      getResolvedProject: model.CrossProjectName => ResolvedProject
  ): ResolvedProject = {

    val projectPaths: ProjectPaths =
      pre.buildPaths.project(crossName, explodedProject)

    val allTransitiveResolved: Map[model.CrossProjectName, ResolvedProject] = {
      val builder = Map.newBuilder[model.CrossProjectName, ResolvedProject]

      def go(cn: model.CrossProjectName): Unit = {
        val p = getResolvedProject(cn)
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
        model.Replacements.versions(Some(build.$version), versionCombo, includeEpoch = true, includeBinVersion = true, buildDir = Some(pre.buildPaths.buildDir))

    def require[T](ot: Option[T], name: String): T =
      ot.toRight(s"missing platform field `$name`").orThrowText

    val resolvedPlatform: Option[ResolvedProject.Platform] =
      explodedPlatform.map {
        case model.Platform.Js(platform) =>
          ResolvedProject.Platform.Js(
            version = require(platform.jsVersion, "version").scalaJsVersion,
            mode = "debug",
            kind = platform.jsKind.fold("NoModule")(_.toString),
            emitSourceMaps = platform.jsEmitSourceMaps.getOrElse(true),
            jsdom = platform.jsJsdom,
            nodePath = platform.jsNodeVersion.map(pre.fetchNode.apply),
            mainClass = platform.mainClass
          )
        case model.Platform.Jvm(platform) =>
          val agentOptions = platform.jvmAgents.values.toList.map { agentDep =>
            val resolved = resolver.force(
              Set(agentDep),
              versionCombo,
              libraryVersionSchemes = SortedSet.empty,
              s"${crossName.value}/jvmAgent",
              model.IgnoreEvictionErrors.No
            )
            val jar = resolved.jars.headOption.getOrElse(
              sys.error(s"Could not resolve jvmAgent: ${agentDep.organization.value}:${agentDep.baseModuleName.value}:${agentDep.version}")
            )
            s"-javaagent:$jar"
          }
          ResolvedProject.Platform.Jvm(
            options = templateDirs.fill.opts(platform.jvmOptions).render ++ agentOptions,
            mainClass = platform.mainClass,
            runtimeOptions = templateDirs.fill.opts(platform.jvmRuntimeOptions).render
          )
        case model.Platform.Native(platform) =>
          ResolvedProject.Platform.Native(
            version = require(platform.nativeVersion, "version").scalaNativeVersion,
            mode = "debug",
            gc = require(platform.nativeGc, "nativeGc"),
            mainClass = platform.mainClass
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

    val resolution: ResolvedProject.Resolution = {
      val modules: List[ResolvedProject.ResolvedModule] =
        resolvedDependencies.detailedArtifacts
          .groupByOrderedUnique { case (dep, _, _, _) => dep.module }
          .map { case (module, files) =>
            val (dep, _, _, _) = files.head

            ResolvedProject.ResolvedModule(
              organization = module.organization.value,
              name = module.name.value,
              version = dep.version,
              artifacts = files
                .map { case (_, pub, _, file) =>
                  ResolvedProject.ResolvedArtifact(
                    dep.module.name.value,
                    if (pub.classifier == Classifier.empty) None else Some(pub.classifier.value),
                    file.toPath
                  )
                }
                .toList
                .distinct
            )
          }
          .toList

      ResolvedProject.Resolution(modules)
    }

    val unmanagedJars: List[Path] =
      explodedProject.jars.values.map(rp => pre.buildPaths.buildDir.resolve(rp.toString)).toList

    val classPath: model.JsonSet[Path] =
      model.JsonSet.fromIterable(
        allTransitiveResolved.values.flatMap(x => x.classesDir :: x.resources.getOrElse(Nil)) ++ resolvedRuntimeDependencies.jars ++ unmanagedJars
      )

    // Resolve annotation processor wiring. Three input signals on `model.Java`:
    //   - scanForAnnotationProcessors: opt-in to scan resolved-deps for processor jars
    //   - annotationProcessors: explicit processor-only deps (separate from runtime classpath)
    //   - annotationProcessorOptions: -A<k>=<v> flags
    // Plus an escape hatch: if `-proc:none` already appears in `java.options`, leave it alone.
    val (annotationProcessingGenSourcesDir: Option[Path], annotationProcessorFlags: List[String]) = {
      val baseOpts = explodedJava.map(_.options).getOrElse(model.Options.empty)
      val rendered = baseOpts.values.mkString(" ")
      val userHasProcNone = baseOpts.values.exists(_.render.contains("-proc:none"))

      val javaCfg = explodedJava
      val wantsScan = javaCfg.flatMap(_.scanForAnnotationProcessors).contains(true)
      val explicitDeps: Set[model.Dep] =
        javaCfg.fold(Set.empty[model.Dep])(_.annotationProcessors.values.toSet)

      if (userHasProcNone) (None, Nil)
      else if (!wantsScan && explicitDeps.isEmpty) (None, List("-proc:none"))
      else {
        val explicitJars: List[Path] = explicitDeps.toList.flatMap { dep =>
          val mapped = dep.mapScala(_.copy(forceJvm = true))
          resolver
            .force(
              Set(mapped),
              versionCombo,
              explodedProject.libraryVersionSchemes.values,
              s"${crossName.value}/annotationProcessor",
              model.IgnoreEvictionErrors.No
            )
            .jars
        }

        val scannedJars: List[Path] =
          if (!wantsScan) Nil
          else
            resolvedDependencies.jars.flatMap { jarPath =>
              ResolveProjects.annotationProcessorClasses(jarPath).filter(_.nonEmpty) match {
                case Some(classes) =>
                  pre.logger.info(s"[${crossName.value}] auto-discovered annotation processor JAR: $jarPath — classes: ${classes.mkString(", ")}")
                  Some(jarPath)
                case None => None
              }
            }

        if (wantsScan && explicitJars.isEmpty && scannedJars.isEmpty) {
          sys.error(
            s"project ${crossName.value}: scanForAnnotationProcessors: true was set but no annotation processor JARs were found in dependencies and annotationProcessors is empty"
          )
        }

        val processorJars: List[Path] = (scannedJars ++ explicitJars).distinct

        if (rendered.contains("-proc:") || rendered.contains(" -s "))
          sys.error(
            s"project ${crossName.value}: cannot use manual -proc:* or -s in java.options when annotation processing is configured (set scanForAnnotationProcessors / annotationProcessors instead)"
          )
        if (rendered.contains("-processorpath"))
          sys.error(
            s"project ${crossName.value}: cannot use manual -processorpath in java.options when annotation processing is configured"
          )
        if (baseOpts.values.exists(_.render.exists(_.startsWith("-A"))))
          sys.error(
            s"project ${crossName.value}: cannot use manual -A flags in java.options when annotation processing is configured (use annotationProcessorOptions)"
          )

        val genDir = pre.buildPaths.generatedSourcesDir(crossName, "annotations")
        val aFlags: List[String] =
          javaCfg
            .map(_.annotationProcessorOptions.value)
            .getOrElse(SortedMap.empty[String, String])
            .iterator
            .map { case (k, v) => s"-A$k=$v" }
            .toList

        val flags = List("-processorpath", processorJars.mkString(File.pathSeparator), "-s", genDir.toString) ++ aFlags
        (Some(genDir), flags)
      }
    }

    // Compute Java options (used both for pure Java and mixed Scala/Java projects).
    // Render the user's options through templateDirs first, then APPEND the annotation
    // processor flags raw. We can't roundtrip the AP flags through Options.parse because
    // Options.parse whitespace-splits each token, and `-processorpath` / `-s` arguments
    // are paths that legitimately contain spaces (e.g. macOS test temp dirs).
    val resolvedJavaOptions: List[String] = {
      val baseOptions = explodedJava.map(_.options).getOrElse(model.Options.empty)
      templateDirs.fill.opts(baseOptions).render ++ annotationProcessorFlags
    }

    // Determine the language: Scala (with Java options) or pure Java
    val language: ResolvedProject.Language = scalaVersion match {
      case Some(scalaVersion) =>
        val compiler = scalaVersion.compiler.mapScala(_.copy(forceJvm = true)).asJava(versionCombo).getOrElse(sys.error("unexpected"))

        val resolvedScalaCompiler: List[Path] = {
          val defaultCompilerJars =
            resolver
              .force(Set(compiler), versionCombo = versionCombo, libraryVersionSchemes = SortedSet.empty, crossName.value, model.IgnoreEvictionErrors.No)
              .jars

          // SIP-51: For Scala 3, update scala-library.jar in the compiler's runtime classpath
          // to match the version resolved from project dependencies. This prevents
          // NoSuchMethodException during macro expansion when macros were compiled against
          // a newer scala-library than what the compiler is using.
          if (!scalaVersion.is3)
            defaultCompilerJars
          else {
            val resolvedScalaLibrary =
              resolvedDependencies.fullDetailedArtifacts.collectFirst {
                case (dep, _, _, Some(file))
                    if dep.module.organization == coursier.core.Organization("org.scala-lang") &&
                      dep.module.name == coursier.core.ModuleName("scala-library") &&
                      file.getName.endsWith(".jar") =>
                  file.toPath
              }

            resolvedScalaLibrary match {
              case None                          => defaultCompilerJars
              case Some(resolvedScalaLibraryJar) =>
                // Replace scala-library.jar in compiler jars with the version from dependencies
                defaultCompilerJars
                  .map { jar =>
                    val filename = jar.getFileName.toString
                    if (filename.startsWith("scala-library") && filename.endsWith(".jar"))
                      resolvedScalaLibraryJar
                    else
                      jar
                  }
            }
          }
        }

        val setup = {
          val provided = maybeScala.flatMap(_.setup).getOrElse(Defaults.DefaultCompileSetup)

          // SIP-51 (Drop Forwards Binary Compatibility) for Scala 2.13 and 3:
          //
          // When scala-library is on the bootclasspath (addLibraryToBootClasspath=true) and
          // filtered from the regular classpath (filterLibraryFromClasspath=true), dependency
          // resolution cannot upgrade it. This breaks when dependencies compiled with newer
          // 2.13.x need methods not present in scala.version's 2.13.y (where y < x).
          //
          // Solution: Place scala-library on the regular classpath where dependency resolution
          // can upgrade it to match what dependencies need.
          //
          // Matches sbt's behavior (https://github.com/sbt/sbt/pull/7480):
          // - Use ClasspathOptionsUtil.noboot instead of .boot for 2.13/3
          val addLibraryToBootClasspath =
            if (scalaVersion.is3Or213) false
            else provided.addLibraryToBootClasspath.get

          val filterLibraryFromClasspath =
            if (scalaVersion.is3Or213) false
            else provided.filterLibraryFromClasspath.get

          ResolvedProject.CompileSetup(
            order = provided.order.get,
            addLibraryToBootClasspath = addLibraryToBootClasspath,
            addCompilerToClasspath = provided.addCompilerToClasspath.get,
            addExtraJarsToClasspath = provided.addExtraJarsToClasspath.get,
            manageBootClasspath = provided.manageBootClasspath.get,
            filterLibraryFromClasspath = filterLibraryFromClasspath
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

        ResolvedProject.Language.Scala(
          organization = compiler.organization.value,
          name = compiler.moduleName.value,
          version = compiler.version,
          options = templateDirs.fill.opts(scalacOptions).render,
          compilerJars = resolvedScalaCompiler,
          analysisFile = Some(projectPaths.incrementalAnalysis),
          setup = Some(setup),
          javaOptions = resolvedJavaOptions
        )

      case None =>
        // Pure Java project
        ResolvedProject.Language.Java(
          options = resolvedJavaOptions
        )
    }

    val isTest = explodedProject.isTestProject.getOrElse(false)

    ResolvedProject(
      name = crossName.value,
      directory = projectPaths.targetDir,
      workspaceDir = pre.buildPaths.buildDir,
      sources = projectPaths.sourcesDirs.all.toList ++ annotationProcessingGenSourcesDir.toList,
      classpath = classPath.values.toList,
      classesDir = projectPaths.classes,
      resources = Some(projectPaths.resourcesDirs.all.toList),
      language = language,
      platform = resolvedPlatform,
      isTestProject = isTest,
      dependencies = model.JsonSet.fromIterable(allTransitiveResolved.keys.map(_.value)).values.toList,
      testFrameworks = explodedProject.testFrameworks.values.toList.map(_.value),
      resolution = Some(resolution)
    )
  }

  // Helper for groupBy preserving order
  implicit class GroupByOrderedImplicitImpl[A](val t: Iterable[A]) extends AnyVal {
    def groupByOrderedUnique[K](f: A => K): Map[K, ListSet[A]] =
      groupByGen(ListSet.newBuilder[A])(f)

    def groupByGen[K, C[_]](makeBuilder: => mutable.Builder[A, C[A]])(f: A => K): Map[K, C[A]] = {
      val map = mutable.LinkedHashMap[K, mutable.Builder[A, C[A]]]()
      for (i <- t) {
        val key = f(i)
        val builder = map.get(key) match {
          case Some(existing) => existing
          case None           =>
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
