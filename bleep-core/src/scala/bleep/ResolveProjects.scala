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

    val annotationProcessingGenSourcesDir = explodedJava.flatMap(_.annotationProcessing) match {
      case Some(model.AnnotationProcessing(true)) =>
        Some(pre.buildPaths.generatedSourcesDir(crossName, "annotations"))
      case _ =>
        None
    }

    // Compute Java options (used both for pure Java and mixed Scala/Java projects)
    val resolvedJavaOptions: List[String] = {
      val baseOptions = explodedJava.map(_.options).getOrElse(model.Options.empty)

      explodedJava.flatMap(_.annotationProcessing).foreach { _ =>
        val renderedOptions = baseOptions.values.mkString(" ")
        if (renderedOptions.contains("-proc:") || renderedOptions.contains(" -s ")) {
          sys.error(s"Cannot use manual -proc or -s options when java.annotationProcessing is configured for project ${crossName.value}")
        }
      }

      val options = explodedJava.flatMap(_.annotationProcessing) match {
        case Some(model.AnnotationProcessing(true)) =>
          val genSourcesDir = pre.buildPaths.generatedSourcesDir(crossName, "annotations")
          baseOptions.union(model.Options.parse(List("-s", genSourcesDir.toString), maybeRelativize = None))
        case _ =>
          baseOptions.union(model.Options.parse(List("-proc:none"), maybeRelativize = None))
      }

      templateDirs.fill.opts(options).render
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
