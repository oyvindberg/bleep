package bleep

import bleep.internal.{rewriteDependentData, Lazy, Replacements, ScalaVersions}
import bloop.config.{Config => b}
import coursier.core.Configuration
import coursier.{Classifier, Dependency}

import java.nio.file.Path
import scala.collection.immutable.SortedMap

object generateBloopFiles {
  implicit val ordering: Ordering[Dependency] =
    Ordering.by(_.toString())

  def apply(build: ExplodedBuild, buildPaths: BuildPaths, resolver: CoursierResolver): SortedMap[model.CrossProjectName, Lazy[b.File]] =
    rewriteDependentData(build.projects) { (crossName, project, getDep) =>
      translateProject(
        resolver,
        buildPaths,
        crossName,
        project,
        build,
        getBloopProject = depName => getDep(depName).forceGet(s"${crossName.value} => ${depName.value}")
      )
    }

  def translateProject(
      resolver: CoursierResolver,
      buildPaths: BuildPaths,
      crossName: model.CrossProjectName,
      explodedProject: model.Project,
      build: ExplodedBuild,
      getBloopProject: model.CrossProjectName => b.File
  ): b.File = {

    val projectPaths: ProjectPaths =
      buildPaths.from(crossName.name, explodedProject)

    val allTransitiveTranslated: Map[model.CrossProjectName, b.File] = {
      val builder = Map.newBuilder[model.CrossProjectName, b.File]

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
        Replacements.versions(maybeScala.flatMap(_.version), explodedPlatform.flatMap(_.name).map(_.value))

    val configuredPlatform: Option[b.Platform] =
      explodedPlatform.map {
        case model.Platform.Js(platform) =>
          b.Platform.Js(
            b.JsConfig(
              version = platform.jsVersion match {
                case Some(value)                             => value.scalaJsVersion
                case None if scalaVersion.fold(false)(_.is3) => ""
                case None                                    => sys.error("missing `version`")
              },
              mode = platform.jsMode.getOrElse(b.JsConfig.empty.mode),
              kind = platform.jsKind.getOrElse(b.JsConfig.empty.kind),
              emitSourceMaps = platform.jsEmitSourceMaps.getOrElse(b.JsConfig.empty.emitSourceMaps),
              jsdom = platform.jsJsdom,
              output = None,
              nodePath = None,
              toolchain = Nil
            ),
            platform.jsMainClass
          )
        case model.Platform.Jvm(platform) =>
          b.Platform.Jvm(
            config = b.JvmConfig(
              home = None,
              options = templateDirs.fill.opts(platform.jvmOptions).render
            ),
            mainClass = platform.jvmMainClass,
            runtimeConfig = Some(b.JvmConfig(home = None, options = templateDirs.fill.opts(platform.jvmRuntimeOptions).render)),
            classpath = None,
            resources = None
          )
        case model.Platform.Native(platform) => ???
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

      resolver(concreteDeps, build.resolvers) match {
        case Left(coursierError) => throw BuildException.ResolveError(coursierError, crossName)
        case Right(value)        => value
      }
    }

    val resolution: b.Resolution = {
      val modules: List[b.Module] =
        resolvedDependencies.detailedArtifacts
          .groupBy { case (dep, _, _, _) => dep.module }
          .map { case (module, files) =>
            val (dep, _, _, _) = files.head

            b.Module(
              organization = module.organization.value,
              name = module.name.value,
              version = dep.version,
              configurations = if (dep.configuration == Configuration.empty) None else Some(dep.configuration.value),
              artifacts = files.collect { case (_, pub, _, file) =>
                b.Artifact(
                  dep.module.name.value,
                  if (pub.classifier == Classifier.empty) None else Some(pub.classifier.value),
                  checksum = None,
                  file.toPath
                )
              }.toList
            )
          }
          .toList

      b.Resolution(modules)
    }

    val classPath: JsonSet[Path] =
      JsonSet.fromIterable(allTransitiveTranslated.values.map(_.project.classesDir) ++ resolvedDependencies.jars)

    val configuredScala: Option[b.Scala] =
      scalaVersion.map { scalaVersion =>
        val scalaCompiler: Dependency =
          scalaVersion.compiler.forceDependency(ScalaVersions.Jvm(scalaVersion))

        val resolvedScalaCompiler: List[Path] =
          resolver(JsonSet(scalaCompiler), build.resolvers) match {
            case Left(coursierError) => throw BuildException.ResolveError(coursierError, crossName)
            case Right(res)          => res.jars
          }

        val setup = {
          val provided = maybeScala.flatMap(_.setup).map(_.union(Defaults.DefaultCompileSetup)).getOrElse(Defaults.DefaultCompileSetup)
          b.CompileSetup(
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

          val artifacts: List[Path] =
            resolver(deps, build.resolvers) match {
              case Left(coursierError) => throw BuildException.ResolveError(coursierError, crossName)
              case Right(res)          => res.jars
            }

          val relevantArtifacts: List[Path] =
            artifacts.filterNot(p => p.endsWith(".jar") && !p.toString.contains("-sources") && !p.toString.contains("-javadoc"))

          Options.fromIterable(relevantArtifacts.map(p => Options.Opt.Flag(s"${Defaults.ScalaPluginPrefix}$p")))
        }

        val scalacOptions: Options =
          maybeScala.fold(Options.empty)(_.options).union(compilerPlugins)

        b.Scala(
          organization = scalaCompiler.module.organization.value,
          name = scalaCompiler.module.name.value,
          version = scalaCompiler.version,
          options = templateDirs.fill.opts(scalacOptions).render,
          jars = resolvedScalaCompiler,
          analysis = Some(projectPaths.incrementalAnalysis(scalaVersion)),
          setup = Some(setup)
        )
      }

    val scope = explodedProject.`sbt-scope`.getOrElse("main")

    def sourceLayout = explodedProject.`source-layout` match {
      case Some(sourceLayout) => sourceLayout
      case None               => if (scalaVersion.isDefined) SourceLayout.Normal else SourceLayout.Java
    }
    val replacementsVersions = Replacements.versions(scalaVersion, configuredPlatform.map(_.name))

    val sources: JsonSet[Path] = {
      val fromSourceLayout = sourceLayout.sources(scalaVersion, explodedProject.`sbt-scope`)
      val fromJson = JsonSet.fromIterable(explodedProject.sources.values.map(replacementsVersions.fill.relPath))
      (fromSourceLayout ++ fromJson).map(projectPaths.dir / _)
    }

    val resources: JsonSet[Path] = {
      val fromJson = JsonSet.fromIterable(explodedProject.resources.values.map(replacementsVersions.fill.relPath))
      val fromSourceLayout = sourceLayout.resources(scalaVersion, explodedProject.`sbt-scope`)
      (fromSourceLayout ++ fromJson).map(projectPaths.dir / _)
    }

    b.File(
      "1.4.0",
      b.Project(
        crossName.value,
        projectPaths.dir,
        Some(buildPaths.buildDir),
        sources = sources.values.toList,
        sourcesGlobs = None,
        sourceRoots = None,
        dependencies = JsonSet.fromIterable(allTransitiveTranslated.keys.map(_.value)).values.toList,
        classpath = classPath.values.toList,
        out = projectPaths.targetDir,
        classesDir = projectPaths.classes(crossName.crossId, isTest = !explodedProject.testFrameworks.isEmpty),
        resources = Some(resources.values.toList),
        scala = configuredScala,
        java = Some(b.Java(options = templateDirs.fill.opts(explodedJava.map(_.options).getOrElse(Options.empty)).render)),
        sbt = None,
        test = if (scope == "test") Some(b.Test.defaultConfiguration) else None,
        platform = configuredPlatform,
        resolution = Some(resolution),
        tags = None
      )
    )
  }
}
