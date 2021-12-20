package bleep

import bleep.internal.{rewriteDependentData, Lazy, ScalaVersions}
import bloop.config.{Config => b}
import coursier.core.Configuration
import coursier.{Classifier, Dependency}

import java.nio.file.Path
import scala.collection.immutable.SortedMap

object generateBloopFiles {
  implicit val ordering: Ordering[Dependency] =
    Ordering.by(_.toString())

  def apply(build: model.Build, buildPaths: BuildPaths, resolver: CoursierResolver): SortedMap[model.ProjectName, Lazy[b.File]] = {
    verify(build)

    rewriteDependentData(build.projects) { (projectName, project, getDep) =>
      translateProject(
        resolver,
        buildPaths,
        projectName,
        project,
        build,
        getBloopProject = dep => getDep(dep).forceGet(s"${projectName.value} => ${dep.value}")
      )
    }
  }

  def verify(build: model.Build): Unit =
    build.scripts match {
      case None => ()
      case Some(scripts) =>
        scripts.foreach { case (scriptName, scriptDefs) =>
          scriptDefs.values.foreach { scriptDef =>
            if (build.projects.contains(scriptDef.project)) ()
            else sys.error(s"script ${scriptName.value} references non-existing project ${scriptDef.project.value}")
          }
        }
    }

  def translateProject(
      resolver: CoursierResolver,
      buildPaths: BuildPaths,
      projName: model.ProjectName,
      proj2: model.Project,
      build: model.Build,
      getBloopProject: model.ProjectName => b.File
  ): b.File = {
    val proj = build.explode(proj2)

    val projectPaths: ProjectPaths =
      buildPaths.from(projName, proj)

    val allTransitiveTranslated: Map[model.ProjectName, b.File] = {
      val builder = Map.newBuilder[model.ProjectName, b.File]

      def go(n: model.ProjectName): Unit = {
        val p = getBloopProject(n)
        if (n == projName) sys.error(s"project ${projName.value} transitively depends on itself")
        builder += ((n, p))
        p.project.dependencies.foreach(projectName => go(model.ProjectName(projectName)))
      }

      proj.dependsOn.values.foreach(go)

      builder.result()
    }

    val templateDirs = Options.TemplateDirs(buildPaths.buildDir, projectPaths.dir)

    val maybeScala: Option[model.Scala] =
      proj.scala

    val explodedJava: Option[model.Java] =
      proj.java

    val scalaVersion: Option[Versions.Scala] =
      maybeScala.flatMap(_.version)

    val explodedPlatform: Option[model.Platform] =
      proj.platform.map {
        case x: model.Platform.Jvm => x.unionJvm(Defaults.Jvm)
        case x                     => x
      }

    val versions: ScalaVersions =
      ScalaVersions.fromExplodedProject(proj) match {
        case Left(err)       => throw new BuildException.Text(projName, err)
        case Right(versions) => versions
      }

    val configuredPlatform: Option[b.Platform] =
      explodedPlatform.map {
        case model.Platform.Js(version, mode, kind, emitSourceMaps, jsdom, mainClass) =>
          b.Platform.Js(
            b.JsConfig(
              version = version match {
                case Some(value)                             => value.scalaJsVersion
                case None if scalaVersion.fold(false)(_.is3) => ""
                case None                                    => sys.error("missing `version`")
              },
              mode = mode.getOrElse(b.JsConfig.empty.mode),
              kind = kind.getOrElse(b.JsConfig.empty.kind),
              emitSourceMaps = emitSourceMaps.getOrElse(b.JsConfig.empty.emitSourceMaps),
              jsdom = jsdom,
              output = None,
              nodePath = None,
              toolchain = Nil
            ),
            mainClass
          )
        case model.Platform.Jvm(options, mainClass, runtimeOptions) =>
          b.Platform.Jvm(
            config = b.JvmConfig(
              home = None,
              options = templateDirs.toAbsolutePaths.opts(options).render
            ),
            mainClass = mainClass,
            runtimeConfig = Some(b.JvmConfig(home = None, options = templateDirs.toAbsolutePaths.opts(runtimeOptions).render)),
            classpath = None,
            resources = None
          )
        case model.Platform.Native(version, mode, gc, mainClass) => ???
      }

    val resolvedDependencies: CoursierResolver.Result = {
      val transitiveDeps: JsonSet[Dep] =
        proj.dependencies.union(JsonSet.fromIterable(build.transitiveDependenciesFor(projName).flatMap { case (_, p) => p.dependencies.values }))

      val concreteDeps: JsonSet[Dependency] =
        transitiveDeps.map { dep =>
          dep.dependency(versions) match {
            case Left(err)    => throw new BuildException.Text(projName, err)
            case Right(value) => value
          }
        }

      resolver(concreteDeps, build.resolvers) match {
        case Left(coursierError) => throw BuildException.ResolveError(coursierError, projName)
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
            case Left(coursierError) => throw BuildException.ResolveError(coursierError, projName)
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
              case Left(coursierError) => throw BuildException.ResolveError(coursierError, projName)
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
          options = templateDirs.toAbsolutePaths.opts(scalacOptions).render,
          jars = resolvedScalaCompiler,
          analysis = Some(projectPaths.incrementalAnalysis(scalaVersion)),
          setup = Some(setup)
        )
      }

    val scope = proj.`sbt-scope`.getOrElse("main")

    def sourceLayout = proj.`source-layout` match {
      case Some(sourceLayout) => sourceLayout
      case None               => if (scalaVersion.isDefined) SourceLayout.Normal else SourceLayout.Java
    }

    val sources: JsonSet[Path] =
      (sourceLayout.sources(scalaVersion, proj.`sbt-scope`) ++ JsonSet.fromIterable(proj.sources.values)).map(projectPaths.dir / _)

    val resources: JsonSet[Path] =
      (sourceLayout.resources(scalaVersion, proj.`sbt-scope`) ++ JsonSet.fromIterable(proj.resources.values)).map(projectPaths.dir / _)

    b.File(
      "1.4.0",
      b.Project(
        projName.value,
        projectPaths.dir,
        Some(buildPaths.buildDir),
        sources = sources.values.toList,
        sourcesGlobs = None,
        sourceRoots = None,
        dependencies = JsonSet.fromIterable(allTransitiveTranslated.keys.map(_.value)).values.toList,
        classpath = classPath.values.toList,
        out = projectPaths.targetDir,
        classesDir = projectPaths.classes(scalaVersion, isTest = !proj.testFrameworks.isEmpty),
        resources = Some(resources.values.toList),
        scala = configuredScala,
        java = Some(b.Java(options = templateDirs.toAbsolutePaths.opts(explodedJava.map(_.options).getOrElse(Options.empty)).render)),
        sbt = None,
        test = if (scope == "test") Some(b.Test.defaultConfiguration) else None,
        platform = configuredPlatform,
        resolution = Some(resolution),
        tags = None
      )
    )
  }
}
