package bleep

import bleep.internal.Lazy
import bleep.model.ProjectName
import bloop.config.{PlatformFiles, Config => b}
import coursier.core.Configuration
import coursier.{Classifier, Dependency}

import java.nio.file.Path
import scala.concurrent.Await
import scala.concurrent.duration.Duration

object generateBloopFiles {
  def apply(file: model.File, workspaceDir: Path, resolver: CoursierResolver): List[b.File] = {
    lazy val resolvedProjects: Map[ProjectName, Lazy[b.File]] =
      file.projects.map { case (projectName, project) =>
        projectName -> Lazy(
          translateProject(
            resolver,
            workspaceDir,
            projectName,
            project,
            file,
            None,
            name => file.projects.getOrElse(name, sys.error(s"Project ${projectName.value} depends on non-existing project ${name.value}")),
            name =>
              resolvedProjects
                .getOrElse(name, sys.error(s"Project ${projectName.value} depends on non-existing project ${name.value}"))
                .get
                .getOrElse(sys.error(s"Circular dependency detected at ${projectName.value}"))
          )
        )
      }

    resolvedProjects.map { case (projectName, resolvedProject) =>
      resolvedProject.get.getOrElse(sys.error(s"Circular dependency detected at ${projectName.value}"))
    }.toList

  }

  def translateProject(
      resolver: CoursierResolver,
      workspaceDir: Path,
      name: model.ProjectName,
      p: model.Project,
      file: model.File,
      scalaJsVersion: Option[Versions.ScalaJs],
      getLocalProject: model.ProjectName => model.Project,
      getBloopProject: model.ProjectName => b.File
  ): b.File = {
    val projectFolder: Path =
      p.folder match {
        case Some(relPath) => workspaceDir / relPath
        case None          => workspaceDir / name.value
      }

    val allTransitiveLocal: Map[ProjectName, model.Project] = {
      val foo = Map.newBuilder[model.ProjectName, model.Project]

      def go(n: model.ProjectName, p: model.Project): Unit = {
        if (n == name) sys.error(s"project ${name.value} transitively depends on itself")
        foo += ((n, p))
        p.dependsOn.flat.foreach(projectName => go(projectName, getLocalProject(projectName)))
      }

      p.dependsOn.flat.foreach(projectName => go(projectName, getLocalProject(projectName)))

      foo.result()
    }

    val allTransitiveBloop: Map[ProjectName, b.File] = {
      val foo = Map.newBuilder[model.ProjectName, b.File]

      def go(n: model.ProjectName, p: b.File): Unit = {
        if (n == name) sys.error(s"project ${name.value} transitively depends on itself")
        foo += ((n, p))
        p.project.dependencies.foreach(projectName => go(model.ProjectName(projectName), getBloopProject(model.ProjectName(projectName))))
      }

      p.dependsOn.flat.foreach(projectName => go(projectName, getBloopProject(projectName)))

      foo.result()
    }

    val mergedScala: Option[model.Scala] =
      model.Scala.merge(p.scala, file.scala)

    val mergedJava: Option[model.Java] =
      model.Java.merge(p.java, file.java)

    val scalaVersion: Versions.Scala =
      mergedScala.flatMap(_.version).getOrElse(sys.error(s"no scalaVersion provided for project ${name.value}"))

    val scalacOptions: List[String] =
      mergedScala.flatMap(_.options).flat

    val resolution: b.Resolution = {
      val transitiveDeps: Seq[Dep] =
        p.dependencies.flat ++ allTransitiveLocal.flatMap { case (_, p) => p.dependencies.flat }

      val coursierDeps: Seq[Dependency] =
        transitiveDeps.map(dep => dep.concrete(scalaVersion, scalaJsVersion).asCoursier)

      val result = Await.result(resolver(coursierDeps), Duration.Inf)

      val modules: List[b.Module] =
        result.fullDetailedArtifacts
          .groupBy { case (dep, _, _, _) => dep.module }
          .map { case (module, files) =>
            val (dep, _, _, _) = files.head

            b.Module(
              organization = module.organization.value,
              name = module.name.value,
              version = dep.version,
              configurations = if (dep.configuration == Configuration.empty) None else Some(dep.configuration.value),
              artifacts = files.collect { case (_, pub, _, Some(file)) =>
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

    val scalaCompiler: Dep.Concrete =
      scalaVersion.compiler.concrete(scalaVersion, scalaJsVersion)

    val resolvedScalaCompiler: List[java.nio.file.Path] =
      Await.result(resolver(List(scalaCompiler.asCoursier)), Duration.Inf).files.toList.map(_.toPath)

    val classPath: List[PlatformFiles.Path] = {
      allTransitiveBloop.values.map(_.project.classesDir).toList ++
        resolution.modules.flatMap(_.artifacts).map(_.path)
    }

    val setup = {
      val provided = mergedScala.flatMap(_.setup)
      b.CompileSetup(
        order = provided.flatMap(_.order) match {
          case Some(model.CompileOrder.JavaThenScala) => b.JavaThenScala
          case Some(model.CompileOrder.ScalaThenJava) => b.ScalaThenJava
          case Some(model.CompileOrder.Mixed)         => b.Mixed
          case None                                   => b.CompileSetup.empty.order
        },
        addLibraryToBootClasspath = provided.flatMap(_.addLibraryToBootClasspath).getOrElse(b.CompileSetup.empty.addLibraryToBootClasspath),
        addCompilerToClasspath = provided.flatMap(_.addCompilerToClasspath).getOrElse(b.CompileSetup.empty.addCompilerToClasspath),
        addExtraJarsToClasspath = provided.flatMap(_.addExtraJarsToClasspath).getOrElse(b.CompileSetup.empty.addExtraJarsToClasspath),
        manageBootClasspath = provided.flatMap(_.manageBootClasspath).getOrElse(b.CompileSetup.empty.manageBootClasspath),
        filterLibraryFromClasspath = provided.flatMap(_.filterLibraryFromClasspath).getOrElse(b.CompileSetup.empty.filterLibraryFromClasspath)
      )
    }

    val isTest = name.value.endsWith("-test")
    val scope = if (isTest) "test" else "main"

    b.File(
      "1.4.0",
      b.Project(
        name.value,
        projectFolder,
        Some(workspaceDir),
        p.sources match {
          case Some(providedSources) => providedSources.values.map(relPath => projectFolder / relPath)
          case None                  => Defaults.sourceDirs(projectFolder, scalaVersion, scope)
        },
        sourcesGlobs = None,
        sourceRoots = None,
        dependencies = p.dependsOn.flat.map(_.value),
        classpath = classPath,
        out = workspaceDir / Defaults.BloopFolder / name.value,
        classesDir = workspaceDir / Defaults.BloopFolder / name.value / s"scala-${scalaVersion.binVersion}" / "classes",
        resources = p.resources match {
          case Some(providedResources) =>
            Some(providedResources.values.map(relPath => projectFolder / relPath))
          case None =>
            Some(Defaults.resourceDirs(projectFolder, scalaVersion, scope))
        },
        scala = Some(
          b.Scala(
            organization = scalaVersion.compiler.org,
            name = scalaCompiler.mangledArtifact,
            version = scalaCompiler.version,
            options = scalacOptions,
            jars = resolvedScalaCompiler,
            analysis = Some(projectFolder / "target" / "streams" / "compile" / "bloopAnalysisOut" / "_global" / "streams" / "inc_compile_2.12.zip"),
            setup = Some(setup)
          )
        ),
        java = Some(
          b.Java(
            options = mergedJava.flatMap(_.options).flat
          )
        ),
        sbt = None,
        test = if (isTest) Some(b.Test.defaultConfiguration) else None,
        platform = None,
        resolution = Some(resolution),
        tags = None
      )
    )
  }
}
