package bleep

import bleep.internal.Lazy
import bloop.config.{Config => b}
import coursier.Classifier
import coursier.core.Configuration

import java.nio.file.Path
import scala.collection.immutable.{SortedMap, SortedSet}
import scala.concurrent.Await
import scala.concurrent.duration.Duration

object generateBloopFiles {
  def apply(file: model.File, workspaceDir: Path, resolver: CoursierResolver): SortedMap[model.ProjectName, Lazy[b.File]] = {
    verify(file)

    // sorted to ensure consistency
    val sortedProjects = SortedMap.empty[model.ProjectName, model.Project] ++ file.projects

    lazy val resolvedProjects: SortedMap[model.ProjectName, Lazy[b.File]] =
      sortedProjects.map { case (projectName, project) =>
        projectName -> Lazy(
          translateProject(
            resolver,
            workspaceDir,
            projectName,
            project,
            file,
            None,
            name =>
              resolvedProjects
                .getOrElse(name, sys.error(s"Project ${projectName.value} depends on non-existing project ${name.value}"))
                .forceGet(projectName.value)
          )
        )
      }

    resolvedProjects
  }

  def verify(file: model.File): Unit =
    file.scripts match {
      case None => ()
      case Some(scripts) =>
        scripts.foreach { case (scriptName, scriptDefs) =>
          scriptDefs.values.foreach { scriptDef =>
            if (file.projects.contains(scriptDef.project)) ()
            else sys.error(s"script ${scriptName.value} references non-existing project ${scriptDef.project.value}")
          }
        }
    }

  def translateProject(
      resolver: CoursierResolver,
      workspaceDir: Path,
      projName: model.ProjectName,
      proj: model.Project,
      inFile: model.File,
      scalaJsVersion: Option[Versions.ScalaJs],
      getBloopProject: model.ProjectName => b.File
  ): b.File = {
    val projectFolder: Path =
      proj.folder match {
        case Some(relPath) => workspaceDir / relPath
        case None          => workspaceDir / projName.value
      }

    val allTransitiveBloop: Map[model.ProjectName, b.File] = {
      val builder = Map.newBuilder[model.ProjectName, b.File]

      def go(n: model.ProjectName, p: b.File): Unit = {
        if (n == projName) sys.error(s"project ${projName.value} transitively depends on itself")
        builder += ((n, p))
        p.project.dependencies.foreach(projectName => go(model.ProjectName(projectName), getBloopProject(model.ProjectName(projectName))))
      }

      proj.dependsOn.flat.foreach(projectName => go(projectName, getBloopProject(projectName)))

      builder.result()
    }

    val mergedScala: Option[model.Scala] =
      model.Scala.merge(proj.scala, inFile.scala)

    val mergedJava: Option[model.Java] =
      model.Java.merge(proj.java, inFile.java)

    val scalaVersion: Option[Versions.Scala] =
      mergedScala.flatMap(_.version)

    val scalacOptions: List[String] =
      mergedScala.flatMap(_.options).flat

    val resolution: b.Resolution = {
      val transitiveDeps: Seq[Dep] =
        proj.dependencies.flat ++ inFile.transitiveDependenciesFor(projName).flatMap { case (_, p) => p.dependencies.flat }

      val concreteDeps: SortedSet[Dep.Concrete] = {
        val seq = transitiveDeps.map {
          case concrete: Dep.Concrete => concrete
          case dep =>
            scalaVersion match {
              case Some(scalaVersion) => dep.concrete(scalaVersion, scalaJsVersion)
              case None               => sys.error(s"Need a configured scala version to resolve $dep")
            }
        }

        SortedSet.empty[Dep.Concrete] ++ seq
      }

      val result = Await.result(resolver(concreteDeps), Duration.Inf)

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

    val classPath: List[Path] = {
      allTransitiveBloop.values.map(_.project.classesDir).toList ++
        resolution.modules.flatMap(_.artifacts).map(_.path)
    }

    val isTest = projName.value.endsWith("-test")
    val scope = if (isTest) "test" else "main"

    val configuredScala: Option[b.Scala] =
      scalaVersion.map { scalaVersion =>
        val scalaCompiler: Dep.Concrete =
          scalaVersion.compiler.concrete(scalaVersion, scalaJsVersion)

        val resolvedScalaCompiler: List[Path] =
          Await.result(resolver(SortedSet(scalaCompiler)), Duration.Inf).files.toList.map(_.toPath)

        val setup = {
          val provided = mergedScala.flatMap(_.setup)
          b.CompileSetup(
            order = provided.flatMap(_.order).getOrElse(b.CompileSetup.empty.order),
            addLibraryToBootClasspath = provided.flatMap(_.addLibraryToBootClasspath).getOrElse(b.CompileSetup.empty.addLibraryToBootClasspath),
            addCompilerToClasspath = provided.flatMap(_.addCompilerToClasspath).getOrElse(b.CompileSetup.empty.addCompilerToClasspath),
            addExtraJarsToClasspath = provided.flatMap(_.addExtraJarsToClasspath).getOrElse(b.CompileSetup.empty.addExtraJarsToClasspath),
            manageBootClasspath = provided.flatMap(_.manageBootClasspath).getOrElse(b.CompileSetup.empty.manageBootClasspath),
            filterLibraryFromClasspath = provided.flatMap(_.filterLibraryFromClasspath).getOrElse(b.CompileSetup.empty.filterLibraryFromClasspath)
          )
        }

        b.Scala(
          organization = scalaVersion.compiler.org,
          name = scalaCompiler.mangledArtifact,
          version = scalaCompiler.version,
          options = scalacOptions,
          jars = resolvedScalaCompiler,
          analysis = Some(projectFolder / "target" / "streams" / "compile" / "bloopAnalysisOut" / "_global" / "streams" / "inc_compile_2.12.zip"),
          setup = Some(setup)
        )
      }

    b.File(
      "1.4.0",
      b.Project(
        projName.value,
        projectFolder,
        Some(workspaceDir),
        proj.sources match {
          case Some(providedSources) => providedSources.values.map(relPath => projectFolder / relPath)
          case None                  => Defaults.sourceDirs(scalaVersion, scope).map(projectFolder / _)
        },
        sourcesGlobs = None,
        sourceRoots = None,
        dependencies = proj.dependsOn.flat.map(_.value),
        classpath = classPath,
        out = workspaceDir / Defaults.BloopFolder / projName.value,
        classesDir = workspaceDir / Defaults.BloopFolder / projName.value / s"scala-${scalaVersion.binVersion}" / "classes",
        resources = proj.resources match {
          case Some(providedResources) =>
            Some(providedResources.values.map(relPath => projectFolder / relPath))
          case None =>
            Some(Defaults.resourceDirs(scalaVersion, scope).map(projectFolder / _))
        },
        scala = configuredScala,
        java = Some(
          b.Java(
            options = mergedJava.flatMap(_.options).flat
          )
        ),
        sbt = None,
        test = if (isTest) Some(b.Test.defaultConfiguration) else None,
        // platform is mostly todo:
        platform = proj.platform.flatMap {
          case model.Platform.Jvm(config, mainClass, runtimeConfig) =>
            Some(
              b.Platform.Jvm(
                config = b.JvmConfig.empty,
                mainClass = mainClass,
                runtimeConfig = None,
                classpath = None,
                resources = None
              )
            )
          case _ => None
        },
        resolution = Some(resolution),
        tags = None
      )
    )
  }
}
