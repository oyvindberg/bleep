package bleep

import bleep.internal.{rewriteDependentData, Lazy}
import bloop.config.{Config => b}
import coursier.{Classifier, Dependency}
import coursier.core.Configuration
import coursier.parse.JavaOrScalaDependency

import java.nio.file.Path
import scala.collection.immutable.{SortedMap, SortedSet}
import scala.concurrent.Await
import scala.concurrent.duration.Duration

object generateBloopFiles {
  implicit val ordering: Ordering[Dependency] =
    Ordering.by(_.toString())

  def apply(build: model.Build, workspaceDir: Path, resolver: CoursierResolver): SortedMap[model.ProjectName, Lazy[b.File]] = {
    verify(build)

    rewriteDependentData(build.projects) { (projectName, project, getDep) =>
      translateProject(
        resolver,
        workspaceDir,
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
      workspaceDir: Path,
      projName: model.ProjectName,
      proj: model.Project,
      build: model.Build,
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
      model.Scala.merge(proj.scala, build.scala)

    val mergedJava: Option[model.Java] =
      model.Java.merge(proj.java, build.java)

    val scalaVersion: Option[Versions.Scala] =
      mergedScala.flatMap(_.version)

    val scalacOptions: List[String] =
      mergedScala.flatMap(_.options).flat

    val resolution: b.Resolution = {
      val transitiveDeps: Seq[JavaOrScalaDependency] =
        proj.dependencies.flat ++ build.transitiveDependenciesFor(projName).flatMap { case (_, p) => p.dependencies.flat }

      val concreteDeps: SortedSet[Dependency] = {
        val seq = transitiveDeps.map { dep =>
          scalaVersion match {
            case Some(scalaVersion) => dep.dependency(scalaVersion.binVersion, scalaVersion.scalaVersion, "todo: scala.js version")
            case None               => sys.error(s"Need a configured scala version to resolve $dep")
          }
        }

        SortedSet.empty[Dependency] ++ seq
      }

      val result = Await.result(resolver(concreteDeps, build.resolvers.flat), Duration.Inf)

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
      allTransitiveBloop.values.map(_.project.classesDir).toList.sorted ++
        resolution.modules.flatMap(_.artifacts.collect { case x if !x.classifier.contains(Classifier.sources.value) => x }).map(_.path).sorted
    }

    val configuredScala: Option[b.Scala] =
      scalaVersion.map { scalaVersion =>
        val scalaCompiler: Dependency =
          scalaVersion.compiler.dependency(scalaVersion.scalaVersion)

        val resolvedScalaCompiler: List[Path] =
          Await.result(resolver(SortedSet(scalaCompiler), build.resolvers.flat), Duration.Inf).files.toList.map(_.toPath)

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
          organization = scalaCompiler.module.organization.value,
          name = scalaCompiler.module.name.value,
          version = scalaCompiler.version,
          options = scalacOptions,
          jars = resolvedScalaCompiler,
          analysis = Some(projectFolder / "target" / "streams" / "compile" / "bloopAnalysisOut" / "_global" / "streams" / "inc_compile_2.12.zip"),
          setup = Some(setup)
        )
      }

    val scope = proj.`sbt-scope`.getOrElse("main")

    def sourceLayout = proj.`source-layout` match {
      case Some(sourceLayout) => sourceLayout
      case None               => if (scalaVersion.isDefined) SourceLayout.Normal else SourceLayout.Java
    }

    val sources: List[Path] =
      sourceLayout.sources(scalaVersion, proj.`sbt-scope`) ++ proj.sources.flat map (projectFolder / _)

    val resources: List[Path] =
      sourceLayout.resources(scalaVersion, proj.`sbt-scope`) ++ proj.resources.flat map (projectFolder / _)

    b.File(
      "1.4.0",
      b.Project(
        projName.value,
        projectFolder,
        Some(workspaceDir),
        sources = sources,
        sourcesGlobs = None,
        sourceRoots = None,
        dependencies = allTransitiveBloop.keys.map(_.value).toList.sorted,
        classpath = classPath,
        out = workspaceDir / Defaults.BloopFolder / projName.value,
        classesDir = scalaVersion match {
          case Some(scalaVersion) => workspaceDir / Defaults.BloopFolder / projName.value / s"scala-${scalaVersion.binVersion}" / "classes"
          case None               => workspaceDir / Defaults.BloopFolder / projName.value / "classes"
        },
        resources = Some(resources).filterNot(_.isEmpty),
        scala = configuredScala,
        java = Some(
          b.Java(
            options = mergedJava.flatMap(_.options).flat
          )
        ),
        sbt = None,
        test = if (scope == "test") Some(b.Test.defaultConfiguration) else None,
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
