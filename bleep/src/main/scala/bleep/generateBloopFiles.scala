package bleep

import bleep.internal.{rewriteDependentData, Lazy}
import bleep.model.orderingDep
import bloop.config.{Config => b}
import coursier.core.Configuration
import coursier.parse.JavaOrScalaDependency
import coursier.{Classifier, Dependency}

import java.nio.file.Path
import scala.collection.immutable.SortedMap
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

    val maybeScala: Option[model.Scala] = {
      def go(s: model.Scala): model.Scala =
        s.`extends` match {
          case Some(id) =>
            val found = build.scala.flatMap(scalas => scalas.get(id)).getOrElse(sys.error(s"referenced non-existing scala definition ${id.value}"))
            s.union(go(found))
          case None =>
            s
        }
      proj.scala.map(go)
    }

    val mergedJava: Option[model.Java] =
      List(proj.java, build.java).flatten.reduceOption(_ union _)

    val scalaVersion: Option[Versions.Scala] =
      maybeScala.flatMap(_.version)

    val explodedPlatform: Option[model.Platform] =
      proj.platform.map { platform =>
        def fail(msg: String) =
          sys.error(s"${projName.value}/${platform.name.value}: $msg")

        def explode(platform: model.Platform): model.Platform =
          platform.`extends` match {
            case Some(id) =>
              build.platforms.flatMap(_.get(id)) match {
                case Some(referenced) =>
                  model.Platform.unsafeReduce(platform, explode(referenced))(_ union _, _ union _, _ union _)

                case None =>
                  fail(s"Extends non-existing platform ${id.value}")
              }
            case None => platform
          }

        explode(platform)
      }

    val configuredPlatform: Option[b.Platform] =
      explodedPlatform.map { platform =>
        def fail(msg: String) =
          sys.error(s"${projName.value}/${platform.name.value}: $msg")

        platform match {
          case model.Platform.Js(_, version, mode, kind, emitSourceMaps, jsdom, mainClass) =>
            b.Platform.Js(
              b.JsConfig(
                version = version match {
                  case Some(value) => value.scalaJsVersion
                  case None        => fail("missing `version`")
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
          case model.Platform.Jvm(_, options, mainClass, runtimeOptions) =>
            b.Platform.Jvm(
              config = b.JvmConfig(
                home = None,
                options = options.map(_.render).getOrElse(Nil)
              ),
              mainClass = mainClass,
              runtimeConfig = runtimeOptions.map(ro => b.JvmConfig(home = None, options = ro.render)),
              classpath = None,
              resources = None
            )
          case model.Platform.Native(_, version, mode, gc, mainClass) => ???
        }
      }

    val platformSuffix =
      explodedPlatform match {
        case Some(x: model.Platform.Js)     => s"sjs${x.version.get.scalaJsBinVersion}"
        case Some(x: model.Platform.Native) => s"native${x.version.get.scalaNativeBinVersion}"
        case _                              => ""
      }

    val resolution: b.Resolution = {
      val transitiveDeps: JsonSet[JavaOrScalaDependency] =
        proj.dependencies.union(JsonSet.fromIterable(build.transitiveDependenciesFor(projName).flatMap { case (_, p) => p.dependencies.values }))

      val concreteDeps: JsonSet[Dependency] =
        transitiveDeps.map { dep =>
          scalaVersion match {
            case Some(scalaVersion) => dep.dependency(scalaVersion.binVersion, scalaVersion.scalaVersion, platformSuffix)
            case None               => sys.error(s"Need a configured scala version to resolve $dep")
          }
        }

      val result = Await.result(resolver(concreteDeps, build.resolvers), Duration.Inf)

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

    val classPath: JsonSet[Path] = JsonSet.fromIterable {
      allTransitiveTranslated.values.map(_.project.classesDir) ++
        resolution.modules.flatMap(_.artifacts.collect { case x if !x.classifier.contains(Classifier.sources.value) => x }).map(_.path)
    }

    val configuredScala: Option[b.Scala] =
      scalaVersion.map { scalaVersion =>
        val scalaCompiler: Dependency =
          scalaVersion.compiler.dependency(scalaVersion.scalaVersion)

        val resolvedScalaCompiler: List[Path] =
          Await.result(resolver(JsonSet(scalaCompiler), build.resolvers), Duration.Inf).files.toList.map(_.toPath)

        val setup = {
          val provided = maybeScala.flatMap(_.setup)
          b.CompileSetup(
            order = provided.flatMap(_.order).getOrElse(b.CompileSetup.empty.order),
            addLibraryToBootClasspath = provided.flatMap(_.addLibraryToBootClasspath).getOrElse(b.CompileSetup.empty.addLibraryToBootClasspath),
            addCompilerToClasspath = provided.flatMap(_.addCompilerToClasspath).getOrElse(b.CompileSetup.empty.addCompilerToClasspath),
            addExtraJarsToClasspath = provided.flatMap(_.addExtraJarsToClasspath).getOrElse(b.CompileSetup.empty.addExtraJarsToClasspath),
            manageBootClasspath = provided.flatMap(_.manageBootClasspath).getOrElse(b.CompileSetup.empty.manageBootClasspath),
            filterLibraryFromClasspath = provided.flatMap(_.filterLibraryFromClasspath).getOrElse(b.CompileSetup.empty.filterLibraryFromClasspath)
          )
        }

        val compilerPlugins: Options = {
          maybeScala.fold(JsonSet.empty[JavaOrScalaDependency])(_.compilerPlugins) match {
            case empty if empty.values.isEmpty => Options.Empty
            case nonEmpty =>
              val reps = nonEmpty.map(_.dependency(scalaVersion.scalaVersion))
              val resolved = Await.result(resolver(reps, build.resolvers), Duration.Inf).files.toList.map(_.toPath)
              val relevant = resolved.filterNot(p => p.endsWith(".jar") && !p.toString.contains("-sources") && !p.toString.contains("-javadoc"))
              new Options(relevant.map(p => Options.Opt.Flag(s"${Defaults.ScalaPluginPrefix}$p")))
          }
        }

        val scalacOptions: Options =
          maybeScala.flatMap(_.options).getOrElse(Options.Empty) union compilerPlugins

        b.Scala(
          organization = scalaCompiler.module.organization.value,
          name = scalaCompiler.module.name.value,
          version = scalaCompiler.version,
          options = scalacOptions.render,
          jars = resolvedScalaCompiler,
          analysis = Some(
            projectFolder / "target" / "streams" / "compile" / "bloopAnalysisOut" / "_global" / "streams" / s"inc_compile_${scalaVersion.binVersion}.zip"
          ),
          setup = Some(setup)
        )
      }

    val scope = proj.`sbt-scope`.getOrElse("main")

    def sourceLayout = proj.`source-layout` match {
      case Some(sourceLayout) => sourceLayout
      case None               => if (scalaVersion.isDefined) SourceLayout.Normal else SourceLayout.Java
    }

    val sources: JsonSet[Path] =
      (sourceLayout.sources(scalaVersion, proj.`sbt-scope`) ++ JsonSet.fromIterable(proj.sources.values)).map(projectFolder / _)

    val resources: JsonSet[Path] =
      (sourceLayout.resources(scalaVersion, proj.`sbt-scope`) ++ JsonSet.fromIterable(proj.resources.values)).map(projectFolder / _)

    b.File(
      "1.4.0",
      b.Project(
        projName.value,
        projectFolder,
        Some(workspaceDir),
        sources = sources.values.toList,
        sourcesGlobs = None,
        sourceRoots = None,
        dependencies = JsonSet.fromIterable(allTransitiveTranslated.keys.map(_.value)).values.toList,
        classpath = classPath.values.toList,
        out = workspaceDir / Defaults.BloopFolder / projName.value,
        classesDir = scalaVersion match {
          case Some(scalaVersion) => workspaceDir / Defaults.BloopFolder / projName.value / s"scala-${scalaVersion.binVersion}" / "classes"
          case None               => workspaceDir / Defaults.BloopFolder / projName.value / "classes"
        },
        resources = Some(resources.values.toList),
        scala = configuredScala,
        java = Some(
          b.Java(
            options = mergedJava.flatMap(_.options).getOrElse(Options.Empty).render
          )
        ),
        sbt = None,
        test = if (scope == "test") Some(b.Test.defaultConfiguration) else None,
        platform = configuredPlatform,
        resolution = Some(resolution),
        tags = None
      )
    )
  }
}
