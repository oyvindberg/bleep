package bleep.testing

import bleep.{model, ResolvedProject, Usage}
import bleep.sbtimport.{findOriginalTargetDir, ImportInputData}
import bloop.config.Config

import java.nio.file.Path
import scala.collection.immutable.{SortedMap, SortedSet}
import scala.jdk.CollectionConverters.IteratorHasAsScala

/** Checks that an sbt import loses nothing, by taking bleep's resolved projects back to what sbt's bloop export said about the same projects.
  *
  * Both sides are reduced to a [[View]]: per aspect of a project (classpath, scalac options, sources, ...), a set of normalized entries. Sets, because the
  * ordering of neither side can be trusted - resolution order differs between coursier and sbt's ivy, and sbt's order of options and source directories is an
  * accident of how settings were appended.
  *
  * Normalization only removes what is known to differ for reasons that do not change what gets built:
  *   - absolute locations: sbt's build dir and bleep's build dir are rendered as `sbt-build/` and `bleep/`
  *   - jars: rendered as `name:version`, because sbt resolves some jars with ivy or its launcher and bleep resolves everything with coursier
  *   - class directories: rendered as the project they belong to
  *   - generated sources and resources: sbt keeps them in `target`, bleep in `.bleep/projects/<p>/generated-*`. Rendered as `<generated>` when sbt actually
  *     generated files there, dropped when the directory was empty
  *   - source directories that exist in no sbt project, which sbt lists anyway (`src/main/scala-2.13` and friends)
  *
  * Everything that remains is a real difference, and is written to a report which is checked in next to the snapshot. Known differences are then visible and
  * reviewed, and a new one shows up as a diff in CI.
  */
object ImportRoundtrip {
  import RoundtripReport.{Jar, View}

  def report(inputData: ImportInputData, resolved: Map[model.CrossProjectName, ResolvedProject], bleepBuildDir: Path): String = {
    val ctx = new Context(inputData, resolved, bleepBuildDir)

    val compared = inputData.projects.toList.collect { case (crossName, input) if resolved.contains(crossName) => (crossName, input) }.sortBy(_._1)
    val notImported = inputData.projects.keySet.filterNot(resolved.contains).toList.sorted.map(_.value)
    val droppedBloopProjects = inputData.bloopFiles.map(_.project.name).toSet.filterNot(inputData.byBloopName.contains).toList.sorted

    val diffs: List[(String, List[(String, List[String])])] =
      compared.flatMap { case (crossName, input) =>
        val byAspect = RoundtripReport.diff(ctx.inputView(input), ctx.outputView(crossName, resolved(crossName)))
        if (byAspect.isEmpty) None else Some((crossName.value, byAspect))
      }

    val header = List(
      "sbt bloop export vs bleep resolved projects, normalized. `-` only in sbt, `+` only in bleep, `~` same jar at another version",
      s"imported from sbt but not in the bleep build: ${if (notImported.isEmpty) "none" else notImported.mkString(", ")}",
      s"sbt bloop projects dropped by the import: ${if (droppedBloopProjects.isEmpty) "none" else droppedBloopProjects.mkString(", ")}"
    )
    RoundtripReport.render(header, compared.size, diffs)
  }

  private val defaultTestFrameworks: Set[String] = Config.Test.defaultConfiguration.frameworks.flatMap(_.names).toSet

  private class Context(inputData: ImportInputData, resolved: Map[model.CrossProjectName, ResolvedProject], bleepBuildDir: Path) {
    // the build's root. zio has sbt builds of its own inside it (`zio-examples/scala-js`), whose projects name their own directory as workspace
    val sbtBuildDir: Path = {
      val dirs = inputData.bloopFiles.map(_.project.workspaceDir.getOrElse(sys.error("bloop file without workspaceDir"))).distinct
      dirs.filter(dir => dirs.forall(_.startsWith(dir))) match {
        case Vector(root) => root
        case _            => sys.error(s"expected one sbt workspace dir containing the others, got $dirs")
      }
    }

    // scala version is part of the key, since sbt reuses the bloop project name for every cross version
    private def bloopKey(project: Config.Project): (String, Option[String]) = (project.name, project.scala.map(_.version))

    private val crossNameByBloopKey: Map[(String, Option[String]), model.CrossProjectName] =
      inputData.projects.map { case (crossName, input) => (bloopKey(input.bloopFile.project), crossName) }

    /** The import keeps one bloop project per cross id, so a project sbt exported at several patch versions of one binary version (scalameta's full cross
      * versioned projects depend on those) is represented by the one that was kept.
      */
    private def renderBloopProject(project: Config.Project): String =
      crossNameByBloopKey.get(bloopKey(project)) match {
        case Some(crossName) => crossName.value
        case None            =>
          val binVersion = project.scala.map(s => model.VersionScala(s.version).binVersion)
          crossNameByBloopKey.toList.collect {
            case ((project.name, Some(v)), crossName) if binVersion.contains(model.VersionScala(v).binVersion) => crossName
          } match {
            case List(crossName) => crossName.value
            case _               => s"${project.name} (not imported)"
          }
      }

    private val inputProjectByClassesDir: Map[Path, String] =
      inputData.bloopFiles.map(f => (f.project.classesDir, renderBloopProject(f.project))).toMap

    private type BloopKey = (String, Option[String])

    private val bloopProjectByKey: Map[BloopKey, Config.Project] =
      inputData.bloopFiles.map(f => (bloopKey(f.project), f.project)).toMap

    /** Which exported sbt project a dependency by name points to. `Left` when there is none.
      *   - full cross version projects (scalameta) may depend on a project sbt only exported at another patch version of the same binary version
      *   - a scala 3 project may depend on a 2.13 project (`for3Use2_13`)
      *   - java projects are exported once, under whichever scala version sbt picked
      */
    private def resolveDependency(name: String, scalaVersion: Option[String]): Either[String, BloopKey] =
      if (bloopProjectByKey.contains((name, scalaVersion))) Right((name, scalaVersion))
      else {
        val binVersions = scalaVersion.map(v => model.VersionScala(v).binVersion).toList.flatMap {
          case "3"   => List("3", "2.13")
          case other => List(other)
        }
        val candidates = bloopProjectByKey.keys.toList.collect { case key @ (`name`, _) => key }
        val byBinVersion = binVersions.iterator
          .map(bin => candidates.filter { case (_, v) => v.exists(v => model.VersionScala(v).binVersion == bin) }.sortBy(_._2).lastOption)
          .collectFirst { case Some(key) => key }
        (byBinVersion, candidates) match {
          case (Some(key), _)    => Right(key)
          case (None, List(one)) => Right(one)
          case (None, _)         => Left(s"$name (unknown)")
        }
      }

    private def renderDependency(dep: Either[String, BloopKey]): String =
      dep.fold(identity, key => renderBloopProject(bloopProjectByKey(key)))

    private val inputDirectDeps: Map[BloopKey, List[Either[String, BloopKey]]] =
      bloopProjectByKey.map { case (key, project) => (key, project.dependencies.map(name => resolveDependency(name, project.scala.map(_.version)))) }

    // keyed by sbt's projects, not by what they are rendered as: several sbt projects may be represented by one bleep project
    private def inputDependsOn(project: Config.Project): SortedSet[String] = {
      val seen = scala.collection.mutable.Set.empty[Either[String, BloopKey]]
      def go(dep: Either[String, BloopKey]): Unit =
        if (seen.add(dep)) dep.foreach(key => inputDirectDeps(key).foreach(go))
      inputDirectDeps(bloopKey(project)).foreach(go)
      seen.iterator.map(renderDependency).to(SortedSet)
    }

    private val outputDirectDeps: Map[String, Set[String]] =
      resolved.map { case (crossName, p) => (crossName.value, p.dependencies.toSet) }

    // one side may list dependencies transitively and the other not. what matters is what ends up upstream
    private def closure(direct: Map[String, Set[String]], from: Set[String]): SortedSet[String] = {
      val seen = scala.collection.mutable.Set.empty[String]
      def go(name: String): Unit =
        if (seen.add(name)) direct.getOrElse(name, Set.empty).foreach(go)
      from.foreach(go)
      seen.to(SortedSet)
    }

    private val outputProjectByClassesDir: Map[Path, String] =
      resolved.map { case (crossName, p) => (p.classesDir, crossName.value) }

    // bleep puts resource directories of dependencies directly on the classpath, sbt copies them into the classes directory
    private val outputResourceDirs: Set[Path] =
      resolved.values.flatMap(_.resources(Usage.Runtime)).toSet

    // source directories no sbt project has sources in. sbt lists these for every project anyway
    private val knownEmptySourceDirs: Set[Path] =
      inputData.bloopFiles.flatMap(_.project.sources).toSet.filterNot(inputData.hasSources)

    /** All we know about which directories exist comes from sbt: if a directory had sources for any project, sbt would have listed it for that project. So a
      * directory bleep adds which no sbt project lists (`src/main/java` for a scala-only cross project, say) holds nothing sbt compiled.
      */
    private val dirsListedBySbt: Set[Path] =
      inputData.bloopFiles.flatMap(f => f.project.sources ++ f.project.resources.getOrElse(Nil)).map(_.normalize()).toSet

    private def isManaged(p: Path): Boolean =
      p.iterator().asScala.map(_.toString).exists(s => s == "src_managed" || s == "resource_managed")

    private def bleepGenerated(p: Path): Option[String] =
      if (!p.startsWith(bleepBuildDir)) None
      else p.iterator().asScala.map(_.toString).find(s => s == "generated-sources" || s == "generated-resources")

    def location(p: Path): String =
      if (p.startsWith(sbtBuildDir)) s"sbt-build/${sbtBuildDir.relativize(p)}"
      else if (p.startsWith(bleepBuildDir)) s"bleep/${bleepBuildDir.relativize(p)}"
      else p.toString

    /** Like the import: anything under sbt's target directory is generated, whether by sbt itself (`src_managed`) or a plugin (akka-grpc) */
    def inputDirs(targetDir: Option[Path], dirs: List[Path]): SortedSet[String] =
      dirs.iterator
        .map(_.normalize())
        .flatMap {
          case dir if isManaged(dir) || targetDir.exists(dir.startsWith) =>
            if (inputData.generatedFilesBySourceDir.get(dir).exists(_.nonEmpty)) Some("<generated>") else None
          case dir if knownEmptySourceDirs(dir) => None
          case dir                              => Some(location(dir))
        }
        .to(SortedSet)

    /** bleep's sourcegen script declares both a sources and a resources directory, and only writes into the ones the import found generated files for */
    def outputDirs(crossName: model.CrossProjectName, dirs: List[Path]): SortedSet[String] = {
      val generated = inputData.generatedFiles.getOrElse(crossName, Vector.empty)
      dirs.iterator
        .map(_.normalize())
        .flatMap { dir =>
          bleepGenerated(dir) match {
            case Some("generated-sources")         => if (generated.exists(!_.isResource)) Some("<generated>") else None
            case Some(_)                           => if (generated.exists(_.isResource)) Some("<generated>") else None
            case None if knownEmptySourceDirs(dir) => None
            case None if !dirsListedBySbt(dir)     => None
            case None                              => Some(location(dir))
          }
        }
        .to(SortedSet)
    }

    /** The compiler itself. sbt's scala instance for Scala 3 also carries scaladoc and everything it depends on, which only the `doc` task uses */
    def compilerJars(jars: List[Path]): SortedSet[String] =
      jars.iterator
        .flatMap(jar)
        .filter {
          case Jar(name, _) =>
            (name.startsWith("scala") || name.startsWith("tasty-core")) && !name.startsWith("scaladoc") && !name.startsWith("scala3-tasty-inspector")
          case _ => true
        }
        .to(SortedSet)

    /** `name:version` for every way a jar ends up on a classpath: coursier's cache, ivy's cache and sbt's launcher. `None` for the JDK's own jars, which sbt
      * puts on the classpath of some builds and bleep never does.
      */
    def jar(p: Path): Option[String] =
      // packaged by the sbt build itself, for instance a compiler plugin
      RoundtripReport.jar(p, ownBuild = p => if (p.startsWith(sbtBuildDir)) Some(location(p)) else None)

    def inputClasspath(entries: List[Path]): SortedSet[String] =
      entries.iterator
        .flatMap {
          case p if p.getFileName.toString.endsWith(".jar") => jar(p)
          case p                                            =>
            inputProjectByClassesDir.get(p) match {
              case Some(project) => Some(s"project $project")
              case None          =>
                // a java project is exported once, and referenced from projects of every scala version: `.bleep/import/bloop/<scala>/<name>/classes`
                sbtBuildDir.relativize(p).iterator().asScala.map(_.toString).toList match {
                  case ".bleep" :: "import" :: "bloop" :: scalaVersion :: name :: rest =>
                    val bloopName = if (rest.lastOption.contains("test-classes")) s"$name-test" else name
                    Some(s"project ${renderDependency(resolveDependency(bloopName, Some(scalaVersion)))}")
                  case _ => Some(location(p))
                }
            }
        }
        .to(SortedSet)

    def outputClasspath(entries: List[Path]): SortedSet[String] =
      entries.iterator
        .flatMap {
          case p if outputResourceDirs(p)                   => None
          case p if p.getFileName.toString.endsWith(".jar") => jar(p)
          case p                                            =>
            outputProjectByClassesDir.get(p) match {
              case Some(project) => Some(s"project $project")
              case None          => Some(location(p))
            }
        }
        .to(SortedSet)

    /** Like [[options]], and scala native's list of source directories compared a directory at a time, since the order of either side says nothing.
      *
      * The plugin relativizes a file being compiled against a listed directory it is in, so only a directory holding the project's own sources does anything.
      * Those are compared. sbt lists more for a test project: its `Test / scalacOptions` start from `Compile / scalacOptions`, so the main project's
      * directories are in there too
      */
    def scalaOptions(opts: List[String], targetDirs: List[Path], sources: List[Path], normalizeDirs: List[Path] => SortedSet[String]): SortedSet[String] = {
      val (relativization, rest) = opts.partition(_.startsWith(model.VersionScalaNative.PositionRelativizationPathsPrefix))
      val listed =
        relativization.flatMap(_.stripPrefix(model.VersionScalaNative.PositionRelativizationPathsPrefix).split(';').toList).map(Path.of(_).normalize())
      val applying = listed.filter(dir => sources.exists(_.normalize().startsWith(dir)))
      options(rest, targetDirs) ++ normalizeDirs(applying).map(dir => s"positionRelativizationPaths $dir")
    }

    def options(opts: List[String], targetDirs: List[Path]): SortedSet[String] = {
      val replacements = targetDirs.map(d => (d.toString, "<target>")) ++ List((sbtBuildDir.toString, "sbt-build"), (bleepBuildDir.toString, "bleep"))
      model.Options
        .parse(opts, None)
        .values
        .iterator
        .map(_.render.mkString(" "))
        .flatMap {
          // the sbt builds are exported with semanticdb enabled for metals. bleep enables semanticdb itself when an IDE asks for it
          case s if s.contains("semanticdb") => Nil
          // bleep adds `-sourceroot` to scala 3 builds, so TASTy holds no absolute paths; sbt leaves it out
          case s if s.startsWith("-sourceroot") => Nil
          // sbt runs in the build root, and bleep sets `-Duser.dir` to it by default
          case s if s == s"-Duser.dir=$sbtBuildDir" || s == s"-Duser.dir=$bleepBuildDir" => Nil
          // bleep passes the plugin together with its dependencies, sbt only the plugin. one entry per jar, so the plugin itself is compared
          case s if s.startsWith("-Xplugin:") =>
            s.stripPrefix("-Xplugin:").split(java.io.File.pathSeparator).toList.flatMap(p => jar(Path.of(p))).map(j => s"-Xplugin:$j")
          case s =>
            // the root of the build is a different directory on each side by construction
            val withRoots = s.replace(s"file:$sbtBuildDir/->", "file:<build-dir>/->").replace(s"file:$bleepBuildDir/->", "file:<build-dir>/->")
            List(replacements.foldLeft(withRoots) { case (acc, (from, to)) => acc.replace(from, to) })
        }
        .to(SortedSet)
    }

    // bleep names module kinds after scala-js, bloop has its own ids
    private def jsKindId(kind: String): String = kind match {
      case "CommonJSModule"        => Config.ModuleKindJS.CommonJSModule.id
      case "ESModule"              => Config.ModuleKindJS.ESModule.id
      case "NoModule" | "nomodule" => Config.ModuleKindJS.NoModule.id
      case other                   => other
    }

    def inputView(input: ImportInputData.InputProject): View = {
      val p = input.bloopFile.project
      val targetDirs = List(p.out, p.classesDir) ++ findOriginalTargetDir(p).toList
      val isTest = p.tags.getOrElse(Nil).contains(bloop.config.Tag.Test)

      val platform: SortedSet[String] = p.platform match {
        case None                                                        => SortedSet.empty
        case Some(Config.Platform.Jvm(config, mainClass, runtime, _, _)) =>
          SortedSet("name=jvm") ++ mainClass.map(m => s"mainClass=$m") ++
            options(config.options, targetDirs).map(o => s"jvmOptions $o") ++
            runtime.toList.flatMap(r => options(r.options, targetDirs)).map(o => s"jvmRuntimeOptions $o")
        case Some(Config.Platform.Js(config, mainClass)) =>
          SortedSet(
            // sbt's bloop export leaves the scala-js version empty, so it is not compared
            "name=js",
            s"mode=${config.mode.id}",
            s"kind=${config.kind.id}",
            s"emitSourceMaps=${config.emitSourceMaps}"
          ) ++ config.jsdom.map(j => s"jsdom=$j") ++ mainClass.map(m => s"mainClass=$m")
        case Some(Config.Platform.Native(config, mainClass)) =>
          SortedSet("name=native", s"version=${config.version}", s"mode=${config.mode.id}", s"gc=${config.gc}") ++ mainClass.map(m => s"mainClass=$m")
      }

      val scala: View = p.scala match {
        case None    => SortedMap.empty
        case Some(s) =>
          SortedMap(
            "scala" -> SortedSet(s"version=${s.version}"),
            "scala.options" -> scalaOptions(s.options, targetDirs, p.sources, dirs => inputDirs(findOriginalTargetDir(p), dirs)),
            "scala.compilerJars" -> compilerJars(s.jars),
            "scala.setup" -> s.setup.toList
              .flatMap(setup =>
                List(
                  s"order=${setup.order.id}",
                  s"addLibraryToBootClasspath=${setup.addLibraryToBootClasspath}",
                  s"addCompilerToClasspath=${setup.addCompilerToClasspath}",
                  s"addExtraJarsToClasspath=${setup.addExtraJarsToClasspath}",
                  s"manageBootClasspath=${setup.manageBootClasspath}",
                  s"filterLibraryFromClasspath=${setup.filterLibraryFromClasspath}"
                )
              )
              .to(SortedSet)
          )
      }

      SortedMap(
        "dependsOn" -> inputDependsOn(p),
        "sources" -> inputDirs(findOriginalTargetDir(p), p.sources),
        "resources" -> inputDirs(findOriginalTargetDir(p), p.resources.getOrElse(Nil)),
        "classpath" -> inputClasspath(p.classpath),
        "java.options" -> options(p.java.map(_.options).getOrElse(Nil), targetDirs),
        "platform" -> platform,
        "isTestProject" -> SortedSet(isTest.toString),
        "testFrameworks" -> (if (isTest) p.test.toList.flatMap(_.frameworks.flatMap(_.names)).filterNot(defaultTestFrameworks).to(SortedSet)
                             else SortedSet.empty[String])
      ) ++ scala
    }

    def outputView(crossName: model.CrossProjectName, p: ResolvedProject): View = {
      val targetDirs = List(p.directory, p.classesDir)

      val platform: SortedSet[String] = p.platform match {
        case None                                                                      => SortedSet.empty
        case Some(ResolvedProject.Platform.Jvm(jvmOptions, mainClass, runtimeOptions)) =>
          SortedSet("name=jvm") ++ mainClass.map(m => s"mainClass=$m") ++
            options(jvmOptions, targetDirs).map(o => s"jvmOptions $o") ++
            options(runtimeOptions, targetDirs).map(o => s"jvmRuntimeOptions $o")
        case Some(ResolvedProject.Platform.Js(version, mode, kind, emitSourceMaps, jsdom, _, mainClass)) =>
          SortedSet("name=js", s"mode=$mode", s"kind=${jsKindId(kind)}", s"emitSourceMaps=$emitSourceMaps") ++
            jsdom.map(j => s"jsdom=$j") ++ mainClass.map(m => s"mainClass=$m")
        case Some(ResolvedProject.Platform.Native(version, mode, gc, mainClass)) =>
          SortedSet("name=native", s"version=$version", s"mode=$mode", s"gc=$gc") ++ mainClass.map(m => s"mainClass=$m")
      }

      val scala: View = p.language match {
        case s: ResolvedProject.Language.Scala =>
          SortedMap(
            "scala" -> SortedSet(s"version=${s.version}"),
            "scala.options" -> scalaOptions(s.options, targetDirs, p.sources, dirs => outputDirs(crossName, dirs)),
            "scala.compilerJars" -> compilerJars(s.compilerJars),
            "scala.setup" -> s.setup.toList
              .flatMap(setup =>
                List(
                  s"order=${setup.order.id}",
                  s"addLibraryToBootClasspath=${setup.addLibraryToBootClasspath}",
                  s"addCompilerToClasspath=${setup.addCompilerToClasspath}",
                  s"addExtraJarsToClasspath=${setup.addExtraJarsToClasspath}",
                  s"manageBootClasspath=${setup.manageBootClasspath}",
                  s"filterLibraryFromClasspath=${setup.filterLibraryFromClasspath}"
                )
              )
              .to(SortedSet)
          )
        case _: ResolvedProject.Language.Java | _: ResolvedProject.Language.Kotlin => SortedMap.empty
      }

      SortedMap(
        "dependsOn" -> closure(outputDirectDeps, outputDirectDeps(crossName.value)),
        "sources" -> outputDirs(crossName, p.sources),
        "resources" -> outputDirs(crossName, p.resources(Usage.Runtime)),
        "classpath" -> outputClasspath(p.classpath(Usage.Compile)),
        "java.options" -> (options(p.language.javaOptions, targetDirs) - "-proc:none"),
        "platform" -> platform,
        "isTestProject" -> SortedSet(p.isTestProject.toString),
        "testFrameworks" -> (if (p.isTestProject) p.testFrameworks.to(SortedSet) else SortedSet.empty[String])
      ) ++ scala
    }
  }
}
