package bleep
package commands

import bleep.commands.Import.findGeneratedJsonFiles
import bleep.internal._
import bleep.logging.Logger
import bleep.rewrites.normalizeBuild
import cats.syntax.apply._
import com.monovore.decline.Opts

import java.nio.file.{Files, Path}
import scala.collection.immutable.SortedMap
import scala.collection.mutable
import scala.jdk.CollectionConverters._
import scala.sys.process.Process

object Import {
  case class Options(
      ignoreWhenInferringTemplates: Set[model.ProjectName],
      skipSbt: Boolean,
      skipGeneratedResourcesScript: Boolean
  )

  val ignoreWhenInferringTemplates: Opts[Set[model.ProjectName]] = Opts
    .options[String](
      "ignore-when-templating",
      "some projects may differ much from the rest, for instance documentation and examples. considering these when computing templates may negatively affect the result",
      "i"
    )
    .orEmpty
    .map(_.map(model.ProjectName.apply))
    .map(_.toSet)

  val skipSbt: Opts[Boolean] =
    Opts.flag("skip-sbt", "use if you already have generated bloop files and want to reimport from them").orFalse

  val skipGeneratedResourcesScript: Opts[Boolean] =
    Opts.flag("skip-generated-resources-script", "disable creating a script to regenerate discovered generated sources/resources ").orFalse

  val opts: Opts[Options] =
    (ignoreWhenInferringTemplates, skipSbt, skipGeneratedResourcesScript).mapN(Options.apply)

  def findGeneratedJsonFiles(under: Path): Iterable[Path] =
    Files
      .list(under)
      .filter(Files.isDirectory(_))
      .flatMap(dir => Files.list(dir).filter(x => Files.isRegularFile(x) && x.getFileName.toString.endsWith(".json")))
      .toList
      .asScala

  def parseProjectsOutput(lines: List[String]): Map[Path, List[String]] = {
    var currentPath = Option.empty[Path]
    val projectNames = List.newBuilder[String]
    val b = Map.newBuilder[Path, List[String]]
    var i = 0

    def commit() = {
      currentPath match {
        case Some(currentPath) => b += ((currentPath, projectNames.result()))
        case None              => ()
      }
      projectNames.clear()
    }

    while (i < lines.length) {
      val line = lines(i)

      if (line.contains("In file:")) {
        commit()
        // store next build file found
        currentPath = Some(Path.of(line.split(":").last.trim))
      } else if (currentPath.isDefined) {
        line.split("\\s+") match {
          case Array(_, name) if !name.contains("**") => projectNames += name
          case Array(_, "*", name)                    => projectNames += name
          case _                                      =>
        }
      }

      i += 1
    }
    commit()
    b.result()
  }
  class ScalaVersionOutput(
      _scalaVersions: mutable.Map[Versions.Scala, Set[String]],
      _crossVersions: mutable.Map[Versions.Scala, Set[String]]
  ) {
    private val toSorted = SortedMap.empty[Versions.Scala, Set[String]]

    val scalaVersions: SortedMap[Versions.Scala, Set[String]] = toSorted ++ _scalaVersions

    val crossVersions: SortedMap[Versions.Scala, Set[String]] = {
      val filtered = _crossVersions
        // don't repeat what was already in default scala versions
        .map { case (v, projects) => (v, projects -- scalaVersions.getOrElse(v, Set.empty)) }
        .filter { case (_, ps) => ps.nonEmpty }
      toSorted ++ filtered
    }

    def combined: SortedMap[Versions.Scala, Set[String]] = {
      val keys = scalaVersions.keys ++ crossVersions.keys
      toSorted ++ keys.map { key =>
        (key, scalaVersions.getOrElse(key, Set.empty) ++ crossVersions.getOrElse(key, Set.empty))
      }
    }
  }
  object ScalaVersionOutput {
    def parse(lines: List[String]): ScalaVersionOutput = {
      val scalaVersionsBuilder = mutable.Map.empty[Versions.Scala, mutable.Set[String]]
      val crossVersionsBuilder = mutable.Map.empty[Versions.Scala, mutable.Set[String]]

      var i = 0
      while (i < lines.length) {
        val line = lines(i)

        def words = line.split("\\s")

        def projectName = words(words.length - 3) // third last, before `/` and `binaryJVMProjects` above

        if (line.contains("ProjectRef")) ()
        else if (line.endsWith(" / scalaVersion")) {
          val nextLine = lines(i + 1)
          val scalaVersion = Versions.Scala(nextLine.split("\\s").last)
          scalaVersionsBuilder.getOrElseUpdate(scalaVersion, mutable.Set.empty).add(projectName)
        } else if (line.endsWith(" / crossScalaVersions")) {
          val nextLine = lines(i + 1)
          val versions = nextLine.dropWhile(_ != '(').drop(1).takeWhile(_ != ')').split(",").map(_.trim).filterNot(_.isEmpty)
          versions.map { scalaVersion =>
            crossVersionsBuilder.getOrElseUpdate(Versions.Scala(scalaVersion), mutable.Set.empty).add(projectName)
          }
        }

        i += 1
      }

      val scalaVersions = scalaVersionsBuilder.map { case (v, projects) => (v, projects.toSet) }
      val crossVersions = crossVersionsBuilder
        // don't repeat what was already in default scala versions
        .map { case (v, projects) => (v, projects.toSet -- scalaVersions.getOrElse(v, Set.empty)) }
        .filter { case (_, ps) => ps.nonEmpty }

      new ScalaVersionOutput(scalaVersions, crossVersions)
    }
  }
}

// pardon the very imperative interface of the class with indirect flow through files. let's refactor later
case class Import(
    existingBuild: Option[model.Build],
    sbtBuildDir: Path,
    destinationPaths: BuildPaths,
    logger: Logger,
    options: Import.Options,
    bleepVersion: model.Version
) extends BleepCommand {
  override def run(): Either[BuildException, Unit] = {
    if (!options.skipSbt) {
      generateBloopAndDependencyFiles()
    }

    val bloopFiles = findGeneratedJsonFiles(destinationPaths.bleepImportBloopDir).map(GenBloopFiles.readAndParseBloopFile)
    val sbtExportFiles = findGeneratedJsonFiles(destinationPaths.bleepImportSbtExportDir).map { path =>
      val contents = Files.readString(path)
      ReadSbtExportFile.parse(path, contents)
    }
    val inputProjects = ImportInputProjects(bloopFiles, sbtExportFiles)

    val generatedFiles: Map[model.CrossProjectName, Vector[GeneratedFile]] =
      if (options.skipGeneratedResourcesScript) Map.empty else findGeneratedFiles(inputProjects)

    val files = generateBuild(
      inputProjects,
      bleepTasksVersion = model.Version(constants.BleepVersionTemplate),
      generatedFiles,
      existingBuild
    ).map { case (path, content) => (RelPath.relativeTo(destinationPaths.buildDir, path), content) }

    FileUtils.syncStrings(destinationPaths.buildDir, files, deleteUnknowns = FileUtils.DeleteUnknowns.No, soft = false)

    Right(())
  }

  /** I know, launching sbt three plus times is incredibly slow.
    *
    * I'm sure it's possible to do the same thing from within sbt and only launch it first, but you know. it's not at all easy.
    */
  def generateBloopAndDependencyFiles(): Unit = {
    val sbtEnvs = List(
      "SBT_OPTS" -> Some("-Xmx4096M"),
      "JAVA_HOME" -> sys.env.get("JAVA_HOME")
    ).collect { case (k, Some(v)) => (k, v) }

    FileUtils.deleteDirectory(destinationPaths.bleepImportDir)

    logger.info("Will call sbt multiple times to retrieve information about the existing build. Grab some popcorn as it may take a while!")

    // run this as a command to discover all projects, possibly across several builds, possibly including non-aggregated projects
    val allProjectNamesByBuild: Map[Path, List[String]] = {
      val cmd = List("sbt", "projects")
      logger.info("Calling sbt to discover projects...")
      logger.debug(cmd)
      val output = Process(cmd, sbtBuildDir.toFile, sbtEnvs: _*)
        .lazyLines(logger.processLogger("sbt discover projects"))
        .toList

      val result = Import.parseProjectsOutput(output)

      result.foreach { case (buildDir, projects) =>
        logger.info(s"Discovered ${projects.length} in $buildDir")
      }

      result
    }

    allProjectNamesByBuild.foreach { case ( /* shadow*/ sbtBuildDir, projectNames) =>
      val tempAddBloopPlugin = sbtBuildDir / "project" / "bleep-temp-add-bloop-plugin.sbt"

      FileUtils.writeString(
        tempAddBloopPlugin,
        s"""
addSbtPlugin("ch.epfl.scala" % "sbt-bloop" % "1.5.0")
addSbtPlugin("build.bleep" % "sbt-export-dependencies" % "0.2.0")
"""
      )

      try {
        // ask for all (cross) scala versions for these projects
        val scalaVersionOutput: Import.ScalaVersionOutput = {
          val cmd = List("sbt", "show " + projectNames.map(p => s"$p/scalaVersion $p/crossScalaVersions").mkString(" "))
          logger.withContext(sbtBuildDir).info("Calling sbt to discover cross projects...")
          logger.withContext(sbtBuildDir).debug(cmd)
          val output = Process(cmd, sbtBuildDir.toFile, sbtEnvs: _*).lazyLines(logger.processLogger("sbt discover cross projects")).toList

          val result = Import.ScalaVersionOutput.parse(output)

          result.combined
            .foldLeft(logger) { case (logger, (scala, projects)) => logger.withContext(scala.scalaVersion, projects.size) }
            .info("Discovered projects")

          result
        }

        // then finally dump each of the three configurations we care about into two files each.
        val args: Iterable[String] = {
          def argsFor(scalaVersion: Versions.Scala, projects: Set[String], switchScalaVersion: Boolean): List[String] =
            List(
              if (switchScalaVersion) s"++ ${scalaVersion.scalaVersion}" else "",
              s"""set ThisBuild / exportProjectsTo := file("${destinationPaths.bleepImportSbtExportDir}")""",
              s"""set Global / bloopConfigDir := file("${destinationPaths.bleepImportBloopDir / scalaVersion.scalaVersion}")"""
            ) ++ projects.flatMap { p =>
              List(
                s"$p/bloopGenerate",
                s"$p/Test/bloopGenerate",
                // if this configuration is not defined it seems to just return `None`
                s"$p/IntegrationTest/bloopGenerate",
                s"$p/exportProject",
                s"$p/Test/exportProject",
                s"$p/IntegrationTest/exportProject"
              )
            }

          scalaVersionOutput.scalaVersions.flatMap { case (scalaVersion, projects) => argsFor(scalaVersion, projects, switchScalaVersion = false) } ++
            scalaVersionOutput.crossVersions.flatMap { case (scalaVersion, projects) => argsFor(scalaVersion, projects, switchScalaVersion = true) }
        }

        val cmd = "sbt" :: args.toList
        logger.withContext(sbtBuildDir).info("Calling sbt to export cross projects...")
        logger.withContext(sbtBuildDir).debug(cmd)
        cli(cmd, logger, "sbt", env = sbtEnvs)(sbtBuildDir)
      } finally Files.delete(tempAddBloopPlugin)
    }
  }

  def generateBuild(
      inputProjects: ImportInputProjects,
      bleepTasksVersion: model.Version,
      generatedFiles: Map[model.CrossProjectName, Vector[GeneratedFile]],
      maybeExistingBleepJson: Option[model.Build]
  ): Map[Path, String] = {

    val build0 = importBloopFilesFromSbt(logger, sbtBuildDir, destinationPaths, inputProjects, bleepVersion)
    val normalizedBuild = normalizeBuild(build0)

    val build1 = Templates(logger, normalizedBuild, options.ignoreWhenInferringTemplates)

    val build =
      maybeExistingBleepJson match {
        case Some(existingBuild) => build1.copy(scripts = existingBuild.scripts)
        case None                => build1
      }

    // complain if we have done illegal rewrites during templating
    ExplodedBuild.diffProjects(Defaults.add(normalizedBuild), ExplodedBuild.of(build).dropTemplates) match {
      case empty if empty.isEmpty => ()
      case diffs =>
        logger.error("Project templating did illegal rewrites. Please report this as a bug")
        diffs.foreach { case (projectName, msg) => logger.withContext(projectName).error(msg) }
    }

    logger.info(s"Imported ${build0.projects.size} cross targets for ${build.projects.value.size} projects")

    GeneratedFilesScript(generatedFiles) match {
      case Some((className, scriptSource)) =>
        // todo: find a project and use same scala config
        val scalaVersion = normalizedBuild.projects.values.flatMap(_.scala.flatMap(_.version)).maxByOption(_.scalaVersion).orElse(Some(Versions.Scala3))

        val scriptProjectName = model.CrossProjectName(model.ProjectName("scripts"), None)
        val scriptsProject = model.Project(
          `extends` = JsonSet.empty,
          cross = JsonMap.empty,
          folder = None,
          dependsOn = JsonSet.empty,
          `source-layout` = None,
          `sbt-scope` = None,
          sources = JsonSet.empty,
          resources = JsonSet.empty,
          dependencies = JsonSet(Dep.Scala("build.bleep", "bleep-tasks", bleepTasksVersion.value)),
          java = None,
          scala = Some(model.Scala(scalaVersion, Options.empty, None, JsonSet.empty, strict = None)),
          platform = Some(model.Platform.Jvm(Options.empty, None, Options.empty)),
          isTestProject = None,
          testFrameworks = JsonSet.empty
        )

        val buildWithScript = build.copy(
          projects = build.projects.updated(scriptProjectName.name, scriptsProject),
          scripts = build.scripts.updated(model.ScriptName("generate-resources"), JsonList(List(model.ScriptDef(scriptProjectName, className))))
        )

        val scriptPath = destinationPaths.project(scriptProjectName, scriptsProject).dir / "src/scala/scripts/GenerateResources.scala"
        logger
          .withContext(scriptPath)
          .warn("Created a makeshift script to copy generated resources from sbt directories. You'll need to edit this file and make it generate your files")

        Map(
          scriptPath -> scriptSource,
          destinationPaths.bleepYamlFile -> asYamlString(buildWithScript)
        )
      case None =>
        Map(
          destinationPaths.bleepYamlFile -> asYamlString(build)
        )
    }
  }
}
