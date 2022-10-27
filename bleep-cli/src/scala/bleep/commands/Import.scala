package bleep
package commands

import bleep.RelPath
import bleep.cli.StdIn
import bleep.commands.Import.findGeneratedJsonFiles
import bleep.internal._
import bleep.logging.Logger
import bleep.rewrites.{normalizeBuild, Defaults}
import bleep.templates.templatesInfer
import cats.syntax.apply._
import com.monovore.decline.Opts

import java.nio.file.{Files, Path}
import scala.collection.immutable.SortedMap
import scala.collection.mutable
import scala.jdk.CollectionConverters._

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

  def parseProjectsOutput(lines: Array[String]): Map[Path, List[String]] = {
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
      _scalaVersions: mutable.Map[model.VersionScala, Set[String]],
      _crossVersions: mutable.Map[model.VersionScala, Set[String]]
  ) {
    private val toSorted = SortedMap.empty[model.VersionScala, Set[String]]

    val scalaVersions: SortedMap[model.VersionScala, Set[String]] = toSorted ++ _scalaVersions

    val crossVersions: SortedMap[model.VersionScala, Set[String]] = {
      val filtered = _crossVersions
        // don't repeat what was already in default scala versions
        .map { case (v, projects) => (v, projects -- scalaVersions.getOrElse(v, Set.empty)) }
        .filter { case (_, ps) => ps.nonEmpty }
      toSorted ++ filtered
    }

    def combined: SortedMap[model.VersionScala, Set[String]] = {
      val keys = scalaVersions.keys ++ crossVersions.keys
      toSorted ++ keys.map { key =>
        (key, scalaVersions.getOrElse(key, Set.empty) ++ crossVersions.getOrElse(key, Set.empty))
      }
    }
  }
  object ScalaVersionOutput {
    def parse(lines: Array[String], onlyOneProject: Option[String]): ScalaVersionOutput = {
      val scalaVersionsBuilder = mutable.Map.empty[model.VersionScala, mutable.Set[String]]
      val crossVersionsBuilder = mutable.Map.empty[model.VersionScala, mutable.Set[String]]

      var i = 0
      while (i < lines.length) {
        val line = lines(i)

        def handleScalaVersion(projectName: String) = {
          i = i + 1
          val nextLine = lines(i)
          val scalaVersion = model.VersionScala(nextLine.split("\\s").last)
          scalaVersionsBuilder.getOrElseUpdate(scalaVersion, mutable.Set.empty).add(projectName)
        }

        def handleCrossScalaVersions(projectName: String) = {
          i = i + 1
          val nextLine = lines(i)
          val versions = nextLine.dropWhile(_ != '(').drop(1).takeWhile(_ != ')').split(",").map(_.trim).filterNot(_.isEmpty)
          versions.map { scalaVersion =>
            crossVersionsBuilder.getOrElseUpdate(model.VersionScala(scalaVersion), mutable.Set.empty).add(projectName)
          }
        }

        (line.split("\\s").toList, onlyOneProject) match {
          // short form, if there is only one project
          case (List(_, "scalaVersion"), Some(projectName)) =>
            handleScalaVersion(projectName)
          // normal form
          case (List(_, projectName, "/", "scalaVersion"), None) =>
            handleScalaVersion(projectName)
          // short form
          case (List(_, "crossScalaVersions"), Some(projectName)) =>
            handleCrossScalaVersions(projectName)
          // normal form
          case (List(_, projectName, "/", "crossScalaVersions"), None) =>
            handleCrossScalaVersions(projectName)
          case other => println(other)
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
    existingBuild: Option[model.BuildFile],
    sbtBuildDir: Path,
    destinationPaths: BuildPaths,
    logger: Logger,
    options: Import.Options,
    bleepVersion: model.BleepVersion
) extends BleepCommand {
  override def run(): Either[BleepException, Unit] = {
    if (!options.skipSbt) {
      generateBloopAndDependencyFiles()
    }

    val bloopFiles = findGeneratedJsonFiles(destinationPaths.bleepImportBloopDir).map(GenBloopFiles.readAndParseBloopFile)
    val sbtExportFiles = findGeneratedJsonFiles(destinationPaths.bleepImportSbtExportDir).map { path =>
      val contents = Files.readString(path)
      ReadSbtExportFile.parse(path, contents)
    }
    val inputProjects = ImportInputProjects(bloopFiles, sbtExportFiles, forceInclude = Set.empty)

    val generatedFiles: Map[model.CrossProjectName, Vector[GeneratedFile]] =
      if (options.skipGeneratedResourcesScript) Map.empty else findGeneratedFiles(inputProjects)

    val files = generateBuild(
      inputProjects,
      bleepTasksVersion = model.BleepVersion(model.Replacements.known.BleepVersion),
      generatedFiles,
      existingBuild
    ).map { case (path, content) => (RelPath.relativeTo(destinationPaths.buildDir, path), content) }

    FileSync.syncStrings(destinationPaths.buildDir, files, deleteUnknowns = FileSync.DeleteUnknowns.No, soft = false)

    Right(())
  }

  def sbtCommands(cmds: Iterable[String]) =
    StdIn.Provided(cmds.mkString("", "\n", "\nexit\n").getBytes)

  /** I know, launching sbt three plus times is incredibly slow.
    *
    * I'm sure it's possible to do the same thing from within sbt and only launch it first, but you know. it's not at all easy.
    */
  def generateBloopAndDependencyFiles(): Unit = {
    val sbtEnvs = List(
      "SBT_OPTS" -> Some("-Xmx4096M"),
      "JAVA_HOME" -> sys.env.get("JAVA_HOME")
    ).collect { case (k, Some(v)) => (k, v) }

    val sbt =
      List("sbt") ++ sys.env.get("JAVA_HOME").toList.flatMap(home => List("-java-home", home))

    FileUtils.deleteDirectory(destinationPaths.bleepImportDir)

    logger.info("Will call sbt multiple times to retrieve information about the existing build. Grab some popcorn as it may take a while!")

    // run this as a command to discover all projects, possibly across several builds, possibly including non-aggregated projects
    val allProjectNamesByBuild: Map[Path, List[String]] = {
      logger.info("Calling sbt to discover projects...")

      val output = cli(
        action = "sbt discover projects",
        cwd = sbtBuildDir,
        cmd = sbt,
        logger = logger,
        env = sbtEnvs,
        stdIn = sbtCommands(List("projects"))
      )

      val result = Import.parseProjectsOutput(output.stdout)

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
addSbtPlugin("ch.epfl.scala" % "sbt-bloop" % "1.5.3")
addSbtPlugin("build.bleep" % "sbt-export-dependencies" % "0.2.0")
"""
      )

      try {
        // ask for all (cross) scala versions for these projects
        val scalaVersionOutput: Import.ScalaVersionOutput = {
          logger.withContext(sbtBuildDir).info("Calling sbt to discover cross projects...")
          val cmds = projectNames.map(p => s"show $p/scalaVersion $p/crossScalaVersions")
          logger.withContext(sbtBuildDir).debug(cmds)

          val output =
            cli(
              "sbt discover cross projects",
              sbtBuildDir,
              sbt,
              logger,
              env = sbtEnvs,
              stdIn = sbtCommands(cmds)
            )
          val onlyOneProject = projectNames match {
            case one :: Nil => Some(one)
            case _          => None
          }
          val result = Import.ScalaVersionOutput.parse(output.stdout, onlyOneProject)

          result.combined
            .foldLeft(logger) { case (logger, (scala, projects)) => logger.withContext(scala.scalaVersion, projects.size) }
            .info("Discovered projects")

          result
        }

        // then finally dump each of the three configurations we care about into two files each.
        val cmds: Iterable[String] = {
          def argsFor(scalaVersion: model.VersionScala, projects: Set[String], switchScalaVersion: Boolean): List[String] =
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

        logger.withContext(sbtBuildDir).info("Calling sbt to export cross projects...")
        logger.withContext(sbtBuildDir).debug(cmds)
        cli(
          action = "sbt export",
          cwd = sbtBuildDir,
          cmd = sbt,
          logger = logger,
          stdIn = sbtCommands(cmds),
          env = sbtEnvs
        )
      } finally Files.delete(tempAddBloopPlugin)
    }
  }

  def generateBuild(
      inputProjects: ImportInputProjects,
      bleepTasksVersion: model.BleepVersion,
      generatedFiles: Map[model.CrossProjectName, Vector[GeneratedFile]],
      maybeExistingBuildFile: Option[model.BuildFile]
  ): Map[Path, String] = {

    val build0 = importBloopFilesFromSbt(logger, sbtBuildDir, destinationPaths, inputProjects, bleepVersion)
    val normalizedBuild = normalizeBuild(build0)

    val buildFile = templatesInfer(new BleepTemplateLogger(logger), normalizedBuild, options.ignoreWhenInferringTemplates)

    val buildFile1 =
      maybeExistingBuildFile match {
        case Some(existingBuild) => buildFile.copy(scripts = existingBuild.scripts)
        case None                => buildFile
      }

    // complain if we have done illegal rewrites during templating
    model.Build.diffProjects(Defaults.add(normalizedBuild), model.Build.FileBacked(buildFile1).dropBuildFile.dropTemplates) match {
      case empty if empty.isEmpty => ()
      case diffs =>
        logger.error("Project templating did illegal rewrites. Please report this as a bug")
        diffs.foreach { case (projectName, msg) => logger.withContext(projectName).error(msg) }
    }

    logger.info(s"Imported ${build0.explodedProjects.size} cross targets for ${buildFile1.projects.value.size} projects")

    GeneratedFilesScript(generatedFiles) match {
      case Some((className, scriptSource)) =>
        // todo: find a project and use same scala config
        val scalaVersion =
          normalizedBuild.explodedProjects.values
            .flatMap(_.scala.flatMap(_.version))
            .maxByOption(_.scalaVersion)
            // avoid picking scala 3 versions lower than what is used to compile the bleep artifacts
            .filter {
              case x if x.is3 && x.scalaVersion < model.VersionScala.Scala3.scalaVersion => false
              case _                                                                     => true
            }
            .orElse(Some(model.VersionScala.Scala3))

        val scriptProjectName = model.CrossProjectName(model.ProjectName("scripts"), None)
        val scriptsProject = model.Project(
          `extends` = model.JsonSet.empty,
          cross = model.JsonMap.empty,
          folder = None,
          dependsOn = model.JsonSet.empty,
          `source-layout` = None,
          `sbt-scope` = None,
          sources = model.JsonSet.empty,
          resources = model.JsonSet.empty,
          dependencies = model.JsonSet(model.Dep.Scala("build.bleep", "bleep-tasks", bleepTasksVersion.value)),
          java = None,
          scala = Some(model.Scala(scalaVersion, model.Options.empty, None, model.JsonSet.empty, strict = None)),
          platform = Some(model.Platform.Jvm(model.Options.empty, None, model.Options.empty)),
          isTestProject = None,
          testFrameworks = model.JsonSet.empty
        )

        val buildWithScript = buildFile1.copy(
          projects = buildFile1.projects.updated(scriptProjectName.name, scriptsProject),
          scripts = buildFile1.scripts.updated(model.ScriptName("generate-resources"), model.JsonList(List(model.ScriptDef(scriptProjectName, className))))
        )

        val scriptPath = destinationPaths.project(scriptProjectName, scriptsProject).dir / "src/scala/scripts/GenerateResources.scala"
        logger
          .withContext(scriptPath)
          .warn("Created a makeshift script to copy generated resources from sbt directories. You'll need to edit this file and make it generate your files")

        Map(
          scriptPath -> scriptSource,
          destinationPaths.bleepYamlFile -> yaml.encodeShortened(buildWithScript)
        )
      case None =>
        Map(
          destinationPaths.bleepYamlFile -> yaml.encodeShortened(buildFile1)
        )
    }
  }
}
