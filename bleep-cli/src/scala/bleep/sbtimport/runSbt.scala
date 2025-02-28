package bleep
package sbtimport

import bleep.internal.FileUtils
import ryddig.Logger

import java.nio.file.{Files, Path}
import scala.collection.immutable.SortedMap
import scala.collection.mutable
import scala.concurrent.ExecutionContext
import scala.util.Try

object runSbt {

  /** I know, launching sbt three plus times is incredibly slow.
    *
    * I'm sure it's possible to do the same thing from within sbt and only launch it first, but you know. it's not at all easy.
    */
  def apply(logger: Logger, sbtBuildDir: Path, destinationPaths: BuildPaths, jvm: ResolvedJvm, providedSbtPath: Option[String]): Unit = {
    val version = readSbtVersionFromFile(sbtBuildDir).getOrElse("1.8.0")
    val sbtPath = providedSbtPath.getOrElse {
      val fetchSbt = new FetchSbt(new BleepCacheLogger(logger), ExecutionContext.global)
      fetchSbt(version)
    }
    def sbtCommands(cmds: Iterable[String]) =
      cli.In.Provided(cmds.mkString("", "\n", "\nexit\n").getBytes)

    val javaHome = jvm.javaBin.getParent.getParent

    val sbtEnvs = List(
      "SBT_OPTS" -> "-Xmx4096M",
      "JAVA_HOME" -> javaHome.toString
    )
    val sbt = List(sbtPath.toString) ++ List("-java-home", javaHome.toString)

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
        out = cli.Out.ViaLogger(logger),
        in = sbtCommands(List("projects")),
        env = sbtEnvs
      )

      val result = parseProjectsOutput(output.stdout)

      result.foreach { case (buildDir, projects) =>
        logger.info(s"Discovered ${projects.length} in $buildDir")
      }

      result
    }

    allProjectNamesByBuild.foreach { case ( /* shadow*/ sbtBuildDir, projectNames) =>
      // ask for all (cross) scala versions for these projects
      val scalaVersionOutput: ScalaVersionOutput = {
        logger.withContext("sbtBuildDir", sbtBuildDir).info("Calling sbt to discover cross projects...")
        val cmds = projectNames.map(p => s"show $p/scalaVersion $p/crossScalaVersions")
        logger.withContext("sbtBuildDir", sbtBuildDir).debug(cmds)

        val output =
          cli(
            "sbt discover cross projects",
            sbtBuildDir,
            sbt,
            logger = logger,
            out = cli.Out.ViaLogger(logger),
            in = sbtCommands(cmds),
            env = sbtEnvs
          )
        val onlyOneProject = projectNames match {
          case one :: Nil => Some(one)
          case _          => None
        }
        val result = ScalaVersionOutput.parse(output.stdout, onlyOneProject)

        result.combined
          .foldLeft(logger) { case (logger, (scala, projects)) => logger.withContext(scala.scalaVersion, projects.size) }
          .info("Discovered projects")

        result
      }

      val tempAddBloopPlugin = sbtBuildDir / "project" / "bleep-temp-add-bloop-plugin.sbt"

      FileUtils.writeString(
        logger,
        None,
        tempAddBloopPlugin,
        s"""
  addSbtPlugin("ch.epfl.scala" % "sbt-bloop" % "1.5.6")
  addSbtPlugin("build.bleep" % "sbt-export-dependencies" % "0.3.0")
  """
      )

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

      logger.withContext("sbtBuildDir", sbtBuildDir).info("Calling sbt to export cross projects...")
      logger.withContext("sbtBuildDir", sbtBuildDir).debug(cmds)

      try
        cli(
          action = "sbt export",
          cwd = sbtBuildDir,
          cmd = sbt,
          logger = logger,
          out = cli.Out.ViaLogger(logger),
          in = sbtCommands(cmds),
          env = sbtEnvs
        )
      finally Files.delete(tempAddBloopPlugin)
    }
  }

  def readSbtVersionFromFile(sbtBuildDir: Path): Option[String] =
    Try {
      val p = sbtBuildDir / "project" / "build.properties"
      val contents = Files.readString(p)
      contents.linesIterator
        .map(_.split("=").map(_.trim))
        .collectFirst { case Array("sbt.version", version) =>
          version
        }
    }.toOption.flatten

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

        def handleScalaVersion(projectName: String): Unit = {
          i = i + 1
          val nextLine = lines(i)
          val scalaVersion = model.VersionScala(nextLine.split("\\s").last)
          scalaVersionsBuilder.getOrElseUpdate(scalaVersion, mutable.Set.empty).add(projectName).discard()
        }

        def handleCrossScalaVersions(projectName: String): Unit = {
          i = i + 1
          val nextLine = lines(i)
          val versions = nextLine.dropWhile(_ != '(').drop(1).takeWhile(_ != ')').split(",").map(_.trim).filterNot(_.isEmpty)
          versions.foreach { scalaVersion =>
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
          case _ => ()
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
