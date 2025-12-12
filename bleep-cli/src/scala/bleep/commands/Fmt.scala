package bleep
package commands

import bleep.internal.FileUtils

import java.nio.file.{Files, Path}
import scala.jdk.StreamConverters.StreamHasToScala

object Fmt {
  val defaultScalafmtConfig =
    """version=3.5.9
      |maxColumn = 160
      |runner.dialect = scala213
      |""".stripMargin

  def getScalafmtVersion(configStr: String): Option[String] =
    configStr
      .lines()
      .map(_.split("=").map(_.trim))
      .toScala(List)
      .collectFirst { case Array("version", version) => version.stripPrefix("\"").stripSuffix("\"") }

  def findSourceFiles(dirs: Set[Path], extension: String): List[Path] =
    dirs.toList.flatMap { dir =>
      if (Files.exists(dir)) {
        Files.walk(dir).toScala(List).filter { path =>
          Files.isRegularFile(path) && path.toString.endsWith(extension)
        }
      } else Nil
    }
}

case class Fmt(check: Boolean) extends BleepBuildCommand {
  override def run(started: Started): Either[BleepException, Unit] = {
    val sourcesDirs: Set[Path] =
      started.build.explodedProjects.keys
        .flatMap { crossName =>
          val sourcesDirs = started.projectPaths(crossName).sourcesDirs
          sourcesDirs.fromSourceLayout ++ sourcesDirs.fromJson.values
        }
        .toSet
        .filter(Files.exists(_))

    val scalaFiles = Fmt.findSourceFiles(sourcesDirs, ".scala")
    val javaFiles = Fmt.findSourceFiles(sourcesDirs, ".java")

    if (scalaFiles.nonEmpty) {
      formatScala(started, sourcesDirs)
    }

    if (javaFiles.nonEmpty) {
      formatJava(started, javaFiles)
    }

    Right(())
  }

  private def formatScala(started: Started, sourcesDirs: Set[Path]): Unit = {
    val configPath = started.buildPaths.buildDir / ".scalafmt.conf"

    val configStr: String =
      if (FileUtils.exists(configPath)) {
        Files.readString(configPath)
      } else {
        FileUtils.writeString(started.logger, Some("Creating example scalafmt configuration"), configPath, Fmt.defaultScalafmtConfig)
        Fmt.defaultScalafmtConfig
      }

    val version = Fmt
      .getScalafmtVersion(configStr)
      .getOrElse(throw new BleepException.Text(s"Couldn't naively extract scalafmt version from $configPath"))

    val scalafmt = FetchScalafmt(started.pre.cacheLogger, started.executionContext, version)

    started.logger.withContext("scalafmt", scalafmt).debug("Using scalafmt")

    val cmd =
      List(scalafmt.toString, "-c", configPath.toString, "--non-interactive") ++
        (if (check) List("--test") else Nil) ++
        sourcesDirs.map(_.toString)

    cli(
      "scalafmt",
      started.buildPaths.cwd,
      cmd,
      logger = started.logger,
      out = cli.Out.ViaLogger(started.logger)
    ).discard()
  }

  private def formatJava(started: Started, javaFiles: List[Path]): Unit = {
    val googleJavaFormat = FetchGoogleJavaFormat(started.pre.cacheLogger, started.executionContext, FetchGoogleJavaFormat.DefaultVersion)

    started.logger.withContext("google-java-format", googleJavaFormat).debug("Using google-java-format")

    val javaCmd = started.jvmCommand.toString

    val cmd =
      List(javaCmd, "-jar", googleJavaFormat.toString) ++
        (if (check) List("--dry-run", "--set-exit-if-changed") else List("--replace")) ++
        javaFiles.map(_.toString)

    cli(
      "google-java-format",
      started.buildPaths.cwd,
      cmd,
      logger = started.logger,
      out = cli.Out.ViaLogger(started.logger)
    ).discard()
  }
}
