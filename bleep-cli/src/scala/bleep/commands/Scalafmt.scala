package bleep
package commands

import bleep.internal.FileUtils

import java.nio.file.{Files, Path}
import scala.jdk.StreamConverters.StreamHasToScala

object Scalafmt {
  val defaultConfig =
    """version=3.5.9
      |maxColumn = 160
      |runner.dialect = scala213
      |""".stripMargin

  def getVersion(configStr: String): Option[String] =
    configStr
      .lines()
      .map(_.split("=").map(_.trim))
      .toScala(List)
      .collectFirst { case Array("version", version) => version.stripPrefix("\"").stripSuffix("\"") }
}
case class Scalafmt(check: Boolean) extends BleepBuildCommand {
  override def run(started: Started): Either[BleepException, Unit] = {
    val configPath = started.buildPaths.buildDir / ".scalafmt.conf"

    val configStr: String =
      if (FileUtils.exists(configPath)) {
        Files.readString(configPath)
      } else {
        FileUtils.writeString(started.logger, Some("Creating example scalafmt configuration"), configPath, Scalafmt.defaultConfig)
        Scalafmt.defaultConfig
      }

    val version = Scalafmt
      .getVersion(configStr)
      .getOrElse(throw new BleepException.Text(s"Couldn't naively extract scalafmt version from $configPath"))

    val scalafmt = FetchScalafmt(started.pre.cacheLogger, started.executionContext, version)

    started.logger.withContext("scalafmt", scalafmt).debug("Using scalafmt")

    val sourcesDirs: Set[Path] =
      started.build.explodedProjects.keys
        .flatMap { crossName =>
          val sourcesDirs = started.projectPaths(crossName).sourcesDirs
          sourcesDirs.fromSourceLayout ++ sourcesDirs.fromJson.values
        }
        .toSet
        .filter(Files.exists(_))

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
    Right(())
  }
}
