package bleep
package commands

import bleep.cli.CliLogger
import bleep.internal.FileUtils

import java.nio.file.Files
import scala.jdk.StreamConverters.StreamHasToScala

object Scalafmt {
  val defaultConfig =
    """version=3.5.9
      |maxColumn = 160
      |runner.dialect = scala213
      |""".stripMargin
}
class Scalafmt(started: Started, check: Boolean) extends BleepCommand {
  override def run(): Either[BleepException, Unit] = {
    val configPath = started.buildPaths.buildDir / ".scalafmt.conf"

    val configStr: String =
      if (FileUtils.exists(configPath)) {
        Files.readString(configPath)
      } else {
        started.logger.warn(s"Creating example scalafmt configuration at $configPath")
        FileUtils.writeString(configPath, Scalafmt.defaultConfig)
        Scalafmt.defaultConfig
      }

    val version =
      configStr
        .lines()
        .map(_.trim.split("="))
        .toScala(List)
        .collectFirst { case Array("version", version) => version }
        .getOrElse(throw new BleepException.Text(s"Couldn't naively extract scalafmt version from $configPath"))

    val path = FetchScalafmt(new BleepCacheLogger(started.logger), started.executionContext, version)

    started.logger.withContext(path).debug("Using scalafmt")

    cli(
      "scalafmt",
      started.buildPaths.cwd,
      List(path.toString, "-c", ".scalafmt.conf") ++ (if (check) List("--check") else Nil),
      CliLogger(started.logger)
    )
    Right(())
  }
}
