package bleep
package mavenimport

import ryddig.Logger

import java.nio.file.{Files, Path}

object runMaven {
  def apply(
      logger: Logger,
      mavenBuildDir: Path,
      destinationPaths: BuildPaths,
      mvnPath: Option[String]
  ): Unit = {
    val mvn = mvnPath.getOrElse("mvn")
    val outputPath = destinationPaths.bleepImportMavenDir.resolve("effective-pom.xml")

    Files.createDirectories(outputPath.getParent)

    logger.info("Running Maven to extract effective POM...")

    cli(
      action = "mvn effective-pom",
      cwd = mavenBuildDir,
      cmd = List(mvn, "help:effective-pom", s"-Doutput=$outputPath"),
      logger = logger,
      out = cli.Out.ViaLogger(logger)
    ).discard()

    logger.info(s"Effective POM written to $outputPath")
  }
}
