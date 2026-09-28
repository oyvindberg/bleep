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

    // what maven resolves for every module, so the import can tell which versions the build manages are used where. maven resolves a module another module
    // depends on from the reactor only once it has been built in the same session, hence `package`
    logger.info("Running Maven to list what every module resolves...")
    val listed = cli(
      action = "mvn dependency:list",
      cwd = mavenBuildDir,
      cmd = List(mvn, "-B", "-am", "-DskipTests", "package", "dependency:list"),
      logger = logger,
      out = cli.Out.ViaLogger(logger)
    )
    val listPath = destinationPaths.bleepImportMavenDir.resolve("dependency-list.txt")
    Files.writeString(listPath, listed.stdout.mkString("\n"))
    logger.info(s"Dependency list written to $listPath")
  }
}
