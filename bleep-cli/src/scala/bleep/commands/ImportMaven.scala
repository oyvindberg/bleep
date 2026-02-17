package bleep
package commands

import ryddig.Logger

import java.nio.file.Path

case class ImportMaven(
    mavenBuildDir: Path,
    destinationPaths: BuildPaths,
    logger: Logger,
    options: mavenimport.MavenImportOptions,
    bleepVersion: model.BleepVersion
) extends BleepCommand {
  override def run(): Either[BleepException, Unit] = {
    if (!options.skipMvn) {
      mavenimport.runMaven(logger, mavenBuildDir, destinationPaths, options.mvnPath)
    }

    val effectivePomPath = destinationPaths.bleepImportMavenDir / "effective-pom.xml"
    val mavenProjects = mavenimport.parsePom(effectivePomPath)

    logger.info(s"Parsed ${mavenProjects.size} Maven module(s)")

    val generatedBuildFiles = mavenimport
      .generateBuildFromMaven(
        destinationPaths = destinationPaths,
        logger = logger,
        options = options,
        bleepVersion = bleepVersion,
        bleepTasksVersion = model.BleepVersion(model.Replacements.known.BleepVersion),
        mavenProjects = mavenProjects
      )
      .map { case (path, content) => (RelPath.relativeTo(destinationPaths.buildDir, path), content) }

    FileSync
      .syncStrings(destinationPaths.buildDir, generatedBuildFiles, deleteUnknowns = FileSync.DeleteUnknowns.No, soft = false)
      .log(logger, "Wrote build files")

    Right(())
  }
}
