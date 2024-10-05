package bleep
package commands

import ryddig.Logger

import java.nio.file.Path

case class Import(
    existingBuild: Option[model.BuildFile],
    sbtBuildDir: Path,
    fetchJvm: FetchJvm,
    destinationPaths: BuildPaths,
    logger: Logger,
    options: sbtimport.ImportOptions,
    bleepVersion: model.BleepVersion
) extends BleepCommand {
  override def run(): Either[BleepException, Unit] = {
    if (!options.skipSbt) {
      val resolvedJvm = fetchJvm(model.Jvm.system)
      sbtimport.runSbt(logger, sbtBuildDir, destinationPaths, resolvedJvm, options.sbtPath)
    }

    val inputData = sbtimport.ImportInputData.collectFromFileSystem(destinationPaths, logger)

    val generatedBuildFiles = sbtimport
      .generateBuild(
        sbtBuildDir = sbtBuildDir,
        destinationPaths = destinationPaths,
        logger = logger,
        options = options,
        bleepVersion = bleepVersion,
        inputData = inputData,
        bleepTasksVersion = model.BleepVersion(model.Replacements.known.BleepVersion),
        maybeExistingBuildFile = existingBuild
      )
      .map { case (path, content) => (RelPath.relativeTo(destinationPaths.buildDir, path), content) }

    FileSync
      .syncStrings(destinationPaths.buildDir, generatedBuildFiles, deleteUnknowns = FileSync.DeleteUnknowns.No, soft = false)
      .log(logger, "Wrote build files")

    Right(())
  }
}
