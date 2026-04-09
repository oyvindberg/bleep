package bleep

import bleep.rewrites.BuildRewrite

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

abstract class BleepCodegenScript(val scriptName: String) {
  // can override this in subclass
  val rewrites: List[BuildRewrite] = Nil

  val ThisClassName = getClass.getName.split('$').head

  case class Target(project: model.CrossProjectName, sources: Path, resources: Path)

  def main(args: Array[String]): Unit = {
    val (preOpts, restArgs) = PreBootstrapOpts.parse(args.toList)
    val (codegenOpts, restArgs2) = CodegenOpts.parse(restArgs)

    bootstrap.forScript(scriptName, preOpts.toLoggingOpts, rewrites) { (started, commands) =>
      val dotBleepDir = started.buildPaths.dotBleepDir

      // Compute real and temp targets for each project
      val realAndTempTargets = codegenOpts.projectNames.map { projectName =>
        val realSources = started.buildPaths.generatedSourcesDir(projectName, ThisClassName)
        val realResources = started.buildPaths.generatedResourcesDir(projectName, ThisClassName)
        val tempSources = dotBleepDir / "generated-sources-tmp" / projectName.value / ThisClassName
        val tempResources = dotBleepDir / "generated-resources-tmp" / projectName.value / ThisClassName
        (projectName, realSources, realResources, tempSources, tempResources)
      }

      // Pass temp targets to script so it writes there instead of real dirs
      val tempTargets = realAndTempTargets.map { case (projectName, _, _, tempSources, tempResources) =>
        Target(projectName, tempSources, tempResources)
      }

      try {
        run(started, commands, tempTargets, restArgs2)

        // Sync temp -> real for each target, preserving timestamps of unchanged files
        realAndTempTargets.foreach { case (_, realSources, realResources, tempSources, tempResources) =>
          syncDir(tempSources, realSources)
          syncDir(tempResources, realResources)

          // Write stamp file so staleness detection sees a fresh timestamp
          Files.createDirectories(realSources)
          Files.writeString(realSources.resolve(".sourcegen-stamp"), "")
        }
      } finally
        // Clean up temp dirs
        realAndTempTargets.foreach { case (_, _, _, tempSources, tempResources) =>
          deleteIfExists(tempSources)
          deleteIfExists(tempResources)
        }
    }
  }

  private def readDirFiles(dir: Path): Map[RelPath, Array[Byte]] =
    if (Files.isDirectory(dir)) {
      val stream = Files.walk(dir)
      try
        stream
          .iterator()
          .asScala
          .filter(Files.isRegularFile(_))
          .map { file =>
            RelPath.relativeTo(dir, file) -> Files.readAllBytes(file)
          }
          .toMap
      finally
        stream.close()
    } else {
      Map.empty
    }

  private def syncDir(tempDir: Path, realDir: Path): Unit = {
    val fileMap = readDirFiles(tempDir)
    FileSync.syncBytes(realDir, fileMap, FileSync.DeleteUnknowns.Yes(None), soft = true): Unit
  }

  private def deleteIfExists(dir: Path): Unit =
    if (Files.isDirectory(dir)) {
      val stream = Files.walk(dir)
      try
        stream.sorted(java.util.Comparator.reverseOrder[Path]()).forEach(p => Files.deleteIfExists(p): Unit)
      finally
        stream.close()
    }

  def run(started: Started, commands: Commands, targets: List[Target], args: List[String]): Unit
}
