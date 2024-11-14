package bleep
package commands

import bleep.internal.writeYamlLogged
import ryddig.Logger

import java.nio.file.{Files, Path}
import scala.collection.immutable.SortedMap
import scala.sys.process.Process

object commit {
  def apply(logger: Logger, buildPaths: BuildPaths, filesToMove: SortedMap[Path, Path], rewrittenBuild: model.BuildFile): Unit = {
    filesToMove.foreach { case (from, to) =>
      logger.info(s"$from => $to")
      Files.createDirectories(to.getParent)

      (Process(List("git", "mv", from.toString, to.toString), buildPaths.buildDir.toFile) #||
        Process(List("mv", from.toString, to.toString), buildPaths.buildDir.toFile)).!!
    }

    writeYamlLogged(logger, "Wrote update build", rewrittenBuild, buildPaths.bleepYamlFile)
  }

}
