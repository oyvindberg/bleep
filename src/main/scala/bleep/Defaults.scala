package bleep

import java.nio.file.Path

object Defaults {
  val BuildFileName = "bleep.json"

  val BloopFolder = ".bloop"

  def sourceDirs(projectFolder: Path, scalaVersion: Versions.Scala, scope: String): List[Path] =
    List(
      projectFolder / s"src/$scope/scala",
      projectFolder / s"src/$scope/scala-${scalaVersion.binVersion}",
      projectFolder / s"src/$scope/scala-${scalaVersion.epoch}",
      projectFolder / s"src/$scope/java",
      projectFolder / s"target/scala-${scalaVersion.binVersion}/src_managed/$scope"
    )

  def resourceDirs(projectFolder: Path, scalaVersion: Versions.Scala, scope: String): List[Path] =
    List(
      projectFolder / s"src/$scope/resources",
      projectFolder / s"target/scala-${scalaVersion.binVersion}/resource_managed/$scope"
    )
}
