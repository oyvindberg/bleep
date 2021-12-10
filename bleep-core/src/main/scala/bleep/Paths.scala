package bleep

import coursier.cache.shaded.dirs.dev.dirs.ProjectDirectories

import java.nio.file.{Path, Paths}

case class UserPaths(cacheDir: Path, configDir: Path) {
  val bspSocketDir = cacheDir / "bsp-socket"
  val coursierRepositoriesJson = configDir / "coursier-repositories.json"
}

object UserPaths {
  def fromAppDirs: UserPaths = {
    val dirs = ProjectDirectories.from("bleep", "1", "com.olvind")
    val cacheDir = Paths.get(dirs.cacheDir)
    val configDir = Paths.get(dirs.configDir)

    UserPaths(cacheDir, configDir)
  }
}

case class BuildPaths(bleepJsonFile: Path) {
  val buildDir = bleepJsonFile.getParent
  val dotBloopDir = buildDir / ".bloop"
  val dotBleepDir = buildDir / ".bleep"
  val digestFile = dotBleepDir / ".digest"
  val bleepBloopDir = dotBleepDir / ".bloop"
  val dotBspDir = buildDir / ".bsp"
  val bspBleepJsonFile = dotBspDir / "bleep.json"

  def from(name: model.ProjectName, p: model.Project): ProjectPaths =
    ProjectPaths(
      dir = buildDir / p.folder.getOrElse(RelPath.force(name.value)),
      targetDir = bleepBloopDir / name.value
    )
}

case class ProjectPaths(dir: Path, targetDir: Path) {
  def classes(scalaVersion: Option[Versions.Scala], isTest: Boolean): Path = {
    val classes = if (isTest) "test-classes" else "classes"
    scalaVersion match {
      case Some(scalaVersion) => targetDir / s"scala-${scalaVersion.binVersion}" / classes
      case None               => targetDir / classes
    }
  }

  def incrementalAnalysis(scalaVersion: Versions.Scala): Path =
    dir / "target" / "streams" / "compile" / "bloopAnalysisOut" / "_global" / "streams" / s"inc_compile_${scalaVersion.binVersion}.zip"
}
