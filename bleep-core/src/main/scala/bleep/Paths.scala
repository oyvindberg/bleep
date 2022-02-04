package bleep

import coursier.cache.shaded.dirs.dev.dirs.ProjectDirectories

import java.nio.file.{Path, Paths}

case class UserPaths(cacheDir: Path, configDir: Path) {
  val bspSocketDir = cacheDir / "bsp-socket"
  val coursierRepositoriesJson = configDir / "coursier-repositories.json"
}

object UserPaths {
  def fromAppDirs: UserPaths = {
    val dirs = ProjectDirectories.from("no", "arktekk", "bleep")
    val cacheDir = Paths.get(dirs.cacheDir)
    val configDir = Paths.get(dirs.configDir)

    UserPaths(cacheDir, configDir)
  }
}

case class BuildPaths(bleepJsonFile: Path) {
  val buildDir = bleepJsonFile.getParent
  val dotBleepDir = buildDir / ".bleep"
  val bleepImportDir = dotBleepDir / "import"
  val bleepBloopDir = dotBleepDir / ".bloop"
  val digestFile = bleepBloopDir / ".digest"
  val bspBleepJsonFile = buildDir / ".bsp" / "bleep.json"

  def from(crossName: model.CrossProjectName, p: model.Project): ProjectPaths =
    ProjectPaths(
      dir = buildDir / p.folder.getOrElse(RelPath.force(crossName.name.value)),
      targetDir = bleepBloopDir / crossName.name.value / crossName.crossId.fold("")(_.value)
    )
}

case class ProjectPaths(dir: Path, targetDir: Path) {
  val classes: Path =
    targetDir / "classes"

  val incrementalAnalysis: Path =
    targetDir / s"inc_compile.zip"
}
