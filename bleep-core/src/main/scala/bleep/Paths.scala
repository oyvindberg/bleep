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

trait BuildPaths {
  val bleepJsonFile: Path
  val buildDir: Path
  val dotBleepDir: Path
  val bleepImportDir: Path
  val bleepBloopDir: Path
  val digestFile: Path
  val bspBleepJsonFile: Path

  final def from(crossName: model.CrossProjectName, p: model.Project): ProjectPaths =
    ProjectPaths(
      dir = buildDir / p.folder.getOrElse(RelPath.force(crossName.name.value)),
      targetDir = bleepBloopDir / crossName.name.value / crossName.crossId.fold("")(_.value)
    )
}

object BuildPaths {
  def fromBleepJson(bleepJsonFile: Path): BuildPaths =
    fromBuildDir(bleepJsonFile.getParent)

  def fromBuildDir(_buildDir: Path): BuildPaths = new BuildPaths {
    override val bleepJsonFile: Path = _buildDir / Defaults.BuildFileName
    override val buildDir: Path = _buildDir
    override val dotBleepDir = buildDir / ".bleep"
    override val bleepImportDir = dotBleepDir / "import"
    override val bleepBloopDir = dotBleepDir / ".bloop"
    override val digestFile = bleepBloopDir / ".digest"
    override val bspBleepJsonFile = buildDir / ".bsp" / "bleep.json"
  }
}

case class ProjectPaths(dir: Path, targetDir: Path) {
  val classes: Path =
    targetDir / "classes"

  val incrementalAnalysis: Path =
    targetDir / s"inc_compile.zip"
}
