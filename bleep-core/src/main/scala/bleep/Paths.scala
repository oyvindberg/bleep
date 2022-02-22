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
  val cwd: Path
  val buildDir: Path

  lazy val bleepJsonFile: Path = buildDir / constants.BuildFileName
  lazy val bspBleepJsonFile: Path = buildDir / ".bsp" / "bleep.json"
  lazy val dotBleepDir: Path = buildDir / ".bleep"
  lazy val bleepImportDir: Path = dotBleepDir / "import"
  lazy val dotBleepBspModeDir: Path = dotBleepDir / "bsp"
  def dotBleepModeDir: Path
  lazy val bleepBloopDir: Path = dotBleepModeDir / ".bloop"
  lazy val digestFile: Path = bleepBloopDir / ".digest"
  lazy val logFile: Path = dotBleepModeDir / "last.log"
  lazy val bspProjectSelectionJsonFile: Path = dotBleepBspModeDir / "project-selection.json"

  final def from(crossName: model.CrossProjectName, p: model.Project): ProjectPaths =
    ProjectPaths(
      dir = buildDir / p.folder.getOrElse(RelPath.force(crossName.name.value)),
      targetDir = bleepBloopDir / crossName.name.value / crossName.crossId.fold("")(_.value)
    )
}

object BuildPaths {
  sealed trait Mode
  object Mode {
    case object Normal extends Mode
    // put bloop files into a different directory for IDE use, since we add flags and select a subset of projects
    case object BSP extends Mode
  }

  def fromBleepJson(cwd: Path, bleepJsonFile: Path, mode: Mode): BuildPaths =
    fromBuildDir(cwd, bleepJsonFile.getParent, mode)

  def fromBuildDir(_cwd: Path, _buildDir: Path, mode: Mode): BuildPaths = new BuildPaths {
    override val cwd = _cwd
    override val buildDir = _buildDir

    override lazy val dotBleepModeDir: Path = mode match {
      case Mode.Normal => dotBleepDir
      case Mode.BSP    => dotBleepBspModeDir
    }
  }
}

case class ProjectPaths(dir: Path, targetDir: Path) {
  val classes: Path =
    targetDir / "classes"

  val incrementalAnalysis: Path =
    targetDir / s"inc_compile.zip"
}
