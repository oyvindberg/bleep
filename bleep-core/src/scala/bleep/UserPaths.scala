package bleep

import coursier.cache.shaded.dirs.ProjectDirectories

import java.nio.file.Path

case class UserPaths(cacheDir: Path, configDir: Path) {
  val bspSocketDir = cacheDir / "bsp-socket"
  val resolveCachePath = cacheDir / "coursier"
  val configYaml = configDir / "config.yaml"
}

object UserPaths {
  def fromAppDirs: UserPaths = {
    val dirs = ProjectDirectories.from("build", "bleep", "bleep")
    val cacheDir = Path.of(dirs.cacheDir)
    val configDir = Path.of(dirs.configDir)

    UserPaths(cacheDir, configDir)
  }
}
