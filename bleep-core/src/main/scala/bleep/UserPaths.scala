package bleep

import coursier.cache.shaded.dirs.ProjectDirectories

import java.nio.file.Path

case class UserPaths(cacheDir: Path, configDir: Path) {
  val bspSocketDir = cacheDir / "bsp-socket"
  val coursierCacheDir = cacheDir / "coursier"
  val configJson = configDir / "config.json"
}

object UserPaths {
  def fromAppDirs: UserPaths = {
    val dirs = ProjectDirectories.from("no", "arktekk", "bleep")
    val cacheDir = Path.of(dirs.cacheDir)
    val configDir = Path.of(dirs.configDir)

    UserPaths(cacheDir, configDir)
  }
}
