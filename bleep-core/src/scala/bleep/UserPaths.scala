package bleep

import bleep.internal.JniGetWinDirs
import coursier.cache.shaded.dirs.{GetWinDirs, ProjectDirectories}
import coursier.jniutils.WindowsKnownFolders

import java.nio.file.Path
import java.util

case class UserPaths(cacheDir: Path, configDir: Path) {
  val bspSocketDir = cacheDir / "socket"
  val resolveCacheDir = cacheDir / "coursier-v2"
  val resolveJvmCacheDir = cacheDir / "coursier-jvms"
  val configYaml = configDir / "config.yaml"
}

object UserPaths {
  def fromAppDirs: UserPaths = {
    val getWinDirs =
      if (coursier.paths.Util.useJni()) new JniGetWinDirs
      else GetWinDirs.powerShellBased

    val dirs = ProjectDirectories.from("build", null, "bleep", getWinDirs)
    val cacheDir = Path.of(dirs.cacheDir)
    val configDir = Path.of(dirs.configDir)

    UserPaths(cacheDir, configDir)
  }
}
