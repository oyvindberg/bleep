package bleep
package internal

import net.harawata.appdirs.{AppDirs, AppDirsFactory}

import java.nio.file.{Path, Paths}

case class Directories(cacheDir: Path, configDir: Path, bspSocketDir: Path)

object Directories {
  lazy val default: Directories = {
    val appDirs: AppDirs = AppDirsFactory.getInstance()
    val cacheDir = Paths.get(appDirs.getUserCacheDir("bleep", "1", "com.olvind"))
    val configDir = Paths.get(appDirs.getUserConfigDir("bleep", "1", "com.olvind"))
    new Directories(cacheDir, configDir, bspSocketDir = cacheDir / "bsp-socket")
  }
}
