package bleep.internal

import java.nio.file.{Path, Paths}
import scala.util.Properties

object Os {
  lazy val cwd: Path = {
    val base = Paths.get(System.getProperty("user.dir"))
    if (Properties.isWin)
      base.toFile.getCanonicalFile.toPath // todo: why? copied from scala-cli
    else
      base
  }
}
