package bleep

import java.nio.file.Path
import scala.collection.immutable.SortedSet

case class ProjectPaths(dir: Path, targetDir: Path, sourcesDirs: ProjectPaths.DirsByOrigin, resourcesDirs: ProjectPaths.DirsByOrigin) {
  val classes: Path =
    targetDir / "classes"

  val incrementalAnalysis: Path =
    targetDir / s"inc_compile.zip"
}

object ProjectPaths {
  case class DirsByOrigin(fromSourceLayout: SortedSet[Path], fromJson: SortedSet[Path], generated: Path) {
    val all: SortedSet[Path] = fromSourceLayout ++ fromJson ++ List(generated)
  }
}
