package bleep

import java.nio.file.Path
import scala.collection.immutable.SortedSet

case class ProjectPaths(dir: Path, targetDir: Path, sourcesDirs: ProjectPaths.DirsByOrigin, resourcesDirs: ProjectPaths.DirsByOrigin, isTestProject: Boolean) {
  val classes: Path =
    targetDir / (if (isTestProject) "test-classes" else "classes")

  val incrementalAnalysis: Path =
    targetDir / s"inc_compile.zip"

  /** Extra JVM options a test fork of this project needs, one per line (blank lines and `#` comments ignored). A sourcegen may write this file to declare
    * options its generated output requires at runtime — the mechanism a Quarkus test project uses to hand its fork the serialized-model path and the jboss
    * LogManager, instead of every project restating them in `bleep.yaml`. Absent means no extra options. Read when the fork is assembled, so a changed file
    * re-forks through the normal option-keyed pool.
    */
  val forkJvmOptions: Path =
    targetDir / "bleep-fork-jvm-options"
}

object ProjectPaths {
  case class DirsByOrigin(
      fromSourceLayout: SortedSet[Path],
      fromJson: Map[RelPath, Path],
      generated: Map[model.ScriptDef, Path],
      annotationProcessing: Option[Path],
      ksp: List[Path]
  ) {
    val all: SortedSet[Path] = fromSourceLayout ++ fromJson.values ++ generated.values ++ annotationProcessing ++ ksp
    val cleanable: Iterable[Path] = generated.values ++ annotationProcessing ++ ksp
  }
}
