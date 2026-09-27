package bleep

import java.nio.file.Path
import scala.collection.immutable.SortedSet

case class ProjectPaths(
    dir: Path,
    targetDir: Path,
    sourcesDirs: ProjectPaths.DirsByOrigin,
    resourcesDirs: ProjectPaths.DirsByOrigin,
    isTestProject: Boolean,
    hasPostCompile: Boolean
) {

  /** The project's classes, as everything outside the compile step sees them: classpaths, runners, test discovery, packaging, the IDE. */
  val classes: Path =
    targetDir / (if (isTestProject) "test-classes" else "classes")

  /** Where the compiler writes. The same as [[classes]], unless the project has a `postCompile` script: then the compiler gets a private directory, the script
    * reads it and writes [[classes]], and nothing else ever looks here. Anything the compiler emits — class files, SemanticDB, zinc's view of its products —
    * belongs here, never in [[classes]], or the script's output would overwrite it.
    */
  val compilerOutput: Path =
    if (hasPostCompile) targetDir / (if (isTestProject) "test-classes-pre" else "classes-pre") else classes

  /** Fingerprint of the inputs [[classes]] was last produced from by the post-compile script. Absent while [[classes]] is not a finished product of the current
    * inputs.
    */
  val postCompileStamp: Path =
    targetDir / "post-compile.stamp"

  /** What the post-compile script did to the API, as consumers are keyed on it. Written with [[postCompileStamp]], from the same run. */
  val postCompileAbi: Path =
    targetDir / "post-compile.abi"

  val incrementalAnalysis: Path =
    targetDir / s"inc_compile.zip"

  /** Extra JVM options a test fork of this project needs, one per line (blank lines and `#` comments ignored). A sourcegen may write this file to declare
    * options its generated output requires at runtime — the mechanism a code-generating test project uses to hand its fork a generated path or a custom
    * LogManager, instead of every project restating them in `bleep.yaml`. Absent means no extra options. Read when the fork is assembled, so a changed file
    * re-forks through the normal option-keyed pool.
    */
  val forkJvmOptions: Path =
    targetDir / "bleep-fork-jvm-options"
}

object ProjectPaths {

  /** A project's source (or resource) directories, grouped by where they came from.
    *
    * Provenance decides [[Usage]]: each origin is tagged at the narrowest usage that includes it, and [[all]] returns everything tagged at or below the usage
    * asked for. A new origin must be placed in exactly one tier below — and landing it in [[input]] is the safe default, because over-including costs a rebuild
    * while under-including serves wrong classes out of the cache.
    */
  case class DirsByOrigin(
      fromSourceLayout: SortedSet[Path],
      fromJson: Map[RelPath, Path],
      generated: Map[model.ScriptDef, Path],
      annotationProcessing: Option[Path],
      ksp: List[Path],
      /** Where bleep writes this project's `stamp:` values, if it declared any.
        *
        * Derived from the build — a version from git, a digest of the project — so feeding it back into the cache key is circular for `project-digest` and, for
        * `dynver`, makes the project and everything downstream of it a permanent cache miss. It reaches the runtime classpath and the packaged jar and nothing
        * else; see [[Usage.Runtime]].
        */
      stamps: Option[Path]
  ) {

    /** Every directory, tagged with the narrowest [[Usage]] that includes it. The one place an origin is assigned a tier; [[all]] is derived from it.
      *
      * Sorted and deduplicated, because every consumer of these directories has always received them through a `SortedSet`.
      */
    val byUsage: PathsByUsage =
      PathsByUsage.sortedDistinct(
        (fromSourceLayout.iterator ++ fromJson.values ++ generated.values).map(PathsByUsage.Entry(_, Usage.Input)) ++
          // Annotation-processor and KSP output is what a compile-time generator expanded from the inputs, no different in kind from a macro expansion that
          // happens to be written to a file. The compiler sees it; nothing is keyed on it. See `Usage` for why sourcegen output, one line up, is different.
          (annotationProcessing.iterator ++ ksp).map(PathsByUsage.Entry(_, Usage.Compile)) ++
          stamps.iterator.map(PathsByUsage.Entry(_, Usage.Runtime))
      )

    private val input: SortedSet[Path] = SortedSet.from(byUsage(Usage.Input))
    private val compile: SortedSet[Path] = SortedSet.from(byUsage(Usage.Compile))
    private val runtime: SortedSet[Path] = SortedSet.from(byUsage(Usage.Runtime))

    /** The directories relevant to `usage`. Deliberately takes the question rather than defaulting to one: see [[Usage]]. */
    def all(usage: Usage): SortedSet[Path] = usage match {
      case Usage.Input   => input
      case Usage.Compile => compile
      case Usage.Runtime => runtime
    }

    /** Everything bleep generated, and may therefore delete. */
    val cleanable: Iterable[Path] = generated.values ++ annotationProcessing ++ ksp ++ stamps
  }
}
