package bleep

import java.nio.file.Path
import scala.collection.immutable.SortedSet

/** The set of directories whose contents feed compilation of a single project.
  *
  * There is exactly one definition of "input" in bleep, and it lives here so the four mechanisms that ask the question — the sourcegen staleness check,
  * [[ProjectDigest]], `bleep build invalidated` and `--watch` — cannot answer it differently. They used to: `sourceGlobs` on a `sourcegen:` entry reached
  * `BleepFileWatching` and nothing else, so a declared input directory woke up watch mode, and then the staleness check skipped the generator anyway because it
  * had never heard of the directory. A setting that is honoured by one out of four mechanisms is worse than one that is honoured by none, because it looks like
  * it works.
  *
  * Note the resolution base: `sourceGlobs` paths are relative to the folder of the project that *declares* `sourcegen:`, not to the script project's folder.
  * That is what the code has always done, so it is what stays.
  */
object ProjectInputs {

  /** Directories declared via `sourceGlobs` on one `sourcegen:` entry, resolved against the consuming project's own folder. */
  def declaredSourcegenInputs(script: model.ScriptDef.Main, consumerPaths: ProjectPaths): Iterator[Path] =
    script.sourceGlobs.values.iterator.map(relPath => consumerPaths.dir / relPath)

  /** Directories declared via `sourceGlobs` across all of a project's `sourcegen:` entries. */
  def declaredSourcegenInputs(project: model.Project, consumerPaths: ProjectPaths): Iterator[Path] =
    project.sourcegen.values.iterator.flatMap { case script: model.ScriptDef.Main => declaredSourcegenInputs(script, consumerPaths) }

  /** Everything that counts as an input to this project: its own sources and resources, plus any directory it declared under `sourceGlobs`.
    *
    * Deliberately *not* used by [[ProjectDigest]], which hashes sources, resources and declared inputs as three separate steps so the byte order fed to the
    * digest — and therefore every published cache key — stays what it was.
    */
  def all(project: model.Project, projectPaths: ProjectPaths): SortedSet[Path] =
    projectPaths.sourcesDirs.all ++ projectPaths.resourcesDirs.all ++ declaredSourcegenInputs(project, projectPaths)
}
