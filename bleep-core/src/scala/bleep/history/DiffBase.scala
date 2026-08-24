package bleep.history

import bleep.{BleepException, BuildPaths}

/** The base of a `--diff` run — what a `bleep compile --diff` / `bleep test --diff` (or the MCP `diffBase` argument) compares against.
  *
  * Parsed from the flag, resolved to a [[Transcript]] BEFORE the run starts: resolving up front both fails fast (a doomed diff must never cost a compile) and
  * pins the base, so a concurrent client writing entries mid-run cannot shift what "previous" meant.
  */
sealed trait DiffBase

object DiffBase {

  /** Bare `--diff` / `diffBase: "previous"`: the most recent history entry of the SAME MODE as the command — compile diffs against the last compile, test
    * against the last test, so a `test --diff` right after someone compiled does not trip on mode mismatch.
    */
  case object Previous extends DiffBase

  /** `--diff=<id>` / numeric `diffBase`: exactly that history entry. */
  case class Id(id: Long) extends DiffBase

  /** The MCP wire form: a numeric historyId or the literal "previous". Anything else fails loudly. */
  def parse(s: String): DiffBase =
    if (s == "previous") Previous
    else
      s.toLongOption match {
        case Some(id) => Id(id)
        case None     => throw new BleepException.Text(s"""diffBase must be a numeric historyId or the literal "previous", got: $s""")
      }

  /** Strict resolution, run BEFORE the build: an explicit id must exist in this workspace's store (missing/evicted ids fail with the store's own text) and
    * mode-match the command; bare `--diff` must find a same-mode entry.
    */
  def resolve(buildPaths: BuildPaths, mode: String, base: DiffBase): Transcript =
    base match {
      case Id(id) =>
        val t = TranscriptStore.read(buildPaths, id)
        if (t.mode != mode)
          throw new BleepException.Text(s"--diff base #$id is a ${t.mode} run, not a $mode run. A diff base must match the command's mode.")
        t
      case Previous =>
        previous(buildPaths, mode).getOrElse(
          throw new BleepException.Text(
            s"No previous $mode run recorded in ${TranscriptStore.dir(buildPaths)}. Run a $mode first, or name an entry explicitly."
          )
        )
    }

  /** The most recent same-mode entry, or None. The lenient variant — it seeds the rolling base of `--watch --diff`, where "no history yet" means the first
    * cycle renders plain output and rolling starts once that cycle has recorded an entry.
    */
  def previous(buildPaths: BuildPaths, mode: String): Option[Transcript] =
    TranscriptStore.list(buildPaths).reverseIterator.map(TranscriptStore.read(buildPaths, _)).find(_.mode == mode)
}
