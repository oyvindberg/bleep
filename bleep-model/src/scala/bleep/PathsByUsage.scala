package bleep

import bleep.internal.codecs.codecPath
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.{Decoder, Encoder}

import java.nio.file.Path

/** An ordered list of paths, each tagged with the narrowest [[Usage]] that includes it, so a reader asks for the set it needs instead of receiving one
  * flattened list and having to know which parts of it to leave out.
  *
  * This is the form a project's classpath and resources take once they leave the directories that know their own provenance ([[ProjectPaths.DirsByOrigin]]): it
  * is what `ResolvedProject` carries, and therefore what crosses the wire to the BSP server. Before it, `ResolvedProject.classpath` was a single list handed to
  * javac and to the test fork alike, so a path could not be on one without being on the other.
  *
  * ==No ordering policy of its own==
  *
  * Classpath order decides which of two entries providing the same class wins, and the sites that build these do not agree on it: the main classpath is built
  * through a `SortedSet` (sorted by path, deduplicated), while the nested-build rewrite prepends bleep's own development classes precisely so that they shadow.
  * So entries keep whatever order they were constructed in, [[++]] is plain concatenation, and a site that wants set semantics asks for [[sortedDistinct]]
  * explicitly. Baking one policy in here would silently change the other.
  */
case class PathsByUsage(entries: List[PathsByUsage.Entry]) {

  /** The paths `usage` admits, in entry order. */
  def apply(usage: Usage): List[Path] = entries.collect { case e if usage.admits(e.usage) => e.path }

  /** Concatenation: `this` first. */
  def ++(other: PathsByUsage): PathsByUsage = PathsByUsage(entries ++ other.entries)
}

object PathsByUsage {
  case class Entry(path: Path, usage: Usage)

  val empty: PathsByUsage = PathsByUsage(Nil)

  /** Every path tagged `usage`, in the order given. */
  def of(usage: Usage, paths: IterableOnce[Path]): PathsByUsage = PathsByUsage(paths.iterator.map(Entry(_, usage)).toList)

  /** What a `SortedSet[Path]` would have produced: sorted by path, one entry per path.
    *
    * Where a path was tagged twice the narrowest usage wins — if anything says the compiler sees it, the compiler sees it.
    */
  def sortedDistinct(entries: IterableOnce[Entry]): PathsByUsage =
    PathsByUsage(
      entries.iterator.toList.groupBy(_.path).values.map(_.minBy(e => Usage.rank(e.usage))).toList.sortBy(_.path)
    )

  implicit val entryEncoder: Encoder[Entry] = deriveEncoder
  implicit val entryDecoder: Decoder[Entry] = deriveDecoder
  implicit val encodes: Encoder[PathsByUsage] = deriveEncoder
  implicit val decodes: Decoder[PathsByUsage] = deriveDecoder
}
