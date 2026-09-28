package bleep.testing

import bleep.BleepException

import java.nio.file.Path
import scala.collection.immutable.{SortedMap, SortedSet}
import scala.jdk.CollectionConverters.IteratorHasAsScala

/** What the import roundtrips have in common: projects reduced to a [[RoundtripReport.View]] on both sides, a diff of the two, and the rendered report. */
object RoundtripReport {

  /** Per aspect of a project (classpath, options, sources, ...), a set of normalized entries. Sets, because the ordering of neither side can be trusted */
  type View = SortedMap[String, SortedSet[String]]

  /** A normalized jar is rendered as `jar name:version` */
  object Jar {
    def render(name: String, version: String): String = s"jar $name:$version"
    def unapply(s: String): Option[(String, String)] =
      if (!s.startsWith("jar ")) None
      else
        s.drop(4).split(':') match {
          case Array(name, version) => Some((name, version))
          case _                    => None
        }
  }

  private val VersionedStem = """(.+?)-(\d+\.\d+.*)""".r

  /** `jar name:version` for every way a jar ends up on a classpath: coursier's cache, a maven local repository, ivy's cache and sbt's launcher. `None` for the
    * JDK's own jars, which sbt puts on the classpath of some builds and bleep never does.
    *
    * @param ownBuild
    *   how to render a jar the imported build packaged itself, if `p` is one
    */
  def jar(p: Path, ownBuild: Path => Option[String]): Option[String] = {
    val segments = p.iterator().asScala.map(_.toString).toVector
    val file = segments.last
    val stem = file.stripSuffix(".jar")
    segments.dropRight(1).reverse.toList match {
      case _ if segments.exists(s => s.endsWith(".jdk") || s == "jre") => None
      case _ if ownBuild(p).isDefined                                  => ownBuild(p)
      // sbt launcher: ~/.sbt/boot/scala-2.12.20/lib/scala-library.jar
      case "lib" :: scalaDir :: "boot" :: _ if scalaDir.startsWith("scala-") =>
        // mostly `scala-library.jar`, versioned by the directory. modules like `scala-xml_2.12-2.3.0.jar` carry their own version
        stem match {
          case VersionedStem(name, version) => Some(Jar.render(name, version))
          case _                            => Some(Jar.render(stem, scalaDir.stripPrefix("scala-")))
        }
      // ivy layout repository in coursier's cache: .../org/name/scala_2.12/sbt_1.0/version/jars/name.jar
      case "jars" :: version :: _ if segments.contains("sbt-plugin-releases") => Some(Jar.render(stem, version))
      // ivy: ~/.ivy2/cache/org/name/jars/name-version.jar
      case ("jars" | "bundles") :: name :: _ if stem.startsWith(s"$name-") => Some(Jar.render(name, stem.stripPrefix(s"$name-")))
      // maven layout, in coursier's cache or a maven local repository: .../org/name/version/name-version(-classifier).jar
      // the name is taken from the file, since the directory of an sbt plugin carries a suffix like `_2.12_1.0`
      case version :: _ if stem.endsWith(s"-$version")  => Some(Jar.render(stem.stripSuffix(s"-$version"), version))
      case version :: _ if stem.contains(s"-$version-") =>
        val idx = stem.indexOf(s"-$version-")
        // ivy's file names can't tell a classifier from the version, so render it the way ivy does
        Some(Jar.render(stem.take(idx), stem.drop(idx + 1)))
      case _ => throw new BleepException.Text(s"Don't know how to normalize jar $p")
    }
  }

  def diff(input: View, output: View): List[(String, List[String])] =
    (input.keySet ++ output.keySet).toList.sorted.flatMap { aspect =>
      val in = input.getOrElse(aspect, SortedSet.empty[String])
      val out = output.getOrElse(aspect, SortedSet.empty[String])
      val removed = in -- out
      val added = out -- in
      if (removed.isEmpty && added.isEmpty) Nil
      else {
        // pair up a jar which is there on both sides at another version. only when there is one of each, a name can be on the classpath several times with
        // different classifiers (netty's native jars for each os)
        def byName(entries: SortedSet[String]): Map[String, List[String]] =
          entries.toList.collect { case Jar(name, version) => (name, version) }.groupMap(_._1)(_._2)
        val removedByName = byName(removed)
        val addedByName = byName(added)
        val changedNames = removedByName.keySet.intersect(addedByName.keySet).filter(name => removedByName(name).size == 1 && addedByName(name).size == 1)
        val changed = changedNames.toList.sorted.map(name => s"~ $name: ${removedByName(name).head} -> ${addedByName(name).head}")
        val notChanged = (s: String) =>
          s match {
            case Jar(name, _) => !changedNames(name)
            case _            => true
          }
        List((aspect, removed.toList.filter(notChanged).map(s => s"- $s") ++ added.toList.filter(notChanged).map(s => s"+ $s") ++ changed))
      }
    }

  /** @param header
    *   what was compared, and what could not be
    * @param compared
    *   how many projects were compared
    * @param diffs
    *   the projects which differ, by aspect
    */
  def render(header: List[String], compared: Int, diffs: List[(String, List[(String, List[String])])]): String = {
    // the same difference in many projects is one finding, so list those first
    val common: List[String] =
      diffs
        .flatMap { case (_, byAspect) => byAspect.flatMap { case (aspect, lines) => lines.map(line => (aspect, line)) } }
        .groupBy(identity)
        .toList
        .collect { case ((aspect, line), occurrences) if occurrences.size >= 3 => (occurrences.size, aspect, line) }
        .sortBy { case (count, aspect, line) => (-count, aspect, line) }
        .take(30)
        .map { case (count, aspect, line) => f"$count%5d  $aspect $line" }

    val summary = if (common.isEmpty) Nil else "" :: "most common differences (projects, aspect, difference):" :: common
    val body = diffs.flatMap { case (name, byAspect) =>
      "" :: name :: byAspect.flatMap { case (aspect, lines) => s"  $aspect" :: lines.map(line => s"    $line") }
    }
    (header ++ List(s"compared: $compared, identical: ${compared - diffs.size}") ++ summary ++ body).mkString("\n")
  }
}
