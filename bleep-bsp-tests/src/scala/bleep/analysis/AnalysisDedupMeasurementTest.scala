package bleep.analysis

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import xsbti.api.AnalyzedClass

import java.nio.file.{Files, Path, Paths}
import java.security.MessageDigest
import scala.jdk.CollectionConverters.*
import scala.jdk.OptionConverters.*

/** How much of the compile server's heap would a leaf interner give back?
  *
  * Not a unit test of behaviour — a measurement, kept in the suite so the number can be reproduced rather than quoted from a session that has scrolled away. It
  * loads real `analysis.zip` files and reports how many `AnalyzedClass` instances are duplicates of each other.
  *
  * ==Why AnalyzedClass is the number that matters==
  *
  * A class histogram of a live daemon at 7.2GB live set found ~4.5GB in `xsbti.api.*`: 31.7M `NameHash` (1.0GB), 31.1M `Id` (498MB), 7.4M `PathComponent[]`
  * (428MB), 839K `AnalyzedClass`, 839K `NameHash[]` (267MB). `AnalyzedClass` count matches `NameHash[]` count exactly, and each one owns the lazy `Companions`
  * tree holding all of the above — so the duplication factor measured here is the factor by which that 4.5GB shrinks.
  *
  * ==Reading the output==
  *
  * `distinct` counts by the proposed intern key — everything `AnalyzedClass` carries EXCEPT `compilationTimestamp`, which zinc 1.12 reads in one place as a
  * fast-path gate that falls back to a full structural diff, and which zinc 2.0.0 stopped reading altogether. The run reports both keys so the cost of
  * including the timestamp is visible rather than assumed: with it, two worktrees that compiled the same code independently do not merge.
  *
  * Point it at more workspaces with `BLEEP_DEDUP_ROOTS` (colon-separated workspace roots). With none set it measures this build alone, which shows only the
  * within-workspace sharing (library stamps, shared dependencies) and not the cross-worktree case that motivates the work.
  */
class AnalysisDedupMeasurementTest extends AnyFunSuite with Matchers {

  /** ScalaTest's `info` is swallowed by the runner's reporter, so the measurement is written where it can be read afterwards. */
  private val report = Paths.get(sys.props.getOrElse("user.dir", ".")).resolve("dedup-report.txt").toAbsolutePath
  private val lines = scala.collection.mutable.ListBuffer.empty[String]
  private def say(s: String): Unit = { lines += s; println(s"[dedup] $s") }
  private def flush(): Unit = Files.write(report, (s"report: $report\n" + lines.mkString("\n")).getBytes("UTF-8")): Unit

  private def analysisFilesUnder(root: Path): List[Path] =
    if (!Files.isDirectory(root)) Nil
    else {
      val stream = Files.walk(root)
      try stream.iterator().asScala.filter(p => p.getFileName.toString == "analysis.zip" && Files.isRegularFile(p)).toList
      finally stream.close()
    }

  /** Roots to measure, one per line in `dedup-roots.txt` beside the build, else this build's own `.bleep`.
    *
    * A file rather than an env var because the test runs in a forked JVM that does not inherit one.
    */
  private def roots: List[Path] = {
    val cwd = Paths.get(sys.props.getOrElse("user.dir", "."))
    val listing = cwd.resolve("dedup-roots.txt")
    if (Files.isRegularFile(listing))
      Files.readAllLines(listing).asScala.toList.map(_.trim).filter(_.nonEmpty).map(Paths.get(_))
    else List(cwd.resolve(".bleep"))
  }

  /** The proposed intern key. `withTimestamp` shows what including `compilationTimestamp` would cost in hit rate. */
  private def internKey(ac: AnalyzedClass, withTimestamp: Boolean): String = {
    val md = MessageDigest.getInstance("SHA-256")
    md.update(ac.name().getBytes("UTF-8"))
    md.update(java.nio.ByteBuffer.allocate(4).putInt(ac.apiHash()).array())
    md.update(java.nio.ByteBuffer.allocate(4).putInt(ac.extraHash()).array())
    md.update(if (ac.hasMacro()) Array[Byte](1) else Array[Byte](0))
    md.update(ac.provenance().getBytes("UTF-8"))
    // nameHashes is an array of (name, scope, hash) — digest it rather than holding it in the key.
    ac.nameHashes().sortBy(nh => (nh.name(), nh.scope().ordinal())).foreach { nh =>
      md.update(nh.name().getBytes("UTF-8"))
      md.update(java.nio.ByteBuffer.allocate(4).putInt(nh.scope().ordinal()).array())
      md.update(java.nio.ByteBuffer.allocate(4).putInt(nh.hash()).array())
    }
    if (withTimestamp) md.update(java.nio.ByteBuffer.allocate(8).putLong(ac.compilationTimestamp()).array())
    md.digest().map("%02x".format(_)).mkString
  }

  test("MEASUREMENT: duplicate AnalyzedClass across real analysis files") {
    val files = roots.flatMap(analysisFilesUnder)
    if (files.isEmpty) {
      say(s"no analysis.zip found under ${roots.mkString(", ")} — compile something first, or set BLEEP_DEDUP_ROOTS")
      flush()
      cancel("nothing to measure")
    }

    val totalBytes = files.map(Files.size).sum
    say(f"${files.size}%d analysis files, ${totalBytes / (1024.0 * 1024)}%.1f MB on disk, across ${roots.size}%d root(s)")

    var total = 0L
    val distinctNoTs = scala.collection.mutable.HashSet.empty[String]
    val distinctWithTs = scala.collection.mutable.HashSet.empty[String]
    var unreadable = 0

    files.foreach { f =>
      // The same store production uses, so this measures what bleep actually writes. Files left
      // over from zinc 1.x are in the old protobuf format and simply do not read here — they count
      // as unreadable and the run reports them.
      val store = sbt.internal.inc.consistent.ConsistentFileAnalysisStore.binary(
        f.toFile,
        ZincBridge.analysisMappers(f),
        reproducible = true,
        parallelism = 4
      )
      store.get().toScala match {
        case None           => unreadable += 1
        case Some(contents) =>
          contents.getAnalysis match {
            case a: sbt.internal.inc.Analysis =>
              val apis = a.apis
              (apis.internal.values ++ apis.external.values).foreach { ac =>
                total += 1
                distinctNoTs += internKey(ac, withTimestamp = false)
                distinctWithTs += internKey(ac, withTimestamp = true)
              }
            case other => say(s"unexpected analysis type ${other.getClass.getName}")
          }
      }
    }

    if (unreadable > 0) say(s"$unreadable file(s) could not be read")

    val ratioNoTs = if (distinctNoTs.isEmpty) 0.0 else total.toDouble / distinctNoTs.size
    val ratioWithTs = if (distinctWithTs.isEmpty) 0.0 else total.toDouble / distinctWithTs.size
    say(f"AnalyzedClass total=$total%d  distinct(no timestamp)=${distinctNoTs.size}%d  →  ${ratioNoTs}%.2fx sharing")
    say(f"AnalyzedClass total=$total%d  distinct(with timestamp)=${distinctWithTs.size}%d  →  ${ratioWithTs}%.2fx sharing")
    say(f"cost of keeping timestamp in the key: ${distinctWithTs.size - distinctNoTs.size}%d extra retained instances")

    flush()
    // A measurement with no data is not a failure — a fresh checkout, or the window right after an
    // analysis-format change, legitimately has nothing readable. Say so and skip.
    if (total == 0L) cancel(s"no readable analyses (${files.size} file(s) found, $unreadable unreadable)")
    distinctNoTs.size should be <= total.toInt
  }
}
