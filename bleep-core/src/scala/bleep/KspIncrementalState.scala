package bleep

import io.circe.*
import io.circe.generic.semiauto.*
import io.circe.parser.decode
import io.circe.syntax.*

import bleep.internal.FileUtils

import java.io.IOException
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.security.MessageDigest
import scala.collection.immutable.SortedMap
import scala.jdk.CollectionConverters.*

/** Persistent manifest of the inputs to the previous KSP run for one project + variant. Used to decide whether the next run can be incremental and what
  * `-modified-sources` / `-removed-sources` / `-changed-classes` lists to pass.
  *
  * Stored at `.bleep/projects/<cross>/builds/<variant>/ksp/inputs-manifest.json`, alongside KSP's own `caches/` directory. Per-variant: a BSP-mode KSP run and
  * a Normal-mode KSP run have independent manifests so they don't cross-invalidate.
  *
  * @param schemaVersion
  *   bumped on incompatible manifest-schema changes. A schema mismatch is treated as "no prior state": next run is a cold rebuild.
  * @param kspVersion
  *   The KSP-side version that produced the last run (e.g. "1.0.32").
  * @param kotlinVersion
  *   The Kotlin compiler version (e.g. "2.1.20").
  * @param processorOptions
  *   Sorted map of `apoption=k=v` flags. A change in any value invalidates the cache.
  * @param processorJarFingerprint
  *   Stable hash over the set of (jar-name → mtime + size). Bumping a processor's version, adding or removing a processor, all change this fingerprint.
  * @param librariesFingerprint
  *   Stable hash over the resolved-dependencies classpath set. Any change invalidates (could be tighter via `-changed-classes` later, but the conservative
  *   answer is "wipe").
  * @param sources
  *   File path → SHA-256 hex digest. Used to compute modified-sources between runs.
  */
case class KspIncrementalState(
    schemaVersion: Int,
    kspVersion: String,
    kotlinVersion: String,
    jdkHome: String,
    jvmTarget: String,
    languageVersion: String,
    apiVersion: String,
    processorOptions: SortedMap[String, String],
    processorJarFingerprint: String,
    librariesFingerprint: String,
    sources: SortedMap[String, String]
)

object KspIncrementalState {

  /** Bump when changing the manifest fields incompatibly. Schema version 2 added jdkHome / jvmTarget / languageVersion / apiVersion to invalidation; an on-disk
    * manifest at schema 1 self-invalidates to FullRebuild on the next run.
    */
  val SchemaVersion: Int = 2

  implicit val codec: Codec[KspIncrementalState] = deriveCodec

  /** Inputs to a fresh KSP invocation, the ones we care to track for incrementality. The `sources` map keys must be the same string form (`Path.toString`) that
    * the KSP runner will see, so the modified-sources list passed to KSP refers to files KSP can locate.
    */
  case class CurrentInputs(
      kspVersion: String,
      kotlinVersion: String,
      jdkHome: String,
      jvmTarget: String,
      languageVersion: String,
      apiVersion: String,
      processorOptions: SortedMap[String, String],
      processorJars: List[Path],
      libraries: List[Path],
      sources: List[Path]
  )

  /** What the caller should do for this KSP run, based on a comparison between the persisted manifest and the current inputs. */
  sealed trait Decision
  object Decision {

    /** No prior state, or prior state is incompatible. Run KSP with `-incremental=false`; wipe the caches directory if it exists. */
    case object FullRebuild extends Decision

    /** A non-source input changed (processor version, options, libraries, …). KSP's per-source cache is no longer valid; wipe `caches/` and run with
      * `-incremental=false`. The next run after this can resume incrementally.
      */
    case object CacheBust extends Decision

    /** Inputs other than the source files are unchanged. KSP can run incrementally; pass the supplied modified/removed lists. Empty lists mean "no changes" and
      * KSP no-ops.
      */
    case class Incremental(modifiedSources: List[Path], removedSources: List[Path]) extends Decision
  }

  /** Hash the current inputs into a manifest snapshot. Used by [[decide]] internally and exposed so callers can thread the same snapshot from the decide step
    * into the [[save]] step on a successful KSP run — no need to re-hash every source on the way out.
    */
  def snapshot(current: CurrentInputs): KspIncrementalState =
    KspIncrementalState(
      schemaVersion = SchemaVersion,
      kspVersion = current.kspVersion,
      kotlinVersion = current.kotlinVersion,
      jdkHome = current.jdkHome,
      jvmTarget = current.jvmTarget,
      languageVersion = current.languageVersion,
      apiVersion = current.apiVersion,
      processorOptions = current.processorOptions,
      processorJarFingerprint = fingerprintFiles(current.processorJars),
      librariesFingerprint = fingerprintFiles(current.libraries),
      sources = SortedMap.from(current.sources.iterator.map(src => src.toString -> hashFile(src)))
    )

  /** Compute deltas + a fresh snapshot in one pass: read the persisted manifest (if any), hash current inputs, decide whether this run can be incremental, and
    * if so produce the modified/removed-sources lists. Return the snapshot alongside the decision so the caller can pass it back to [[save]] without
    * re-hashing.
    */
  def decideWithSnapshot(stateFile: Path, current: CurrentInputs): (Decision, KspIncrementalState) = {
    val prior = readState(stateFile)
    val snap = snapshot(current)
    val decision = prior match {
      case None =>
        Decision.FullRebuild

      case Some(p) if p.schemaVersion != SchemaVersion =>
        Decision.FullRebuild

      case Some(p)
          if p.kspVersion != snap.kspVersion ||
            p.kotlinVersion != snap.kotlinVersion ||
            p.jdkHome != snap.jdkHome ||
            p.jvmTarget != snap.jvmTarget ||
            p.languageVersion != snap.languageVersion ||
            p.apiVersion != snap.apiVersion ||
            p.processorJarFingerprint != snap.processorJarFingerprint ||
            p.librariesFingerprint != snap.librariesFingerprint ||
            p.processorOptions != snap.processorOptions =>
        Decision.CacheBust

      case Some(p) =>
        val priorPaths: Set[String] = p.sources.keySet
        val currentPaths: Set[String] = snap.sources.keySet
        val removed: List[Path] = priorPaths.diff(currentPaths).toList.sorted.map(java.nio.file.Paths.get(_))
        val modified: List[Path] = current.sources.filter { src =>
          val path = src.toString
          !p.sources.get(path).contains(snap.sources(path))
        }
        Decision.Incremental(modifiedSources = modified, removedSources = removed)
    }
    (decision, snap)
  }

  /** Convenience for callers that only need the decision (e.g. tests). Production paths should use [[decideWithSnapshot]] and feed the snapshot to [[save]] so
    * sources are hashed exactly once per build.
    */
  def decide(stateFile: Path, current: CurrentInputs): Decision =
    decideWithSnapshot(stateFile, current)._1

  /** Persist a previously-computed snapshot. Written via [[FileUtils.writeBytesAtomic]] so a crash mid-write doesn't leave a partial JSON file that would parse
    * as garbage and silently force a FullRebuild on the next run.
    */
  def save(stateFile: Path, snapshot: KspIncrementalState): Unit =
    FileUtils.writeBytesAtomic(stateFile, snapshot.asJson.spaces2.getBytes(StandardCharsets.UTF_8))

  /** Convenience overload that hashes inputs from scratch. Tests and any caller that doesn't already have a snapshot can use this; the hot build path uses
    * [[decideWithSnapshot]] + [[save]] (snapshot overload) to avoid hashing twice.
    */
  def save(stateFile: Path, current: CurrentInputs): Unit =
    save(stateFile, snapshot(current))

  /** Best-effort read of the persisted manifest. Returns None if the file is missing or unparseable (in which case the caller treats it as "no prior state" and
    * does a full rebuild).
    */
  private def readState(stateFile: Path): Option[KspIncrementalState] =
    if (!Files.exists(stateFile)) None
    else {
      try {
        val json = new String(Files.readAllBytes(stateFile), StandardCharsets.UTF_8)
        decode[KspIncrementalState](json).toOption
      } catch {
        case _: IOException => None
      }
    }

  /** SHA-256 of a file's bytes, as lowercase hex. Used per-source so source edits show up as modified-sources. */
  def hashFile(path: Path): String =
    if (!Files.isRegularFile(path)) "missing"
    else {
      val md = MessageDigest.getInstance("SHA-256")
      md.update(Files.readAllBytes(path))
      md.digest().map(b => f"$b%02x").mkString
    }

  /** A coarse fingerprint over a list of paths. Stable across input order. Regular files contribute `filename|mtime|size`; directories recurse so deep changes
    * (e.g. a single `.class` file under `target/classes/com/foo/Bar.class` getting recompiled) actually register — directory `mtime` only bubbles up one level
    * on Unix, so a top-level dir stat alone misses changes nested below it. Cost: an extra `Files.walk` per directory input per build.
    */
  def fingerprintFiles(paths: List[Path]): String = {
    val md = MessageDigest.getInstance("SHA-256")
    val entries = paths.iterator.map(fingerprintPath).toArray.sorted
    entries.foreach(e => md.update(e.getBytes(StandardCharsets.UTF_8)))
    md.digest().map(b => f"$b%02x").mkString
  }

  private def fingerprintPath(p: Path): String =
    if (!Files.exists(p)) s"${p.getFileName}|missing"
    else if (Files.isRegularFile(p)) {
      val attrs = Files.readAttributes(p, classOf[java.nio.file.attribute.BasicFileAttributes])
      s"${p.getFileName}|f|${attrs.lastModifiedTime.toMillis}|${attrs.size}"
    } else if (Files.isDirectory(p)) {
      val stream = Files.walk(p)
      try {
        val children = stream
          .iterator()
          .asScala
          .filter(Files.isRegularFile(_))
          .map { child =>
            val attrs = Files.readAttributes(child, classOf[java.nio.file.attribute.BasicFileAttributes])
            s"${p.relativize(child)}|${attrs.lastModifiedTime.toMillis}|${attrs.size}"
          }
          .toArray
        children.sorted.mkString(s"${p.getFileName}|d|", "\n", "")
      } finally stream.close()
    } else s"${p.getFileName}|special"

  /** List Kotlin and Java source files transitively under a set of source roots. Filter out KSP's own output dirs to avoid the "process my own output" feedback
    * loop. Returns sorted paths for determinism.
    */
  def listSources(sourceRoots: List[Path]): List[Path] = {
    val out = scala.collection.mutable.ArrayBuffer.empty[Path]
    sourceRoots.foreach { root =>
      if (Files.isDirectory(root)) {
        val stream = Files.walk(root)
        try
          stream
            .iterator()
            .asScala
            .filter(Files.isRegularFile(_))
            .filter { p =>
              val name = p.getFileName.toString
              name.endsWith(".kt") || name.endsWith(".java")
            }
            .foreach(out += _)
        finally stream.close()
      }
    }
    out.toList.sorted
  }
}
