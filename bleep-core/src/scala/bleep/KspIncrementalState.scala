package bleep

import io.circe.*
import io.circe.generic.semiauto.*
import io.circe.parser.decode
import io.circe.syntax.*

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
    processorOptions: SortedMap[String, String],
    processorJarFingerprint: String,
    librariesFingerprint: String,
    sources: SortedMap[String, String]
)

object KspIncrementalState {

  /** Bump when changing the manifest fields incompatibly. */
  val SchemaVersion: Int = 1

  implicit val codec: Codec[KspIncrementalState] = deriveCodec

  /** Inputs to a fresh KSP invocation, the ones we care to track for incrementality. The `sources` map keys must be the same string form (`Path.toString`) that
    * the KSP runner will see, so the modified-sources list passed to KSP refers to files KSP can locate.
    */
  case class CurrentInputs(
      kspVersion: String,
      kotlinVersion: String,
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

  /** Compute deltas: read the persisted manifest (if any), hash current inputs, decide whether this run can be incremental, and if so produce the
    * modified/removed-sources lists.
    */
  def decide(stateFile: Path, current: CurrentInputs): Decision = {
    val prior = readState(stateFile)
    val currentProcessorJarFingerprint = fingerprintFiles(current.processorJars)
    val currentLibrariesFingerprint = fingerprintFiles(current.libraries)

    prior match {
      case None =>
        Decision.FullRebuild

      case Some(p) if p.schemaVersion != SchemaVersion =>
        Decision.FullRebuild

      case Some(p)
          if p.kspVersion != current.kspVersion ||
            p.kotlinVersion != current.kotlinVersion ||
            p.processorJarFingerprint != currentProcessorJarFingerprint ||
            p.librariesFingerprint != currentLibrariesFingerprint ||
            p.processorOptions != current.processorOptions =>
        Decision.CacheBust

      case Some(p) =>
        val priorPaths: Set[String] = p.sources.keySet
        val currentPaths: Set[String] = current.sources.iterator.map(_.toString).toSet
        val removed: List[Path] = priorPaths.diff(currentPaths).toList.sorted.map(java.nio.file.Paths.get(_))
        val modified: List[Path] = current.sources.flatMap { src =>
          val path = src.toString
          val currentHash = hashFile(src)
          p.sources.get(path) match {
            case Some(priorHash) if priorHash == currentHash => None
            case _                                           => Some(src)
          }
        }
        Decision.Incremental(modifiedSources = modified, removedSources = removed)
    }
  }

  /** Persist the manifest after a successful KSP run. Hashes are computed fresh from disk. */
  def save(stateFile: Path, current: CurrentInputs): Unit = {
    val state = KspIncrementalState(
      schemaVersion = SchemaVersion,
      kspVersion = current.kspVersion,
      kotlinVersion = current.kotlinVersion,
      processorOptions = current.processorOptions,
      processorJarFingerprint = fingerprintFiles(current.processorJars),
      librariesFingerprint = fingerprintFiles(current.libraries),
      sources = SortedMap.from(current.sources.iterator.map(src => src.toString -> hashFile(src)))
    )
    Files.createDirectories(stateFile.getParent)
    Files.write(stateFile, state.asJson.spaces2.getBytes(StandardCharsets.UTF_8))
  }

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

  /** A coarse fingerprint over a list of paths: hash (filename + mtime + size) for each. Stable across path-order shuffles. Used for processor jars and
    * libraries — these are bigger and we don't need source-level precision; any change to the set or to a file's mtime/size means "something moved."
    */
  def fingerprintFiles(paths: List[Path]): String = {
    val md = MessageDigest.getInstance("SHA-256")
    val entries = paths.iterator
      .map { p =>
        if (!Files.exists(p)) s"${p.getFileName}|missing"
        else {
          val attrs = Files.readAttributes(p, classOf[java.nio.file.attribute.BasicFileAttributes])
          s"${p.getFileName}|${attrs.lastModifiedTime.toMillis}|${attrs.size}"
        }
      }
      .toArray
      .sorted // stable across input ordering
    entries.foreach(e => md.update(e.getBytes(StandardCharsets.UTF_8)))
    md.digest().map(b => f"$b%02x").mkString
  }

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
