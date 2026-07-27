package bleep.analysis

import bleep.model
import ryddig.Logger
import xsbti.compile.CompileAnalysis

import java.nio.file.{Files, Path}
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicLong
import scala.jdk.CollectionConverters.*

/** Zinc analyses read while compiling, held per workspace and bounded per workspace.
  *
  * ==Why this is bounded at all==
  *
  * This was a global, unbounded map of soft references, on the theory that GC would reclaim it when the heap got tight. Measured on a live server, it does not.
  * A class histogram of a daemon sitting at 7.2GB live set showed ~4.5GB in `xsbti.api.*` — 31.7M `NameHash` (1.0GB), 31.1M `Id` (498MB), 839K retained
  * `AnalyzedClass` — and a forced full GC moved the live set by 68MB. Soft references are not cleared until the collector is nearly out of room, and until then
  * they are indistinguishable from live data: they occupy the heap, they count as live set, and they crowd out the very compiles this cache exists to speed up.
  *
  * ==Why it is per workspace==
  *
  * Analysis files live at `.bleep/projects/<project>/builds/<variant>/.zinc/analysis.zip` — inside a workspace, partitioned by variant. So a
  * [[bleep.model.WorkspaceKey]] does not merely label these entries, it partitions them exactly, and nothing is shared across workspaces to lose by splitting
  * them up. Two things follow, both of which the previous global pool got wrong:
  *
  *   - '''Attribution.''' The retained cost of a workspace is a number this can report, rather than something to be inferred afterwards from a heap histogram
  *     and a linear fit.
  *   - '''Isolation.''' A monorepo churning through 166 analyses can no longer evict a small workspace's entire working set. Each gets its own budget.
  *
  * ==Why eviction is safe==
  *
  * Read-through: a miss re-reads from disk, costing about one disk read. A compile in flight holds its own strong references to the analyses it loaded, so
  * dropping an entry never pulls the ground from under running work — it only means the next reader re-reads.
  */
class AnalysisCache(budgetBytesPerWorkspace: Long, maxIdleMs: Long) {

  private case class Entry(mtime: Long, analysis: CompileAnalysis, fileBytes: Long, lastUsedMs: AtomicLong)

  private val byWorkspace = new ConcurrentHashMap[model.WorkspaceKey, ConcurrentHashMap[Path, Entry]]()

  private def bucket(key: model.WorkspaceKey): ConcurrentHashMap[Path, Entry] =
    byWorkspace.computeIfAbsent(key, _ => new ConcurrentHashMap[Path, Entry]())

  /** The cached analysis for `analysisFile`, if one is held and the file has not changed underneath it.
    *
    * The mtime check is what makes a `remote-cache pull` (or any other out-of-band rewrite) safe: a changed file invalidates the entry rather than serving
    * something that no longer describes what is on disk.
    */
  def get(key: model.WorkspaceKey, analysisFile: Path, currentMtime: Long): Option[CompileAnalysis] =
    Option(byWorkspace.get(key)).flatMap(b => Option(b.get(analysisFile))) match {
      case Some(entry) if entry.mtime == currentMtime =>
        entry.lastUsedMs.set(System.currentTimeMillis())
        Some(entry.analysis)
      case _ => None
    }

  def put(key: model.WorkspaceKey, analysisFile: Path, mtime: Long, analysis: CompileAnalysis): Unit = {
    val bytes =
      try Files.size(analysisFile)
      catch { case _: Exception => 0L }
    bucket(key).put(analysisFile, Entry(mtime, analysis, bytes, new AtomicLong(System.currentTimeMillis()))): Unit
  }

  /** Forget one analysis — used when the file on disk is deleted or found corrupt, so the next read starts from whatever is actually there. */
  def invalidate(key: model.WorkspaceKey, analysisFile: Path): Unit =
    Option(byWorkspace.get(key)).foreach(_.remove(analysisFile): Unit)

  /** Drop everything held for one workspace, returning what was freed.
    *
    * Called when that workspace's build is evicted from `BuildCache`: if nothing wants the build, nothing wants its analyses, and this is where the memory
    * actually is — the resolved build is hundreds of MB, its analyses are gigabytes.
    */
  def evictWorkspace(key: model.WorkspaceKey): AnalysisCache.Freed =
    Option(byWorkspace.remove(key)) match {
      case Some(b) => AnalysisCache.Freed(entries = b.size(), fileBytes = b.values().iterator().asScala.map(_.fileBytes).sum)
      case None    => AnalysisCache.Freed(0, 0L)
    }

  /** Drop expired and over-budget entries, workspace by workspace.
    *
    * Swept periodically rather than on every write so that a build loading twenty analyses in a burst is not made to walk the cache twenty times, and so the
    * largest retainer in the server heap is one legible periodic event rather than a side effect of whichever compile happened to finish last.
    */
  def sweep(nowMs: Long, logger: Logger): AnalysisCache.Stats = {
    byWorkspace.entrySet().iterator().asScala.foreach { e =>
      val key = e.getKey
      val b = e.getValue
      val present = b.entrySet().iterator().asScala.map(x => (x.getKey, x.getValue.lastUsedMs.get(), x.getValue.fileBytes)).toVector
      val doomed = AnalysisCache.selectEvictions(present, nowMs, maxIdleMs, budgetBytesPerWorkspace)
      if (doomed.nonEmpty) {
        doomed.foreach(p => b.remove(p): Unit)
        logger
          .withContext("workspace", key.short)
          .withContext("evicted", doomed.size)
          .withContext("remaining", b.size())
          .debug("Evicted Zinc analyses to bound retained heap; they will be re-read on next use")
      }
      // A workspace with nothing left keeps no bucket: an empty map per workspace ever seen is a
      // small leak, but it is still a leak, and it would make `stats` report workspaces that hold
      // nothing.
      if (b.isEmpty) byWorkspace.remove(key, b): Unit
    }
    stats
  }

  /** What is held right now, per workspace, for telemetry. */
  def stats: AnalysisCache.Stats = {
    val per = byWorkspace
      .entrySet()
      .iterator()
      .asScala
      .map { e =>
        val bytes = e.getValue.values().iterator().asScala.map(_.fileBytes).sum
        AnalysisCache.WorkspaceStats(e.getKey, e.getValue.size(), bytes)
      }
      .toList
      .sortBy(-_.fileBytes)
    AnalysisCache.Stats(per)
  }
}

object AnalysisCache {

  /** The cache bound to the one workspace a given compile is allowed to touch.
    *
    * Every call inside a compile needs both the cache and the key, and passing them separately means every call site is an opportunity to pass the wrong key —
    * which would silently charge one workspace's analyses to another and, worse, serve them across workspaces. Binding them once, where the workspace is known,
    * makes that unrepresentable.
    */
  case class Ref(cache: AnalysisCache, workspace: model.WorkspaceKey) {
    def get(analysisFile: Path, currentMtime: Long): Option[CompileAnalysis] = cache.get(workspace, analysisFile, currentMtime)
    def put(analysisFile: Path, mtime: Long, analysis: CompileAnalysis): Unit = cache.put(workspace, analysisFile, mtime, analysis)
    def invalidate(analysisFile: Path): Unit = cache.invalidate(workspace, analysisFile)
  }

  /** A cache for a compilation that belongs to no workspace — a standalone single-file compile, or a DAG built outside a BSP session.
    *
    * It gets its own instance, so whatever it holds dies with the call rather than accumulating on a daemon that will never sweep it. The alternative — letting
    * these share the daemon's cache under some placeholder key — would put entries in a bucket nothing owns and nothing evicts, which is precisely the shape of
    * the leak this class exists to remove.
    */
  def standalone(buildDir: Path): Ref =
    Ref(new AnalysisCache(DefaultBudgetBytesPerWorkspace, DefaultMaxIdleMs), model.WorkspaceKey(buildDir, model.BuildVariant.Normal))

  case class Freed(entries: Int, fileBytes: Long)
  case class WorkspaceStats(key: model.WorkspaceKey, entries: Int, fileBytes: Long)
  case class Stats(perWorkspace: List[WorkspaceStats]) {
    def entries: Int = perWorkspace.map(_.entries).sum
    def fileBytes: Long = perWorkspace.map(_.fileBytes).sum
  }

  /** Budget per workspace, measured in the on-disk bytes of the analysis files read.
    *
    * On-disk size is a proxy for retained heap, and a deliberately coarse one: measured against a real build, 113MB of analysis files (166 files, one
    * workspace) inflated to roughly 4.5GB of live objects — call it 6-7x, since the files are compressed and the in-memory form is an object graph. So this
    * budget is worth something like 1.5GB of heap per workspace, which holds a busy workspace's whole working set and change.
    *
    * Sized by measurement rather than taste, and worth re-measuring if that ratio moves: the `analysis_cache` metrics event records entries and bytes per
    * workspace, and a class histogram gives the heap side.
    */
  val DefaultBudgetBytesPerWorkspace: Long = 256L * 1024 * 1024

  /** Drop an analysis untouched for this long. A build loads a given dependency's analysis many times within seconds; across builds minutes apart, re-reading
    * costs a fraction of a second and saves gigabytes. This is the bound that actually matches what the cache is for — sharing within one build.
    */
  val DefaultMaxIdleMs: Long = 120000L

  /** Which entries to drop: everything idle past `maxIdleMs`, then least-recently-used until the summed file size is within `budgetBytes`.
    *
    * Pure, so the policy can be tested without a Zinc analysis to hand.
    */
  private[analysis] def selectEvictions(
      present: Vector[(Path, Long, Long)],
      nowMs: Long,
      maxIdleMs: Long,
      budgetBytes: Long
  ): Vector[Path] = {
    val (expired, fresh) = present.partition { case (_, lastUsed, _) => nowMs - lastUsed > maxIdleMs }
    val doomed = Vector.newBuilder[Path]
    doomed ++= expired.map(_._1)
    var total = fresh.map(_._3).sum
    if (total > budgetBytes) {
      // Oldest first: the entry least likely to be wanted by whatever is compiling now.
      fresh.sortBy { case (_, lastUsed, _) => lastUsed }.foreach { case (path, _, bytes) =>
        if (total > budgetBytes) { doomed += path; total -= bytes }
      }
    }
    doomed.result()
  }
}
