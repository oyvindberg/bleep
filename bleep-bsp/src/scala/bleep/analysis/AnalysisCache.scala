package bleep.analysis

import bleep.model
import xsbti.api.AnalyzedClass
import xsbti.compile.CompileAnalysis

import java.lang.ref.{ReferenceQueue, WeakReference}
import java.nio.file.{Files, Path}
import java.security.MessageDigest
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.{AtomicLong, LongAdder}
import scala.jdk.CollectionConverters.*

/** Zinc analyses read while compiling, owned by the workspace they belong to and shared structurally between workspaces.
  *
  * ==Retention==
  *
  * An analysis lives exactly as long as its workspace's entry in [[bleep.bsp.BuildCache]]. There is no second retention policy: no size budget, no idle
  * timeout, no sweep. An earlier version had all three, on the assumption that holding one workspace's analyses was expensive enough to need its own eviction
  * schedule. Interning removes that assumption — see below — so the cache is now a plain index whose lifetime is the workspace's.
  *
  * ==Why interning==
  *
  * A class histogram of a live daemon at 7.2GB live set found ~4.5GB in `xsbti.api.*`: 31.7M `NameHash` (1.0GB), 31.1M `Id` (498MB), 7.4M `PathComponent[]`
  * (428MB), 839K `AnalyzedClass`, 839K `NameHash[]` (267MB). A forced full GC moved the live set by 68MB, so it was retained, not garbage.
  *
  * `AnalyzedClass` is 1:1 with `NameHash[]` and owns the lazy `Companions` tree containing all of the above, so interning that one type shares everything
  * beneath it. Measured against real builds: 2.06x sharing within one dlab workspace, 2.96x within bleep's own, and — the control — a second copy of an
  * identical workspace adds exactly zero distinct instances. Divergent branches share little; a freshly forked worktree shares everything.
  *
  * ==Why the key omits compilationTimestamp==
  *
  * Zinc 1.12 read it in exactly one place, as a fast-path gate that falls back to a full structural diff when it differs; zinc 2 removed the read entirely. Two
  * worktrees that compiled the same code independently get different timestamps, and including it cost 156,691 extra retained instances across six measured
  * worktrees against 94 within a single one. `ConsistentAnalysisFormat(reproducible = true)`, now in use, normalises them anyway.
  *
  * ==Safety of merging==
  *
  * A false merge needs two classes with the same name, apiHash, extraHash and nameHashes but different APIs — the state in which zinc's own invalidation is
  * already broken, since those hashes are what it compares. So this adds no failure mode zinc does not already have.
  */
class AnalysisCache {

  private case class Entry(mtime: Long, analysis: CompileAnalysis, fileBytes: Long, lastUsedMs: AtomicLong)

  private val byWorkspace = new ConcurrentHashMap[model.WorkspaceKey, ConcurrentHashMap[Path, Entry]]()

  /** The interner: an index, not a cache.
    *
    * Values are weakly referenced, so an interned `AnalyzedClass` lives exactly as long as some loaded analysis still points at it. Nothing here retains
    * anything; dropping a workspace drops its analyses, and whatever they alone held becomes collectable. Stale keys are expunged through the queue on each
    * intern, so the map does not accumulate tombstones for classes nobody references any more.
    */
  private val interned = new ConcurrentHashMap[String, InternRef]()
  private val internQueue = new ReferenceQueue[AnalyzedClass]()
  private val internHits = new LongAdder()
  private val internMisses = new LongAdder()

  private final class InternRef(val key: String, value: AnalyzedClass) extends WeakReference[AnalyzedClass](value, internQueue)

  private def expungeStaleInterned(): Unit = {
    var ref = internQueue.poll()
    while (ref != null) {
      ref match {
        case ir: InternRef => interned.remove(ir.key, ir): Unit
        case _             => ()
      }
      ref = internQueue.poll()
    }
  }

  /** The shared instance for this `AnalyzedClass` — `ac` itself if it is the first of its content seen. */
  private def internOne(ac: AnalyzedClass): AnalyzedClass = {
    val key = AnalysisCache.internKey(ac)
    var result: AnalyzedClass = null
    while (result == null) {
      val existing = interned.get(key)
      val alive = if (existing == null) null else existing.get()
      if (alive != null) {
        internHits.increment()
        result = alive
      } else {
        val fresh = new InternRef(key, ac)
        val won =
          if (existing == null) interned.putIfAbsent(key, fresh) == null
          else interned.replace(key, existing, fresh)
        if (won) {
          internMisses.increment()
          result = ac
        }
        // lost the race — loop and take whatever the winner installed
      }
    }
    result
  }

  /** Rebuild an analysis so its `AnalyzedClass` values are the shared instances.
    *
    * Only `apis` is substituted. Stamps and relations are keyed by `VirtualFileRef`s whose ids are already workspace-neutral (`${BASE}/…`), but their spines
    * are per-analysis maps that would have to be rebuilt wholesale for a much smaller return; `apis` is where the measured bytes are.
    *
    * The un-interned original becomes garbage as soon as the caller drops it, which is why this returns a new analysis rather than mutating.
    */
  private def internAnalysis(analysis: CompileAnalysis): CompileAnalysis =
    analysis match {
      case a: sbt.internal.inc.Analysis =>
        expungeStaleInterned()
        val apis = a.apis
        val internal = apis.internal.map { case (name, ac) => (name, internOne(ac)) }
        val external = apis.external.map { case (name, ac) => (name, internOne(ac)) }
        a.copy(a.stamps, sbt.internal.inc.APIs(internal, external), a.relations, a.infos, a.compilations)
      // Any other implementation is left alone rather than guessed at: interning is an optimisation,
      // and an analysis we cannot rebuild faithfully is one we should hand back untouched.
      case other => other
    }

  private def bucket(key: model.WorkspaceKey): ConcurrentHashMap[Path, Entry] =
    byWorkspace.computeIfAbsent(key, _ => new ConcurrentHashMap[Path, Entry]())

  /** Analyses indexed by the SHA-256 of the file they were read from, so two workspaces holding byte-identical analysis share one object graph.
    *
    * `internAnalysis` shares `AnalyzedClass` values and nothing else, which was right when the API tree was most of the heap. It no longer is: measured on
    * three divergent dlab worktrees, `xsbti.api.*` was 597MB of a 2.03GB floor while stamps and relations — untouched by interning — were 854MB. Their spines
    * are per-analysis maps, so there is no cheap way to share them piecemeal. Sharing the whole analysis is the cheap way.
    *
    * The opportunity is real rather than theoretical: hashing every project's `analysis.zip` across four worktrees checked out at four DIFFERENT commits, 35 of
    * 133 projects were byte-identical everywhere and 50% of the retained bytes were redundant copies. Worktrees mostly differ in a few projects.
    *
    * Safe across workspaces because a `CompileAnalysis` holds no absolute paths: everything is keyed by `VirtualFileRef` ids that `MappedFileConverter` writes
    * in `${BASE}/…` form. The one Path-typed field the read mapper rebases, `classpathHash`, lives in `MiniSetup`, which is not part of the analysis. (The same
    * property is why copying a `.bleep` directory into a fresh worktree works.)
    *
    * Weak values, like the class interner: a shared analysis lives exactly as long as some workspace still holds it.
    */
  private val byContent = new ConcurrentHashMap[String, ContentRef]()
  private val contentQueue = new ReferenceQueue[CompileAnalysis]()
  private val contentHits = new LongAdder()

  private final class ContentRef(val key: String, value: CompileAnalysis) extends WeakReference[CompileAnalysis](value, contentQueue)

  private def expungeStaleContent(): Unit = {
    var ref = contentQueue.poll()
    while (ref != null) {
      ref match {
        case cr: ContentRef => byContent.remove(cr.key, cr): Unit
        case _              => ()
      }
      ref = contentQueue.poll()
    }
  }

  /** SHA-256 of the analysis file, or None if it cannot be read — in which case the caller simply deserialises as before. */
  private def contentHash(analysisFile: Path): Option[String] =
    try {
      val md = MessageDigest.getInstance("SHA-256")
      val buf = new Array[Byte](64 * 1024)
      val in = Files.newInputStream(analysisFile)
      try {
        var n = in.read(buf)
        while (n > 0) { md.update(buf, 0, n); n = in.read(buf) }
      } finally in.close()
      Some(md.digest().map("%02x".format(_)).mkString)
    } catch { case _: Exception => None }

  /** The cached analysis for `analysisFile`, if held and the file has not changed underneath it.
    *
    * The mtime check is what makes a `remote-cache pull` (or any other out-of-band rewrite) safe: a changed file invalidates the entry rather than serving
    * something that no longer describes what is on disk.
    *
    * Deliberately NOT extended to consult the content index. Doing so would also skip deserialisation, but it would let a workspace be served an analysis it
    * never read — which breaks the partitioning this cache is built on, lets `mtime` be bypassed as the staleness check, and lets an evicted workspace
    * resurrect what it just dropped. The sharing happens in [[put]] instead, where the analysis is one this workspace legitimately read: the memory is shared,
    * the ownership is not.
    */
  def get(key: model.WorkspaceKey, analysisFile: Path, currentMtime: Long): Option[CompileAnalysis] =
    Option(byWorkspace.get(key)).flatMap(b => Option(b.get(analysisFile))) match {
      case Some(entry) if entry.mtime == currentMtime =>
        entry.lastUsedMs.set(System.currentTimeMillis())
        Some(entry.analysis)
      case _ => None
    }

  /** Intern and store, returning the instance to use. Callers must use the RETURN value, not what they passed in — that is the whole point. */
  def put(key: model.WorkspaceKey, analysisFile: Path, mtime: Long, analysis: CompileAnalysis): CompileAnalysis = {
    expungeStaleContent()
    // If some workspace already holds an analysis deserialised from these exact bytes, keep that one
    // and let this copy become garbage. Both describe the same compilation, so either is correct —
    // this just stops the daemon holding N graphs where one will do. The caller must use the return
    // value, which it already had to.
    val hash = contentHash(analysisFile)
    val existing = hash.flatMap(h => Option(byContent.get(h))).flatMap(ref => Option(ref.get()))
    val shared = existing match {
      case Some(already) =>
        contentHits.increment()
        already
      case None =>
        val fresh = internAnalysis(analysis)
        hash.foreach(h => byContent.put(h, new ContentRef(h, fresh)): Unit)
        fresh
    }
    val bytes =
      try Files.size(analysisFile)
      catch { case _: Exception => 0L }
    bucket(key).put(analysisFile, Entry(mtime, shared, bytes, new AtomicLong(System.currentTimeMillis()))): Unit
    shared
  }

  /** Forget one analysis — used when the file on disk is deleted or found corrupt, so the next read starts from what is actually there. */
  def invalidate(key: model.WorkspaceKey, analysisFile: Path): Unit =
    Option(byWorkspace.get(key)).foreach(_.remove(analysisFile): Unit)

  /** Drop everything held for one workspace, returning what was freed.
    *
    * Called when that workspace's build leaves `BuildCache`. Interned classes it shared with other workspaces stay alive through those; ones only it referenced
    * become collectable, and their keys leave the interner via the reference queue on the next intern.
    */
  def evictWorkspace(key: model.WorkspaceKey): AnalysisCache.Freed =
    Option(byWorkspace.remove(key)) match {
      case Some(b) => AnalysisCache.Freed(entries = b.size(), fileBytes = b.values().iterator().asScala.map(_.fileBytes).sum)
      case None    => AnalysisCache.Freed(0, 0L)
    }

  /** What is held right now, for telemetry. Purely observational — nothing here drives eviction. */
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
    AnalysisCache.Stats(
      per,
      internedClasses = interned.size(),
      internHits = internHits.sum(),
      internMisses = internMisses.sum(),
      sharedAnalyses = byContent.size(),
      contentHits = contentHits.sum()
    )
  }
}

object AnalysisCache {

  /** The cache bound to the one workspace a given compile may touch.
    *
    * Every call inside a compile needs both the cache and the key, and passing them separately makes every call site an opportunity to pass the wrong one —
    * which would charge one workspace's analyses to another and, worse, serve them across workspaces. Binding them once, where the workspace is known, makes
    * that unrepresentable.
    */
  case class Ref(cache: AnalysisCache, workspace: model.WorkspaceKey) {
    def get(analysisFile: Path, currentMtime: Long): Option[CompileAnalysis] = cache.get(workspace, analysisFile, currentMtime)
    def put(analysisFile: Path, mtime: Long, analysis: CompileAnalysis): CompileAnalysis = cache.put(workspace, analysisFile, mtime, analysis)
    def invalidate(analysisFile: Path): Unit = cache.invalidate(workspace, analysisFile)
  }

  /** A cache for a compilation belonging to no workspace — a standalone single-file compile, or a DAG built outside a BSP session. It gets its own instance, so
    * whatever it holds dies with the call instead of accumulating in a bucket nothing owns.
    */
  def standalone(buildDir: Path): Ref =
    Ref(new AnalysisCache, model.WorkspaceKey(buildDir, model.BuildVariant.Normal))

  case class Freed(entries: Int, fileBytes: Long)
  case class WorkspaceStats(key: model.WorkspaceKey, entries: Int, fileBytes: Long)
  case class Stats(
      perWorkspace: List[WorkspaceStats],
      internedClasses: Int,
      internHits: Long,
      internMisses: Long,
      sharedAnalyses: Int,
      contentHits: Long
  ) {
    def entries: Int = perWorkspace.map(_.entries).sum
    def fileBytes: Long = perWorkspace.map(_.fileBytes).sum

    /** How many `AnalyzedClass` instances the daemon would be holding without interning, per instance it actually holds. 1.0 means nothing was shared. */
    def sharingFactor: Double = if (internedClasses == 0) 0.0 else (internHits + internMisses).toDouble / internedClasses
  }

  /** Content key for an `AnalyzedClass`: everything it carries except `compilationTimestamp`.
    *
    * `nameHashes` is digested rather than held, and sorted first because zinc does not promise an order.
    */
  private[analysis] def internKey(ac: AnalyzedClass): String = {
    val md = MessageDigest.getInstance("SHA-256")
    def int(i: Int): Unit = md.update(java.nio.ByteBuffer.allocate(4).putInt(i).array())
    md.update(ac.name().getBytes("UTF-8"))
    int(ac.apiHash())
    int(ac.extraHash())
    md.update(if (ac.hasMacro()) Array[Byte](1) else Array[Byte](0))
    md.update(ac.provenance().getBytes("UTF-8"))
    ac.nameHashes().sortBy(nh => (nh.name(), nh.scope().ordinal())).foreach { nh =>
      md.update(nh.name().getBytes("UTF-8"))
      int(nh.scope().ordinal())
      int(nh.hash())
    }
    md.digest().map("%02x".format(_)).mkString
  }
}
