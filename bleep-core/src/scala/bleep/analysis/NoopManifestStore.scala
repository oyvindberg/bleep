package bleep.analysis

import java.io.{DataInputStream, DataOutputStream}
import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.{Files, Path, StandardCopyOption}

/** Persistent fast-path cache that lets bleep skip Zinc entirely when nothing about a project has changed since its last successful compile.
  *
  * The manifest captures per-source/per-dir/per-output-dir `(ctime, mtime, size)` tuples plus a hash of compiler options and the recorded result. On the next
  * compile attempt, [[ZincBridge.isNoop]] short-circuits when every recorded stat still matches.
  *
  * The format is binary; see `NoopManifestVersion` for evolution. Old versions are treated as cache misses (forces a Zinc round-trip which then writes a fresh
  * manifest).
  *
  * Lives in `bleep-core` (not `bleep-bsp`) so the remote-cache pull command can regenerate manifests against the local filesystem after extracting an archive
  * from S3 — turning the first post-pull compile into a true noop hit instead of a Zinc no-op compile.
  */
object NoopManifestStore {

  /** Per-file (or per-directory) stat tuple captured at compile time. */
  /** `contentHash` is 0 for anything not hashed — directories, and entries from before the hash existed.
    *
    * It exists for one case: a source whose timestamps moved but whose bytes did not. That is the single most common reason the noop fast path declines and
    * zinc then finds nothing to recompile (`noop_disagreement` in the metrics), because a checkout, a formatter or a code generator rewriting identical content
    * moves ctime without changing the file. Comparing the hash turns a full zinc invalidation into one read of a file we already stat.
    */
  /** A dependency's analysis file as this project last saw it.
    *
    * mtime alone was the check, and it is the wrong one: any recompile of the dependency rewrites its `analysis.zip` and moves the mtime, even when the
    * resulting analysis is byte-identical — which `ConsistentAnalysisFormat(reproducible = true)` makes the common case. Every downstream project then declined
    * the fast path and made zinc prove, the expensive way, that nothing had changed. Measured: 100% of organic `noop_disagreement` events were exactly this.
    *
    * mtime is kept as the cheap first test; the hash is only read when it disagrees.
    */
  case class DepAnalysisStat(mtimeMillis: Long, contentHash: Long)

  case class FileStatEntry(ctimeMillis: Long, mtimeMillis: Long, size: Long, contentHash: Long)

  case class NoopManifest(
      sourceStats: Map[Path, FileStatEntry],
      sourceDirStats: Map[Path, FileStatEntry], // source dir + subdirs → stat (detects file add/delete)
      outputDirStats: Map[Path, FileStatEntry], // output dir + subdirs → stat (detects class file add/delete)
      depAnalysisStats: Map[Path, DepAnalysisStat], // outputDir → dep analysis mtime + content hash
      optionsHash: Long,
      cachedResult: ProjectCompileSuccess
  )

  /** True on macOS/Linux. False on Windows where Java's `unix:ctime` attribute is unsupported. When false, [[isNoop]] short-circuits to None and the manifest
    * mechanism is fully disabled.
    */
  val ctimeAvailable: Boolean =
    !System.getProperty("os.name", "").toLowerCase.contains("win")

  private val NoopManifestMagic: Int = 0x4e4f4f50
  // v4: sourceDirStats records source roots *and every nested subdirectory*. v3 recorded
  // only the top-level roots, so a file added in a nested package dir bumped no recorded
  // mtime and the project was wrongly declared a noop. v3 manifests are a cache miss.
  private val NoopManifestVersion: Byte = 6

  /** Path of the manifest file, sibling to the analysis file. */
  def manifestPath(analysisFile: Path): Path =
    analysisFile.resolveSibling("noop-manifest.bin")

  /** Stat a file: read ctime, mtime, size. */
  def statFile(path: Path): FileStatEntry = {
    val attrs = Files.readAttributes(path, classOf[BasicFileAttributes])
    val ctimeAttr = Files.getAttribute(path, "unix:ctime").asInstanceOf[java.nio.file.attribute.FileTime]
    FileStatEntry(
      ctimeMillis = ctimeAttr.toMillis,
      mtimeMillis = attrs.lastModifiedTime().toMillis,
      size = attrs.size(),
      contentHash = 0L
    )
  }

  /** As [[statFile]], plus a content hash. Only for source FILES — directories have no content and never take this path. */
  def statSourceFile(path: Path): FileStatEntry = statFile(path).copy(contentHash = hashContent(path))

  /** FNV-1a over the file's bytes. Not cryptographic: it only has to change when the content does, and a source file we mis-hash costs a recompile, not a wrong
    * answer — zinc still does the real comparison.
    */
  def hashContent(path: Path): Long =
    try {
      var h = 0xcbf29ce484222325L
      val prime = 0x100000001b3L
      val in = Files.newInputStream(path)
      try {
        val buf = new Array[Byte](64 * 1024)
        var n = in.read(buf)
        while (n > 0) {
          var i = 0
          while (i < n) { h ^= (buf(i) & 0xffL); h *= prime; i += 1 }
          n = in.read(buf)
        }
      } finally in.close()
      // Never collide with "not hashed".
      if (h == 0L) 1L else h
    } catch { case _: Exception => 0L }

  /** Drop source directories that are nested under another source directory. E.g. given {src, src/java}, drop src/java since walking src already covers it. */
  def removeNestedDirs(dirs: Set[Path]): Set[Path] = {
    val abs = dirs.map(_.toAbsolutePath.normalize())
    abs.filter(d => !abs.exists(other => other != d && d.startsWith(other)))
  }

  /** Every directory at or below `root`, inclusive. Empty when `root` is not a directory.
    *
    * Recording nested dirs (not just the root) is what makes file *additions* detectable by stat alone: adding `src/java/com/foo/New.java` bumps the mtime of
    * `src/java/com/foo` only — no ancestor's mtime changes — so a manifest holding just `src/java` cannot see it.
    */
  def collectDirsRecursively(root: Path): Array[Path] =
    if (!Files.isDirectory(root)) Array.empty
    else {
      import scala.jdk.CollectionConverters.*
      scala.util.Using.resource(Files.walk(root)) { stream =>
        stream.iterator().asScala.filter(Files.isDirectory(_)).toArray
      }
    }

  /** FNV-1a hash of compiler options — and of the [[OutputDeterminants]], which decide the output just as much — for cheap equality check. */
  def computeOptionsHash(language: ProjectLanguage.ScalaJava, ecjVersion: Option[String], determinants: OutputDeterminants): Long = {
    var hash = 0xcbf29ce484222325L
    val prime = 0x100000001b3L

    def mix(s: String): Unit = {
      var i = 0
      while (i < s.length) {
        hash ^= s.charAt(i).toLong
        hash *= prime
        i += 1
      }
      hash ^= 0L // separator
      hash *= prime
    }

    mix(language.scalaVersion)
    language.scalaOptions.foreach(mix)
    language.javaOptions.foreach(mix)
    ecjVersion.foreach(mix)
    determinants.asZincExtra.foreach { case (k, v) => mix(k); mix(v) }
    determinants.transformedClasses.toList.sortBy(_._1).foreach { case (name, c) => mix(name); mix(c.kind.value); mix(c.hash) }
    hash
  }

  /** Build + write a manifest for `to` by rebasing `from`'s own manifest, used by `copy-state` after cloning a project's compiled state into a sibling
    * worktree. Unlike [[regenerateFromLocal]]'s other two callers, a directory clone comes with no proof that `to`'s actual source files are what `from`'s
    * analysis was compiled from: `ZincBridge` calls `regenerateFromLocal` right after a real compile ran against these exact files, and `RemoteCache.Pull` only
    * reaches it after an exact digest match already proved file-set-and-content equality. A clone proves neither — `to` is a sibling worktree that can
    * genuinely be on different, locally-modified sources (the "dirty parent" case).
    *
    * So only the source side gets re-verified here: every file `from`'s manifest recorded is re-hashed at its rebased path in `to`, and the source directories
    * are re-walked to confirm nothing was added. Output directories and dependency analyses are not re-proven this way — they are copied build artifacts,
    * correct by construction of the clone, so they are simply re-stat'd/re-hashed fresh off `to`'s disk, the same as [[regenerateFromLocal]] already does for
    * its own callers.
    *
    * Returns `None` (writing nothing) the moment anything about the source files doesn't match, or `from` has no manifest to rebase in the first place (ctime
    * unavailable there, or one was never written) — the existing behavior either way: a real Zinc round-trip on `to`'s first compile, never a false noop from
    * trusting a mismatch.
    */
  def regenerateFromCopy(fromAnalysisFile: Path, fromBuildDir: Path, toAnalysisFile: Path, toBuildDir: Path): Option[NoopManifest] = {
    if (!ctimeAvailable) return None

    def rebase(p: Path): Path = toBuildDir.resolve(fromBuildDir.relativize(p))

    load(fromAnalysisFile) match {
      case None               => None
      case Some(fromManifest) =>
        // Every recorded source file must still be there, at its rebased path, with identical content.
        val sourceStatsBuilder = Map.newBuilder[Path, FileStatEntry]
        var sourcesOk = true
        val sourceIter = fromManifest.sourceStats.iterator
        while (sourcesOk && sourceIter.hasNext) {
          val (fromPath, fromStat) = sourceIter.next()
          val toPath = rebase(fromPath)
          if (!Files.isRegularFile(toPath)) sourcesOk = false
          else {
            val toStat = statSourceFile(toPath)
            // Size first: free (both stats already in hand) and strictly stronger than the hash alone —
            // the same order checkNoopFromDirs/checkNoopManifest already check in, at real compile time.
            if (toStat.size != fromStat.size || toStat.contentHash != fromStat.contentHash) sourcesOk = false
            else sourceStatsBuilder += toPath -> toStat
          }
        }
        if (!sourcesOk) return None

        // And no NEW source file must have appeared that `from` never saw: count every `.scala`/`.java` file actually in the rebased source roots, and
        // compare against how many `from` recorded. Combined with every recorded file having just been confirmed present above, equal counts mean equal
        // sets — the cheap way to ask "exactly these files, nothing more" without materializing and diffing two path sets.
        val sourceRoots = removeNestedDirs(fromManifest.sourceDirStats.keySet.map(rebase))
        val actualSourceFileCount = sourceRoots.iterator.map { root =>
          if (!Files.isDirectory(root)) 0
          else
            scala.util
              .Using(Files.walk(root)) { stream =>
                import scala.jdk.StreamConverters.*
                stream.toScala(List).count(p => Files.isRegularFile(p) && (p.toString.endsWith(".scala") || p.toString.endsWith(".java")))
              }
              .getOrElse(0)
        }.sum
        if (actualSourceFileCount != fromManifest.sourceStats.size) return None

        // Source directories themselves, re-stat'd fresh (not reused from `from`: a directory's own mtime is per-filesystem-entry, never portable).
        val sourceDirStatsBuilder = Map.newBuilder[Path, FileStatEntry]
        var dirsOk = true
        val dirIter = fromManifest.sourceDirStats.keysIterator
        while (dirsOk && dirIter.hasNext) {
          val toPath = rebase(dirIter.next())
          if (!Files.isDirectory(toPath)) dirsOk = false
          else sourceDirStatsBuilder += toPath -> statFile(toPath)
        }
        if (!dirsOk) return None

        // Output directories and dependency analyses: copied artifacts, not git-tracked sources — nothing to re-verify content against, just re-stat/
        // re-hash what the clone actually put on disk.
        val outputDirStatsBuilder = Map.newBuilder[Path, FileStatEntry]
        var outputOk = true
        val outIter = fromManifest.outputDirStats.keysIterator
        while (outputOk && outIter.hasNext) {
          val toPath = rebase(outIter.next())
          if (!Files.isDirectory(toPath)) outputOk = false
          else outputDirStatsBuilder += toPath -> statFile(toPath)
        }
        if (!outputOk) return None

        // A dependency's analysis, re-stat'd the same way as our own output dir above. A missing file
        // here must fail the whole regeneration, not paper over it with a sentinel — matching every
        // other check in this function. Callers that process several projects in one copy (like
        // copy-state's own `copyProject`) MUST have already cloned every project's state before calling
        // this for any of them, precisely so a dependency is never "not there yet" by construction —
        // see MultiWorkspaceBspServer.handleCopyState's two-phase (clone-all, then regenerate-all)
        // structure. If this still finds one missing, something is actually wrong (or absent) in `to`.
        val depStatsBuilder = Map.newBuilder[Path, DepAnalysisStat]
        var depsOk = true
        val depIter = fromManifest.depAnalysisStats.keysIterator
        while (depsOk && depIter.hasNext) {
          val fromOutputDir = depIter.next()
          val toOutputDir = rebase(fromOutputDir)
          val toDepAnalysisFile = toOutputDir.getParent.resolve(".zinc").resolve("analysis.zip")
          if (!Files.exists(toDepAnalysisFile)) depsOk = false
          else depStatsBuilder += toOutputDir -> DepAnalysisStat(Files.getLastModifiedTime(toDepAnalysisFile).toMillis, hashContent(toDepAnalysisFile))
        }
        if (!depsOk) return None

        val toOutputDir = rebase(fromManifest.cachedResult.outputDir)
        val toClassFiles: Set[Path] =
          if (!Files.isDirectory(toOutputDir)) Set.empty
          else
            scala.util
              .Using(Files.walk(toOutputDir)) { stream =>
                import scala.jdk.StreamConverters.*
                stream.toScala(List).filter(p => Files.isRegularFile(p) && p.toString.endsWith(".class")).toSet
              }
              .getOrElse(Set.empty)

        val manifest = NoopManifest(
          sourceStats = sourceStatsBuilder.result(),
          sourceDirStats = sourceDirStatsBuilder.result(),
          outputDirStats = outputDirStatsBuilder.result(),
          depAnalysisStats = depStatsBuilder.result(),
          optionsHash = fromManifest.optionsHash, // config, not content — unchanged if wrong, loadAndValidateManifest declines rather than trusts
          cachedResult = ProjectCompileSuccess(toOutputDir, toClassFiles, fromManifest.cachedResult.analysisFile.map(rebase))
        )

        write(toAnalysisFile, manifest)
        Some(manifest)
    }
  }

  /** Build + write a manifest by stat'ing the local filesystem. Used by both ZincBridge (post-compile) and RemoteCache.Pull (post-extraction).
    *
    * Returns the constructed manifest so the caller can populate any in-memory cache. No-op on Windows.
    */
  def regenerateFromLocal(
      analysisFile: Path,
      sourceDirs: Set[Path],
      sourceFiles: Array[Path],
      dependencyAnalyses: Map[Path, Path],
      language: ProjectLanguage.ScalaJava,
      ecjVersion: Option[String],
      determinants: OutputDeterminants,
      result: ProjectCompileSuccess
  ): Option[NoopManifest] = {
    if (!ctimeAvailable) return None

    val sourceStats = new java.util.HashMap[Path, FileStatEntry](sourceFiles.length * 2)
    var i = 0
    while (i < sourceFiles.length) {
      val path = sourceFiles(i)
      sourceStats.put(path, statSourceFile(path))
      i += 1
    }

    // Stat source directories and every nested subdirectory — a dir's mtime changes when a
    // file is added/deleted/renamed directly inside it (POSIX), but NOT when something
    // changes in a descendant. Recording only the roots therefore misses additions in
    // package subdirectories, which is exactly what code generators produce.
    val sourceDirStatsMap = removeNestedDirs(sourceDirs).iterator
      .flatMap(collectDirsRecursively)
      .map(dir => dir -> statFile(dir))
      .toMap

    val depStats = dependencyAnalyses.map { case (outputDir, depAnalysisFile) =>
      val stat =
        if (Files.exists(depAnalysisFile))
          DepAnalysisStat(Files.getLastModifiedTime(depAnalysisFile).toMillis, hashContent(depAnalysisFile))
        else DepAnalysisStat(0L, 0L)
      outputDir -> stat
    }

    // Stat output directories (outputDir + all subdirs).
    // Directory mtime changes on file add/delete — catches class file deletion cheaply.
    val outputDirStatsMap: Map[Path, FileStatEntry] =
      collectDirsRecursively(result.outputDir).map(d => d -> statFile(d)).toMap

    val manifest = NoopManifest(
      sourceStats = {
        import scala.jdk.CollectionConverters.*
        sourceStats.asScala.toMap
      },
      sourceDirStats = sourceDirStatsMap,
      outputDirStats = outputDirStatsMap,
      depAnalysisStats = depStats,
      optionsHash = computeOptionsHash(language, ecjVersion, determinants),
      cachedResult = result
    )

    write(analysisFile, manifest)
    Some(manifest)
  }

  /** Serialize a manifest to disk via tmp + atomic rename. */
  def write(analysisFile: Path, manifest: NoopManifest): Unit = {
    val target = manifestPath(analysisFile)
    val tmpFile = target.resolveSibling(target.getFileName.toString + ".tmp")
    val out = new DataOutputStream(new java.io.BufferedOutputStream(Files.newOutputStream(tmpFile)))
    try {
      out.writeInt(NoopManifestMagic)
      out.writeByte(NoopManifestVersion)
      out.writeLong(manifest.optionsHash)

      out.writeInt(manifest.sourceStats.size)
      manifest.sourceStats.foreach { case (path, stat) =>
        out.writeUTF(path.toString)
        out.writeLong(stat.ctimeMillis)
        out.writeLong(stat.mtimeMillis)
        out.writeLong(stat.size)
        out.writeLong(stat.contentHash)
      }

      out.writeInt(manifest.sourceDirStats.size)
      manifest.sourceDirStats.foreach { case (dir, stat) =>
        out.writeUTF(dir.toString)
        out.writeLong(stat.ctimeMillis)
        out.writeLong(stat.mtimeMillis)
        out.writeLong(stat.size)
      }

      out.writeInt(manifest.outputDirStats.size)
      manifest.outputDirStats.foreach { case (dir, stat) =>
        out.writeUTF(dir.toString)
        out.writeLong(stat.ctimeMillis)
        out.writeLong(stat.mtimeMillis)
        out.writeLong(stat.size)
      }

      out.writeInt(manifest.depAnalysisStats.size)
      manifest.depAnalysisStats.foreach { case (outputDir, stat) =>
        out.writeUTF(outputDir.toString)
        out.writeLong(stat.mtimeMillis)
        out.writeLong(stat.contentHash)
      }

      out.writeUTF(manifest.cachedResult.outputDir.toString)
      val analysisPath = manifest.cachedResult.analysisFile match {
        case Some(p) => p.toString
        case None    => ""
      }
      out.writeUTF(analysisPath)
      out.writeInt(manifest.cachedResult.classFiles.size)
      manifest.cachedResult.classFiles.foreach(cf => out.writeUTF(cf.toString))

      out.flush()
    } finally out.close()

    val _ = Files.move(tmpFile, target, StandardCopyOption.REPLACE_EXISTING, StandardCopyOption.ATOMIC_MOVE)
  }

  /** Load a manifest from disk. Returns None if the file doesn't exist or has a stale format version. */
  def load(analysisFile: Path): Option[NoopManifest] = {
    val target = manifestPath(analysisFile)
    if (!Files.exists(target)) return None

    val in = new DataInputStream(new java.io.BufferedInputStream(Files.newInputStream(target)))
    try {
      val magic = in.readInt()
      if (magic != NoopManifestMagic)
        throw new IllegalStateException(s"Bad noop-manifest magic: 0x${magic.toHexString}, expected 0x${NoopManifestMagic.toHexString}")
      val version = in.readByte()
      if (version != NoopManifestVersion) return None
      val optionsHash = in.readLong()

      val sourceCount = in.readInt()
      val sourceStatsBuilder = Map.newBuilder[Path, FileStatEntry]
      sourceStatsBuilder.sizeHint(sourceCount)
      var i = 0
      while (i < sourceCount) {
        val path = Path.of(in.readUTF())
        val ctime = in.readLong()
        val mtime = in.readLong()
        val size = in.readLong()
        val hash = in.readLong()
        sourceStatsBuilder += path -> FileStatEntry(ctime, mtime, size, hash)
        i += 1
      }

      val dirCount = in.readInt()
      val sourceDirStatsBuilder = Map.newBuilder[Path, FileStatEntry]
      sourceDirStatsBuilder.sizeHint(dirCount)
      i = 0
      while (i < dirCount) {
        val dir = Path.of(in.readUTF())
        val dirCtime = in.readLong()
        val dirMtime = in.readLong()
        val dirSize = in.readLong()
        sourceDirStatsBuilder += dir -> FileStatEntry(dirCtime, dirMtime, dirSize, 0L)
        i += 1
      }

      val outDirCount = in.readInt()
      val outputDirStatsBuilder = Map.newBuilder[Path, FileStatEntry]
      outputDirStatsBuilder.sizeHint(outDirCount)
      i = 0
      while (i < outDirCount) {
        val dir = Path.of(in.readUTF())
        val outCtime = in.readLong()
        val outMtime = in.readLong()
        val outSize = in.readLong()
        outputDirStatsBuilder += dir -> FileStatEntry(outCtime, outMtime, outSize, 0L)
        i += 1
      }

      val depCount = in.readInt()
      val depStatsBuilder = Map.newBuilder[Path, DepAnalysisStat]
      depStatsBuilder.sizeHint(depCount)
      i = 0
      while (i < depCount) {
        val outputDir = Path.of(in.readUTF())
        val mtime = in.readLong()
        val depHash = in.readLong()
        depStatsBuilder += outputDir -> DepAnalysisStat(mtime, depHash)
        i += 1
      }

      val outputDir = Path.of(in.readUTF())
      val analysisPathStr = in.readUTF()
      val cachedAnalysisFile = if (analysisPathStr.isEmpty) None else Some(Path.of(analysisPathStr))
      val classFileCount = in.readInt()
      val classFilesBuilder = Set.newBuilder[Path]
      classFilesBuilder.sizeHint(classFileCount)
      i = 0
      while (i < classFileCount) {
        classFilesBuilder += Path.of(in.readUTF())
        i += 1
      }

      Some(
        NoopManifest(
          sourceStats = sourceStatsBuilder.result(),
          sourceDirStats = sourceDirStatsBuilder.result(),
          outputDirStats = outputDirStatsBuilder.result(),
          depAnalysisStats = depStatsBuilder.result(),
          optionsHash = optionsHash,
          cachedResult = ProjectCompileSuccess(outputDir, classFilesBuilder.result(), cachedAnalysisFile)
        )
      )
    } finally in.close()
  }
}
