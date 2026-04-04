package bleep.analysis

import cats.effect.IO
import java.io.{DataInputStream, DataOutputStream, File, IOException}
import java.lang.ref.SoftReference
import java.nio.file.{Files, Path, StandardCopyOption, StandardOpenOption}
import java.nio.file.attribute.BasicFileAttributes
import java.util.Optional
import scala.jdk.CollectionConverters.*
import scala.jdk.OptionConverters.*
import sbt.internal.inc.*
import sbt.internal.inc.classpath.ClassLoaderCache
import xsbti.*
import xsbti.compile.{ScalaInstance as ZincScalaInstance, *}

/** Listener for compilation progress updates */
trait ProgressListener {

  /** Called when compilation makes progress.
    * @param current
    *   current number of units compiled
    * @param total
    *   total number of units to compile
    * @param phase
    *   current compiler phase
    * @return
    *   true to continue, false to cancel
    */
  def onProgress(current: Int, total: Int, phase: String): Boolean
}

object ProgressListener {
  val noop: ProgressListener = (_, _, _) => true
}

/** Bridge to Zinc incremental compiler.
  *
  * Zinc provides battle-tested incremental compilation for Scala/Java:
  *   - Name hashing for precise API change detection
  *   - Transitive dependency invalidation
  *   - Analysis persistence for cross-session incrementality
  *
  * This bridge handles:
  *   1. Creating Zinc-compatible ScalaInstance from our resolved JARs
  *   2. Resolving the compiler bridge for each Scala version
  *   3. Running the incremental compiler
  *   4. Converting results back to our types
  */
object ZincBridge {

  /** Drop source directories that are nested under another source directory. E.g. given {src, src/java}, drop src/java since walking src already covers it.
    */
  private[analysis] def removeNestedDirs(dirs: Set[Path]): Set[Path] = {
    val abs = dirs.map(_.toAbsolutePath.normalize())
    abs.filter(d => !abs.exists(other => other != d && d.startsWith(other)))
  }

  // ─── Cached compiler infrastructure ───────────────────────────────────────
  // Safe to share: immutable values and stateless objects.
  // NOT cached: Compilers/AnalyzingCompiler — their ClassLoaderCache uses HashMap
  // (not thread-safe) so they must be created fresh per compilation.
  // The Scala compiler classes themselves are cached in ScalaInstance.loader()
  // (via CompilerResolver.instanceCache), so fresh ClassLoaderCache only re-loads
  // the small bridge classes, not the full compiler.

  /** Singleton incremental compiler — stateless, thread-safe. */
  private lazy val incrementalCompiler: IncrementalCompiler = ZincUtil.defaultIncrementalCompiler

  /** Cached ZincScalaInstance per Scala version. Immutable, safe to share. */
  private val scalaInstanceCache = new java.util.concurrent.ConcurrentHashMap[String, ZincScalaInstance]()

  /** Cached bridge jar per Scala version. Avoids coursier I/O on every compile. */
  private val bridgeCache = new java.util.concurrent.ConcurrentHashMap[String, Path]()

  /** Soft-reference cache for dependency analyses. Entries are evicted under memory pressure. Key: analysis file path. Value: SoftReference to parsed
    * CompileAnalysis. This avoids re-reading 10-50MB analysis files from disk when the same dependency is loaded by multiple projects within the same build,
    * while allowing GC to reclaim them when heap is tight.
    */
  private val analysisCache = new java.util.concurrent.ConcurrentHashMap[Path, SoftReference[CompileAnalysis]]()

  /** ReadWriteMappers for portable zinc analysis files.
    *
    * Uses RootPaths with three machine-dependent roots: source root (build dir), library root (coursier cache), product root (build output dir). Zinc
    * relativizes these paths on write and rebases on read, making analysis files portable across machines.
    *
    * Computed lazily and cached — the roots are constant for the lifetime of the JVM.
    */
  private lazy val analysisMappers: xsbti.compile.analysis.ReadWriteMappers = {
    // Build dir: inferred from the bleep BSP socket directory convention
    // For the BSP server, the build dir is set when handling initialize.
    // As a fallback, use cwd.
    val buildDir = analysisBuildDir.get()
    val coursierCache = coursier.cache.CacheDefaults.location.toPath
    val rootPaths = xsbti.compile.analysis.RootPaths.getPaths(
      buildDir.toFile,
      coursierCache.toFile,
      buildDir.toFile // products are under buildDir (.bleep/builds/normal/.bloop/*/classes)
    )
    new xsbti.compile.analysis.ReadWriteMappers(
      xsbti.compile.analysis.ReadMapper.getMachineIndependentMapper(rootPaths),
      xsbti.compile.analysis.WriteMapper.getMachineIndependentMapper(rootPaths)
    )
  }

  /** The build directory, set by the BSP server during initialize. Used for analysis mappers. */
  private val analysisBuildDir = new java.util.concurrent.atomic.AtomicReference[Path](Path.of(System.getProperty("user.dir")))

  /** Set the build directory for analysis mappers. Called by MultiWorkspaceBspServer during initialize. */
  def setBuildDir(buildDir: Path): Unit =
    analysisBuildDir.set(buildDir)

  // ─── Pre-Zinc noop detection via unix:ctime manifest ──────────────────────
  // On macOS/Linux, st_ctime (inode change time) is kernel-managed and updated
  // by ALL file operations (cp, mv, rsync -a, tar x, touch -t, echo >, git ops).
  // Non-root users cannot fake it. This gives a reliable "was this file touched?"
  // signal without reading file contents (~3-5μs/call vs ~3ms/file for FarmHash).

  private case class FileStatEntry(ctimeMillis: Long, mtimeMillis: Long, size: Long)

  private case class NoopManifest(
      sourceStats: Map[Path, FileStatEntry],
      sourceDirStats: Map[Path, FileStatEntry], // source dir → stat (detects file add/delete)
      outputDirStats: Map[Path, FileStatEntry], // output dir + subdirs → stat (detects class file add/delete)
      depAnalysisStats: Map[Path, Long], // outputDir → dep analysis mtime millis
      optionsHash: Long,
      cachedResult: ProjectCompileSuccess
  )

  private val noopManifestCache = new java.util.concurrent.ConcurrentHashMap[Path, NoopManifest]()
  private val ctimeAvailable: Boolean =
    !System.getProperty("os.name", "").toLowerCase.contains("win")

  private val NoopManifestMagic: Int = 0x4e4f4f50
  private val NoopManifestVersion: Byte = 3

  private val debugLogFile = Path.of(System.getProperty("user.home"), ".bleep", "zinc-debug.log")
  private val debugEnabled = System.getProperty("bleep.zinc.debug", "false").toBoolean

  private[analysis] def debug(msg: String): Unit = {
    // Only print to stderr if debug is explicitly enabled via -Dbleep.zinc.debug=true
    if (debugEnabled) {
      System.err.println(s"[ZincBridge-DEBUG] $msg")
    }
    // Write to file for diagnostics (silent failure)
    try {
      Files.createDirectories(debugLogFile.getParent)
      val _ = Files.writeString(
        debugLogFile,
        s"[${java.time.LocalDateTime.now}] $msg\n",
        StandardOpenOption.CREATE,
        StandardOpenOption.APPEND
      )
    } catch {
      case _: Exception => () // Silent failure for file logging
    }
  }

  /** Check if a project is a noop (nothing changed since last successful compile).
    *
    * Uses the ctime manifest to detect changes without any Zinc work. Intended to be called BEFORE acquiring compile semaphore / heap pressure gate so that
    * noop projects don't consume concurrency slots or wait for memory.
    *
    * @return
    *   Some(result) if noop, None if compilation is needed
    */
  def isNoop(
      config: ProjectConfig,
      language: ProjectLanguage.ScalaJava,
      dependencyAnalyses: Map[Path, Path],
      ecjVersion: Option[String]
  ): Option[ProjectCompileSuccess] = {
    val analysisDir = config.analysisDir.getOrElse(config.outputDir.resolve(".zinc"))
    val analysisFile = analysisDir.resolve("analysis.zip")
    checkNoopFromDirs(analysisFile, config.sources, dependencyAnalyses, language, ecjVersion)
  }

  /** Compile a Scala/Java project using Zinc.
    *
    * @param config
    *   project configuration
    * @param language
    *   Scala/Java language settings
    * @param diagnosticListener
    *   receives compilation diagnostics
    * @param cancellationToken
    *   for cancellation support
    * @param dependencyAnalyses
    *   analysis files from dependent projects (keyed by output dir)
    * @param progressListener
    *   receives compilation progress updates
    * @param ecjVersion
    *   optional ECJ version - if set, uses ECJ instead of javac
    * @return
    *   compilation result
    */
  def compile(
      config: ProjectConfig,
      language: ProjectLanguage.ScalaJava,
      diagnosticListener: DiagnosticListener,
      cancellationToken: CancellationToken,
      dependencyAnalyses: Map[Path, Path],
      progressListener: ProgressListener,
      ecjVersion: Option[String] = None
  ): IO[ProjectCompileResult] = IO.interruptible {
    debug(s"[ZincBridge] compile() called for ${config.name}")
    Files.createDirectories(config.outputDir)

    val analysisDir = config.analysisDir.getOrElse(config.outputDir.resolve(".zinc"))
    Files.createDirectories(analysisDir)
    val analysisFile = analysisDir.resolve("analysis.zip")

    val sources = collectSources(config.sources)
    if (sources.isEmpty) {
      ProjectCompileSuccess(config.outputDir, Set.empty, Some(analysisFile))
    } else {
      compileOnce(config, sources, language, diagnosticListener, cancellationToken, dependencyAnalyses, progressListener, ecjVersion, analysisFile)
    }
  }

  // ─── Noop manifest: check ────────────────────────────────────────────────

  /** Load and validate the noop manifest: cache lookup → disk fallback → dep analysis check → output dir check.
    *
    * Returns the validated manifest, or None if any shared validation fails. Callers still need to check sources.
    */
  private def loadAndValidateManifest(
      analysisFile: Path,
      dependencyAnalyses: Map[Path, Path],
      language: ProjectLanguage.ScalaJava,
      ecjVersion: Option[String]
  ): Option[NoopManifest] = {
    if (!ctimeAvailable) return None

    val manifest = {
      val mem = noopManifestCache.get(analysisFile)
      if (mem != null) mem
      else {
        loadNoopManifest(analysisFile) match {
          case Some(disk) =>
            noopManifestCache.put(analysisFile, disk)
            disk
          case None => return None
        }
      }
    }

    // Options hash (cheap)
    val currentHash = computeOptionsHash(language, ecjVersion)
    if (currentHash != manifest.optionsHash) return None

    // Dependency analysis mtimes
    if (dependencyAnalyses.size != manifest.depAnalysisStats.size) return None
    val depIter = dependencyAnalyses.iterator
    while (depIter.hasNext) {
      val (outputDir, depAnalysisFile) = depIter.next()
      manifest.depAnalysisStats.get(outputDir) match {
        case None => return None
        case Some(expectedMtime) =>
          if (!Files.exists(depAnalysisFile)) return None
          val actualMtime = Files.getLastModifiedTime(depAnalysisFile).toMillis
          if (actualMtime != expectedMtime) return None
      }
    }

    // Output validation: verify that output directories haven't changed.
    // Directory mtime changes when files are added/deleted/renamed (POSIX guarantee).
    // This catches: bleep clean, manual rm, individual class file deletion, external tools.
    // Much cheaper than checking individual class files (5-20 dirs vs hundreds/thousands of files).
    if (manifest.outputDirStats.isEmpty) {
      // No output dirs recorded (empty compile or old manifest) — skip check
    } else {
      val outIter = manifest.outputDirStats.iterator
      while (outIter.hasNext) {
        val (dir, expected) = outIter.next()
        if (!Files.isDirectory(dir)) {
          noopManifestCache.remove(analysisFile)
          return None
        }
        val stat = statFile(dir)
        if (stat.ctimeMillis != expected.ctimeMillis || stat.mtimeMillis != expected.mtimeMillis) {
          noopManifestCache.remove(analysisFile)
          return None
        }
      }
    }
    // Also verify analysis file still exists
    if (!Files.exists(analysisFile)) {
      noopManifestCache.remove(analysisFile)
      return None
    }

    Some(manifest)
  }

  /** Check noop using source DIRECTORIES (no file walk).
    *
    * Instead of walking directories to collect sources, this checks:
    *   1. Source directory stats (mtime changes when files are added/deleted/renamed — POSIX guarantee)
    *   2. Per-source file stats from the manifest's recorded paths
    *
    * This avoids the O(files) directory walk entirely, making the noop check O(dirs + recorded_files) with pure stat calls.
    */
  private def checkNoopFromDirs(
      analysisFile: Path,
      sourceDirs: Set[Path],
      dependencyAnalyses: Map[Path, Path],
      language: ProjectLanguage.ScalaJava,
      ecjVersion: Option[String]
  ): Option[ProjectCompileSuccess] = {
    val manifest = loadAndValidateManifest(analysisFile, dependencyAnalyses, language, ecjVersion) match {
      case Some(m) => m
      case None    => return None
    }

    // Check source directory stats — detects file add/delete/rename
    val normalizedDirs = removeNestedDirs(sourceDirs)
    if (normalizedDirs.size != manifest.sourceDirStats.size) return None
    val dirIter = normalizedDirs.iterator
    while (dirIter.hasNext) {
      val dir = dirIter.next()
      manifest.sourceDirStats.get(dir) match {
        case None => return None
        case Some(expected) =>
          if (!Files.isDirectory(dir)) return None
          val stat = statFile(dir)
          if (
            stat.ctimeMillis != expected.ctimeMillis ||
            stat.mtimeMillis != expected.mtimeMillis
          ) return None
      }
    }

    // Check per-source file stats (from manifest's recorded paths — no directory walk)
    val srcIter = manifest.sourceStats.iterator
    while (srcIter.hasNext) {
      val (path, expected) = srcIter.next()
      if (!Files.exists(path)) return None
      val stat = statFile(path)
      if (
        stat.ctimeMillis != expected.ctimeMillis ||
        stat.mtimeMillis != expected.mtimeMillis ||
        stat.size != expected.size
      ) return None
    }

    Some(manifest.cachedResult)
  }

  /** Check noop using an already-collected source file array.
    *
    * Check order (cheapest first):
    *   1. Shared validation (ctime, manifest load, options hash, deps, output dir)
    *   2. Source count match
    *   3. Per-source (ctime, mtime, size) check
    *
    * Any mismatch → None (fall through to Zinc).
    */
  private def checkNoopManifest(
      analysisFile: Path,
      sources: Array[VirtualFile],
      dependencyAnalyses: Map[Path, Path],
      language: ProjectLanguage.ScalaJava,
      ecjVersion: Option[String]
  ): Option[ProjectCompileSuccess] = {
    val manifest = loadAndValidateManifest(analysisFile, dependencyAnalyses, language, ecjVersion) match {
      case Some(m) => m
      case None    => return None
    }

    // Source count
    if (sources.length != manifest.sourceStats.size) return None

    // Per-source stat check
    val srcIter = sources.iterator
    while (srcIter.hasNext) {
      val vf = srcIter.next()
      val path = vf match {
        case pvf: PlainVirtualFile => pvf.path
        case other                 => Path.of(other.id())
      }
      manifest.sourceStats.get(path) match {
        case None => return None
        case Some(expected) =>
          val stat = statFile(path)
          if (
            stat.ctimeMillis != expected.ctimeMillis ||
            stat.mtimeMillis != expected.mtimeMillis ||
            stat.size != expected.size
          ) return None
      }
    }

    Some(manifest.cachedResult)
  }

  /** Stat a file: read ctime, mtime, size. */
  private def statFile(path: Path): FileStatEntry = {
    val attrs = Files.readAttributes(path, classOf[BasicFileAttributes])
    val ctimeAttr = Files.getAttribute(path, "unix:ctime").asInstanceOf[java.nio.file.attribute.FileTime]
    FileStatEntry(
      ctimeMillis = ctimeAttr.toMillis,
      mtimeMillis = attrs.lastModifiedTime().toMillis,
      size = attrs.size()
    )
  }

  /** FNV-1a hash of compiler options for cheap equality check. */
  private def computeOptionsHash(language: ProjectLanguage.ScalaJava, ecjVersion: Option[String]): Long = {
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
    language.javaRelease.foreach(r => mix(r.toString))
    language.javaOptions.foreach(mix)
    ecjVersion.foreach(mix)
    hash
  }

  // ─── Noop manifest: save ────────────────────────────────────────────────

  /** Save a noop manifest after successful compilation. */
  private def saveNoopManifest(
      analysisFile: Path,
      sourceDirs: Set[Path],
      sources: Array[VirtualFile],
      dependencyAnalyses: Map[Path, Path],
      language: ProjectLanguage.ScalaJava,
      ecjVersion: Option[String],
      result: ProjectCompileSuccess
  ): Unit = {
    if (!ctimeAvailable) return

    val sourceStats = new java.util.HashMap[Path, FileStatEntry](sources.length * 2)
    var i = 0
    while (i < sources.length) {
      val vf = sources(i)
      val path = vf match {
        case pvf: PlainVirtualFile => pvf.path
        case other                 => Path.of(other.id())
      }
      val stat = statFile(path)
      sourceStats.put(path, stat)
      i += 1
    }

    // Stat source directories — mtime changes on file add/delete/rename (POSIX)
    val normalizedDirs = removeNestedDirs(sourceDirs)
    val sourceDirStatsMap = normalizedDirs.flatMap { dir =>
      if (Files.isDirectory(dir)) Some(dir -> statFile(dir))
      else None
    }.toMap

    val depStats = dependencyAnalyses.map { case (outputDir, depAnalysisFile) =>
      val mtime = if (Files.exists(depAnalysisFile)) Files.getLastModifiedTime(depAnalysisFile).toMillis else 0L
      outputDir -> mtime
    }

    // Stat output directories (outputDir + all subdirs).
    // Directory mtime changes on file add/delete — catches class file deletion cheaply.
    val outputDirStatsMap: Map[Path, FileStatEntry] =
      if (Files.isDirectory(result.outputDir)) {
        import scala.jdk.CollectionConverters.*
        val dirs = scala.util
          .Using(Files.walk(result.outputDir)) { stream =>
            stream.iterator().asScala.filter(Files.isDirectory(_)).toArray
          }
          .getOrElse(Array.empty[Path])
        dirs.map(d => d -> statFile(d)).toMap
      } else Map.empty

    val manifest = NoopManifest(
      sourceStats = {
        import scala.jdk.CollectionConverters.*
        sourceStats.asScala.toMap
      },
      sourceDirStats = sourceDirStatsMap,
      outputDirStats = outputDirStatsMap,
      depAnalysisStats = depStats,
      optionsHash = computeOptionsHash(language, ecjVersion),
      cachedResult = result
    )

    noopManifestCache.put(analysisFile, manifest)
    writeNoopManifest(analysisFile, manifest)
  }

  // ─── Noop manifest: disk I/O ───────────────────────────────────────────

  private def manifestPath(analysisFile: Path): Path =
    analysisFile.resolveSibling("noop-manifest.bin")

  private def writeNoopManifest(analysisFile: Path, manifest: NoopManifest): Unit = {
    val target = manifestPath(analysisFile)
    val tmpFile = target.resolveSibling(target.getFileName.toString + ".tmp")
    val out = new DataOutputStream(new java.io.BufferedOutputStream(Files.newOutputStream(tmpFile)))
    try {
      out.writeInt(NoopManifestMagic)
      out.writeByte(NoopManifestVersion)
      out.writeLong(manifest.optionsHash)

      // Sources
      out.writeInt(manifest.sourceStats.size)
      manifest.sourceStats.foreach { case (path, stat) =>
        out.writeUTF(path.toString)
        out.writeLong(stat.ctimeMillis)
        out.writeLong(stat.mtimeMillis)
        out.writeLong(stat.size)
      }

      // Source directories
      out.writeInt(manifest.sourceDirStats.size)
      manifest.sourceDirStats.foreach { case (dir, stat) =>
        out.writeUTF(dir.toString)
        out.writeLong(stat.ctimeMillis)
        out.writeLong(stat.mtimeMillis)
        out.writeLong(stat.size)
      }

      // Output directories
      out.writeInt(manifest.outputDirStats.size)
      manifest.outputDirStats.foreach { case (dir, stat) =>
        out.writeUTF(dir.toString)
        out.writeLong(stat.ctimeMillis)
        out.writeLong(stat.mtimeMillis)
        out.writeLong(stat.size)
      }

      // Dependency analyses
      out.writeInt(manifest.depAnalysisStats.size)
      manifest.depAnalysisStats.foreach { case (outputDir, mtime) =>
        out.writeUTF(outputDir.toString)
        out.writeLong(mtime)
      }

      // Cached result
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

  /** Load noop manifest from disk. Returns None if file doesn't exist or has a stale format version. */
  private def loadNoopManifest(analysisFile: Path): Option[NoopManifest] = {
    val target = manifestPath(analysisFile)
    if (!Files.exists(target)) return None

    val in = new DataInputStream(new java.io.BufferedInputStream(Files.newInputStream(target)))
    try {
      val magic = in.readInt()
      if (magic != NoopManifestMagic)
        throw new IllegalStateException(s"Bad noop-manifest magic: 0x${magic.toHexString}, expected 0x${NoopManifestMagic.toHexString}")
      val version = in.readByte()
      if (version != NoopManifestVersion) return None // stale format — treat as cache miss
      val optionsHash = in.readLong()

      // Sources
      val sourceCount = in.readInt()
      val sourceStatsBuilder = Map.newBuilder[Path, FileStatEntry]
      sourceStatsBuilder.sizeHint(sourceCount)
      var i = 0
      while (i < sourceCount) {
        val path = Path.of(in.readUTF())
        val ctime = in.readLong()
        val mtime = in.readLong()
        val size = in.readLong()
        sourceStatsBuilder += path -> FileStatEntry(ctime, mtime, size)
        i += 1
      }

      // Source directories
      val dirCount = in.readInt()
      val sourceDirStatsBuilder = Map.newBuilder[Path, FileStatEntry]
      sourceDirStatsBuilder.sizeHint(dirCount)
      i = 0
      while (i < dirCount) {
        val dir = Path.of(in.readUTF())
        val dirCtime = in.readLong()
        val dirMtime = in.readLong()
        val dirSize = in.readLong()
        sourceDirStatsBuilder += dir -> FileStatEntry(dirCtime, dirMtime, dirSize)
        i += 1
      }

      // Output directories
      val outDirCount = in.readInt()
      val outputDirStatsBuilder = Map.newBuilder[Path, FileStatEntry]
      outputDirStatsBuilder.sizeHint(outDirCount)
      i = 0
      while (i < outDirCount) {
        val dir = Path.of(in.readUTF())
        val outCtime = in.readLong()
        val outMtime = in.readLong()
        val outSize = in.readLong()
        outputDirStatsBuilder += dir -> FileStatEntry(outCtime, outMtime, outSize)
        i += 1
      }

      // Dependency analyses
      val depCount = in.readInt()
      val depStatsBuilder = Map.newBuilder[Path, Long]
      depStatsBuilder.sizeHint(depCount)
      i = 0
      while (i < depCount) {
        val outputDir = Path.of(in.readUTF())
        val mtime = in.readLong()
        depStatsBuilder += outputDir -> mtime
        i += 1
      }

      // Cached result
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

  /** Run a single compilation attempt. Unexpected Zinc exceptions propagate. On cancellation, the analysis file is preserved so the next compile can be
    * incremental. Corrupt analysis is detected and cleaned automatically.
    */
  private def compileOnce(
      config: ProjectConfig,
      sources: Array[VirtualFile],
      language: ProjectLanguage.ScalaJava,
      diagnosticListener: DiagnosticListener,
      cancellationToken: CancellationToken,
      dependencyAnalyses: Map[Path, Path],
      progressListener: ProgressListener,
      ecjVersion: Option[String],
      analysisFile: Path
  ): ProjectCompileResult = {
    // Fast path: check noop manifest BEFORE any Zinc work (loading analysis,
    // creating compilers, hashing files). This skips ~5s of FarmHash I/O per
    // project on noop builds by using unix:ctime as a reliable change detector.
    checkNoopManifest(analysisFile, sources, dependencyAnalyses, language, ecjVersion) match {
      case Some(cachedResult) =>
        diagnosticListener.onCompilationReason(config.name, CompilationReason.UpToDate)
        return cachedResult
      case None => ()
    }

    val scalaInstance = getScalaInstance(language.scalaVersion)
    val compilers = createCompilers(scalaInstance, language, ecjVersion, cancellationToken, progressListener)
    val logger = new BleepLogger(diagnosticListener)
    val reporter = new BleepReporter(diagnosticListener)

    val previousResult = loadPreviousResult(analysisFile)
    val hasPrevAnalysis = previousResult.analysis().isPresent
    debug(s"[ZincBridge] ${config.name}: ${sources.length} sources, hasPreviousAnalysis=$hasPrevAnalysis, outputDir=${config.outputDir}")

    // Signal reading-analysis phase with tracked API count
    val trackedApis = if (hasPrevAnalysis) {
      previousResult.analysis().get().asInstanceOf[sbt.internal.inc.Analysis].apis.internal.size
    } else 0
    diagnosticListener.onCompilePhase(config.name, CompilePhase.ReadingAnalysis(trackedApis))

    // When there's no analysis but old class files exist (e.g. manual clean or
    // corrupt analysis deletion), we must clear the output directory.
    // Without this, zinc does a clean compile (all sources) but the output dir is
    // on the classpath (for incremental Java compilation), so javac sees both the
    // stale .class files AND the source files → "duplicate class" errors.
    if (!hasPrevAnalysis && Files.exists(config.outputDir)) {
      debug(s"[ZincBridge] No analysis for ${config.name} — clearing stale class files from ${config.outputDir}")
      bleep.internal.FileUtils.deleteDirectory(config.outputDir)
      Files.createDirectories(config.outputDir)
    }

    // Emit CompilationReason so the client display shows "Compiling: project-name ..."
    if (!hasPrevAnalysis) {
      diagnosticListener.onCompilationReason(config.name, CompilationReason.CleanBuild)
    } else {
      diagnosticListener.onCompilationReason(config.name, CompilationReason.Incremental(sources.length, Nil, Nil))
    }

    // For first compile (no previous analysis), report all source files upfront
    if (!hasPrevAnalysis) {
      sources.foreach { vf =>
        val path = vf match {
          case pvf: PlainVirtualFile => pvf.path
          case other                 => Path.of(other.id())
        }
        diagnosticListener.onCompileFile(path, Some("zinc-initial"))
      }
    }

    val inputs = createInputs(
      config,
      sources,
      language,
      compilers,
      previousResult,
      reporter,
      dependencyAnalyses,
      progressListener,
      cancellationToken,
      diagnosticListener,
      !hasPrevAnalysis
    )

    val compiler = incrementalCompiler

    // Ensure analysis directory exists (may have been deleted by stale-class-files guard above)
    Files.createDirectories(analysisFile.getParent)

    // Signal analyzing phase — zinc will compute invalidation before the first advance() callback
    diagnosticListener.onCompilePhase(config.name, CompilePhase.Analyzing)

    try {
      debug(s"[ZincBridge] Starting compilation for ${config.name}")
      val result = compiler.compile(inputs, logger)
      debug(s"[ZincBridge] Compilation done for ${config.name}, hasModified=${result.hasModified}")

      // Retroactively mark up-to-date if incremental analysis found nothing to recompile
      if (hasPrevAnalysis && !result.hasModified) {
        diagnosticListener.onCompilationReason(config.name, CompilationReason.UpToDate)
      }

      if (result.hasModified) {
        diagnosticListener.onCompilePhase(config.name, CompilePhase.SavingAnalysis)
        debug(s"[ZincBridge] Saving analysis for ${config.name}")
        saveAnalysis(analysisFile, result.analysis, result.setup)
        checkCaseInsensitiveCollisions(result.analysis, config.name, diagnosticListener)
      }

      val classFiles = collectClassFiles(config.outputDir)
      val success = ProjectCompileSuccess(config.outputDir, classFiles, Some(analysisFile))
      saveNoopManifest(analysisFile, config.sources, sources, dependencyAnalyses, language, ecjVersion, success)
      success
    } catch {
      case e: xsbti.CompileFailed =>
        ProjectCompileFailure(e.problems.toList.map(problemToError))
      case e: IllegalArgumentException if e.getMessage == "requirement failed" =>
        // Corrupt analysis — wipe and signal clean rebuild needed
        System.err.println(s"[ZincBridge] ${config.name}: corrupt analysis detected (${e.getMessage}), deleting for clean rebuild")
        Files.deleteIfExists(analysisFile)
        analysisCache.remove(analysisFile)
        noopManifestCache.remove(analysisFile)
        if (Files.exists(config.outputDir)) {
          bleep.internal.FileUtils.deleteDirectory(config.outputDir)
          Files.createDirectories(config.outputDir)
        }
        ProjectCompileFailure(
          List(
            CompilerError(
              path = None,
              line = 0,
              column = 0,
              message = "Corrupt incremental state detected, cleaned. Please recompile.",
              rendered = None,
              severity = CompilerError.Severity.Error
            )
          )
        )
    }
  }

  private def collectSources(sourceDirs: Set[Path]): Array[VirtualFile] = {
    val dirs = removeNestedDirs(sourceDirs)
    dirs.toArray.flatMap { srcDir =>
      if (Files.isDirectory(srcDir)) {
        scala.util
          .Using(Files.walk(srcDir)) { stream =>
            stream
              .iterator()
              .asScala
              .filter(p => p.toString.endsWith(".scala") || p.toString.endsWith(".java"))
              .filter(Files.isRegularFile(_))
              .map(p => PlainVirtualFile(p))
              .toArray
          }
          .getOrElse(Array.empty[VirtualFile])
      } else if (Files.exists(srcDir) && (srcDir.toString.endsWith(".scala") || srcDir.toString.endsWith(".java"))) {
        Array(PlainVirtualFile(srcDir))
      } else {
        Array.empty[VirtualFile]
      }
    }
  }

  private def collectClassFiles(outputDir: Path): Set[Path] =
    if (Files.exists(outputDir)) {
      // Use Using to ensure Files.walk stream is properly closed
      scala.util
        .Using(Files.walk(outputDir)) { stream =>
          stream
            .iterator()
            .asScala
            .filter(p => p.toString.endsWith(".class"))
            .filter(Files.isRegularFile(_))
            .toSet
        }
        .getOrElse(Set.empty)
    } else {
      Set.empty
    }

  /** Check for class names that differ only in case, which cause silent file collisions on case-insensitive filesystems (macOS HFS+/APFS, Windows NTFS).
    *
    * Uses Zinc's in-memory analysis (apis.internal) which records class names from the compiler, not from the filesystem. This means it correctly detects both
    * names even when the filesystem has already silently clobbered one file with the other.
    *
    * Cost: O(n) string operations on class names already in memory. Zero I/O.
    */
  private def checkCaseInsensitiveCollisions(
      analysis: CompileAnalysis,
      projectName: String,
      diagnosticListener: DiagnosticListener
  ): Unit = {
    val internalAnalysis = analysis.asInstanceOf[sbt.internal.inc.Analysis]
    val classNames = internalAnalysis.apis.internal.keySet.toList
    val collisions = classNames.groupBy(_.toLowerCase).filter(_._2.size > 1)
    collisions.foreach { case (_, names) =>
      diagnosticListener.onDiagnostic(
        CompilerError(
          path = None,
          line = 0,
          column = 0,
          message = s"Case-insensitive class name collision in $projectName: ${names.sorted.mkString(" vs ")}. " +
            "These produce class files that overwrite each other on case-insensitive filesystems (macOS, Windows), causing NoClassDefFoundError.",
          rendered = None,
          severity = CompilerError.Severity.Error
        )
      )
    }
  }

  private def getScalaInstance(scalaVersion: String): ZincScalaInstance =
    scalaInstanceCache.computeIfAbsent(
      scalaVersion,
      sv => {
        val instance = CompilerResolver.getScalaCompiler(sv)
        val resolvedAllJars = instance.allJars.map(_.toFile).toArray

        val resolvedLibraryJars = resolvedAllJars.filter { f =>
          f.getName.contains("scala-library") || f.getName.contains("scala3-library")
        }
        val resolvedCompilerJars = resolvedAllJars.filter { f =>
          val name = f.getName
          name.contains("scala-compiler") || name.contains("scala3-compiler") ||
          name.contains("tasty-core") || name.contains("scala3-interfaces") ||
          name.contains("scala-asm") || name.contains("scala-reflect")
        }
        val resolvedOtherJars = resolvedAllJars.filterNot(j => resolvedLibraryJars.contains(j) || resolvedCompilerJars.contains(j))
        val resolvedLoader = instance.loader

        new ZincScalaInstance {
          override def version(): String = sv
          override def libraryJars(): Array[File] = resolvedLibraryJars
          override def compilerJars(): Array[File] = resolvedCompilerJars
          override def allJars(): Array[File] = resolvedAllJars
          override def otherJars(): Array[File] = resolvedOtherJars
          override def loaderLibraryOnly(): ClassLoader = resolvedLoader
          override def loaderCompilerOnly(): ClassLoader = resolvedLoader
          override def loader(): ClassLoader = resolvedLoader
          override def actualVersion(): String = sv
        }
      }
    )

  private def createCompilers(
      scalaInstance: ZincScalaInstance,
      language: ProjectLanguage.ScalaJava,
      ecjVersion: Option[String],
      cancellationToken: CancellationToken,
      progressListener: ProgressListener
  ): Compilers = {
    val bridgeJar = getBridge(language.scalaVersion)
    val classpathOptions = ClasspathOptionsUtil.noboot(scalaInstance.version)
    val cache = new ClassLoaderCache(new java.net.URLClassLoader(Array()))

    val scalaCompiler = new AnalyzingCompiler(
      scalaInstance,
      ZincCompilerUtil.constantBridgeProvider(scalaInstance, bridgeJar.toFile),
      classpathOptions,
      _ => (),
      Some(cache)
    )

    val javaTools = ecjVersion match {
      case Some(version) =>
        debug(s"[ZincBridge] Using ECJ version $version for Java compilation")
        createEcjTools(scalaInstance, classpathOptions, version, cancellationToken, progressListener)
      case None =>
        sbt.internal.inc.javac.JavaTools.directOrFork(
          scalaInstance,
          classpathOptions,
          None // Use system Java
        )
    }

    ZincUtil.compilers(javaTools, scalaCompiler)
  }

  /** Create JavaTools that use ECJ (Eclipse Compiler for Java) instead of javac */
  private def createEcjTools(
      scalaInstance: ZincScalaInstance,
      classpathOptions: ClasspathOptions,
      ecjVersion: String,
      cancellationToken: CancellationToken,
      progressListener: ProgressListener
  ): xsbti.compile.JavaTools = {
    // Resolve ECJ jar
    val ecjJars = resolveEcj(ecjVersion)
    val ecjClassLoader = new java.net.URLClassLoader(
      ecjJars.map(_.toUri.toURL).toArray,
      getClass.getClassLoader
    )

    // Create a forked Java compiler that uses ECJ
    val ecjCompiler = new EcjCompiler(ecjJars, ecjClassLoader, cancellationToken, progressListener)

    // Use standard javadoc (ECJ doesn't include javadoc)
    val standardJavaTools = sbt.internal.inc.javac.JavaTools.directOrFork(
      scalaInstance,
      classpathOptions,
      None
    )

    new xsbti.compile.JavaTools {
      def javac(): xsbti.compile.JavaCompiler = ecjCompiler
      def javadoc(): xsbti.compile.Javadoc = standardJavaTools.javadoc()
    }
  }

  /** Resolve ECJ jars from Maven */
  private def resolveEcj(version: String): Seq[Path] = {
    val dep = bleep.model.Dep.Java("org.eclipse.jdt", "ecj", version)
    val combo = bleep.model.VersionCombo.Java
    dep.asJava(combo) match {
      case Right(javaDep) =>
        coursier
          .Fetch()
          .addDependencies(javaDep.dependency)
          .run()
          .map(_.toPath)
      case Left(e) =>
        throw new RuntimeException(s"Failed to resolve ECJ $version: $e")
    }
  }

  private def getBridge(scalaVersion: String): Path =
    bridgeCache.computeIfAbsent(scalaVersion, resolveBridge)

  private def resolveBridge(scalaVersion: String): Path = {
    val bridgeDep = if (scalaVersion.startsWith("3.")) {
      // scala3-sbt-bridge is a Java artifact, not a Scala artifact (no _3 suffix)
      s"org.scala-lang:scala3-sbt-bridge:$scalaVersion"
    } else if (scalaVersion.startsWith("2.13")) {
      s"org.scala-sbt::compiler-bridge:1.10.4"
    } else if (scalaVersion.startsWith("2.12")) {
      s"org.scala-sbt::compiler-bridge:1.10.4"
    } else {
      throw new IllegalArgumentException(s"Unsupported Scala version: $scalaVersion")
    }

    val dep = bleep.model.Dep.parse(bridgeDep) match {
      case Right(d) => d
      case Left(e)  => throw new RuntimeException(s"Failed to parse bridge dependency: $e")
    }

    val combo = bleep.model.VersionCombo.Jvm(bleep.model.VersionScala(scalaVersion))
    val jars = dep.asJava(combo) match {
      case Right(javaDep) =>
        coursier
          .Fetch()
          .addDependencies(javaDep.dependency)
          .run()
          .map(_.toPath)
      case Left(e) =>
        throw new RuntimeException(s"Failed to convert bridge dependency: $e")
    }

    jars
      .find(_.getFileName.toString.contains("bridge"))
      .getOrElse(
        jars.headOption.getOrElse(
          throw new RuntimeException(s"Could not resolve bridge for Scala $scalaVersion")
        )
      )
  }

  private def createInputs(
      config: ProjectConfig,
      sources: Array[VirtualFile],
      language: ProjectLanguage.ScalaJava,
      compilers: Compilers,
      previousResult: PreviousResult,
      reporter: Reporter,
      dependencyAnalyses: Map[Path, Path],
      progressListener: ProgressListener,
      cancellationToken: CancellationToken,
      diagnosticListener: DiagnosticListener,
      isCleanBuild: Boolean
  ): Inputs = {
    val outputDir = config.outputDir
    // Include output directory in classpath for incremental Java compilation.
    // When javac recompiles a subset of files, it needs to find already-compiled
    // classes from the same project (e.g., TestProtocol.class when compiling ForkedTestRunner.java)
    val classpathVf = (outputDir +: config.classpath).map(p => PlainVirtualFile(p): VirtualFile).toArray

    val scalacOptions = language.scalaOptions.toArray
    val releaseOptions = language.javaRelease.map(r => Array(s"--release", r.toString)).getOrElse(Array.empty[String])
    val javacOptions = releaseOptions ++ language.javaOptions.toArray

    // Load analyses from dependency projects for proper incremental compilation.
    // Uses a global SoftReference cache to avoid re-reading from disk when the same
    // dependency is referenced by multiple projects in the same build.
    val loadedAnalyses: Map[Path, CompileAnalysis] = dependencyAnalyses.flatMap { case (classDir, analysisFile) =>
      if (Files.exists(analysisFile)) {
        val cached = {
          val ref = analysisCache.get(analysisFile)
          if (ref != null) ref.get() else null
        }
        if (cached != null) {
          Some(classDir -> cached)
        } else {
          try {
            val store = sbt.internal.inc.FileAnalysisStore.binary(analysisFile.toFile, analysisMappers)
            store.get().toScala.map { contents =>
              val analysis = contents.getAnalysis
              analysisCache.put(analysisFile, new SoftReference(analysis))
              classDir -> analysis
            }
          } catch {
            case _: Exception => None
          }
        }
      } else None
    }

    val lookup = new PerClasspathEntryLookup {
      def analysis(classpathEntry: VirtualFile): Optional[CompileAnalysis] = {
        // Match classpath entry to loaded analysis
        val entryPath = classpathEntry match {
          case pvf: PlainVirtualFile => pvf.path
          case other                 => Path.of(other.id())
        }
        loadedAnalyses.get(entryPath).toJava
      }
      def definesClass(classpathEntry: VirtualFile): DefinesClass = {
        // Use analysis to check if class is defined
        val entryPath = classpathEntry match {
          case pvf: PlainVirtualFile => pvf.path
          case other                 => Path.of(other.id())
        }
        loadedAnalyses.get(entryPath) match {
          case Some(analysis) =>
            (className: String) => {
              val relations = analysis.asInstanceOf[sbt.internal.inc.Analysis].relations
              relations.productClassName._2s.contains(className) ||
              relations.libraryClassName._2s.contains(className)
            }
          case None =>
            (_: String) => false
        }
      }
    }

    // Create a temp cache path (Zinc requires non-null)
    val cachePath = config.analysisDir.getOrElse(config.outputDir.resolve(".zinc")).resolve("cache")

    // Create progress callback that wraps our listener
    // Detect the transition from zinc's invalidation analysis to actual compilation
    // via the first advance() callback from the Scala compiler
    val firstAdvance = new java.util.concurrent.atomic.AtomicBoolean(true)
    val progress: CompileProgress = new CompileProgress {
      override def startUnit(phase: String, unitPath: String): Unit =
        debug(s"[ZincBridge] startUnit called: phase=$phase, unitPath=$unitPath")

      override def advance(current: Int, total: Int, prevPhase: String, nextPhase: String): Boolean = {
        if (firstAdvance.compareAndSet(true, false)) {
          diagnosticListener.onCompilePhase(config.name, CompilePhase.Compiling)
        }
        val shouldContinue = progressListener.onProgress(current, total, nextPhase)
        shouldContinue && !cancellationToken.isCancelled
      }
    }

    // Detect Zinc incremental cycle bugs:
    // 1. After a clean build, Zinc should never want another cycle — the analysis is fresh.
    // 2. If Zinc re-invalidates the exact same classes after recompiling them, it will never converge.
    // In both cases: stop the cycle and warn.
    var previousInvalidations: Option[Set[String]] = None
    // Extend ExternalLookup directly (not NoopExternalLookup) because NoopExternalLookup
    // narrows lookupAnalyzedClass return type to None.type which we can't widen back.
    val cycleGuard: ExternalLookup = new ExternalLookup {
      // Return Some(emptyAnalyzedClass) so LookupImpl falls through to super.lookupAnalyzedClass
      // (the normal dependency analysis path). Returning None would short-circuit the lookup,
      // making Zinc think ALL external dependencies are missing → massive over-invalidation.
      override def lookupAnalyzedClass(binaryClassName: String, file: Option[VirtualFileRef]): Option[xsbti.api.AnalyzedClass] =
        Some(APIs.emptyAnalyzedClass)

      override def changedSources(previousAnalysis: CompileAnalysis): Option[Changes[VirtualFileRef]] = None
      override def changedBinaries(previousAnalysis: CompileAnalysis): Option[Set[VirtualFileRef]] = None
      override def removedProducts(previousAnalysis: CompileAnalysis): Option[Set[VirtualFileRef]] = None
      override def hashClasspath(classpath: Array[VirtualFile]): Optional[Array[FileHash]] = Optional.empty()

      override def shouldDoIncrementalCompilation(
          changedClasses: Set[String],
          analysis: CompileAnalysis
      ): Boolean = {
        val isBug = if (isCleanBuild && previousInvalidations.isEmpty) {
          // Clean build just finished — any invalidation is a Zinc bug
          true
        } else {
          previousInvalidations match {
            case Some(prev) if prev == changedClasses => true
            case _                                    => false
          }
        }

        if (isBug) {
          val classes = changedClasses.toList.sorted.take(20).mkString(", ")
          val more = if (changedClasses.size > 20) s" (and ${changedClasses.size - 20} more)" else ""
          val reason =
            if (isCleanBuild && previousInvalidations.isEmpty)
              "after a clean build"
            else
              "after recompiling, re-invalidated the exact same class(es)"
          System.err.println(
            s"[ZincBridge] WARNING: Zinc incremental compilation bug in ${config.name}: " +
              s"$reason. ${changedClasses.size} class(es): $classes$more"
          )
          diagnosticListener.onDiagnostic(
            CompilerError(
              path = None,
              line = 0,
              column = 0,
              message = s"Zinc incremental compilation bug in ${config.name}: " +
                s"$reason. ${changedClasses.size} class(es): $classes$more",
              rendered = None,
              severity = CompilerError.Severity.Warning
            )
          )
          false
        } else {
          previousInvalidations = Some(changedClasses)
          true
        }
      }
    }
    val externalHooks = new DefaultExternalHooks(
      Optional.of[ExternalHooks.Lookup](cycleGuard),
      Optional.empty[ClassFileManager]()
    )
    val incOptions = IncOptions.of().withExternalHooks(externalHooks)

    val setup = Setup.of(
      lookup,
      false, // skip
      cachePath,
      CompilerCache.fresh,
      incOptions,
      reporter,
      Optional.of(progress),
      Array.empty[xsbti.T2[String, String]]
    )

    // sourcePositionMapper: identity function
    val sourcePositionMapper: java.util.function.Function[Position, Position] = (p: Position) => p

    val options = CompileOptions.of(
      classpathVf,
      sources,
      outputDir,
      scalacOptions,
      javacOptions,
      100, // maxErrors
      sourcePositionMapper,
      CompileOrder.JavaThenScala
    )

    debug(s"[ZincBridge] Current setup: outputDir=$outputDir, order=JavaThenScala")
    debug(s"[ZincBridge] Current scalacOptions: ${scalacOptions.mkString(", ")}")
    debug(s"[ZincBridge] Source count: ${sources.length}, classpath count: ${classpathVf.length}")
    // Log sample source info for debugging
    if (sources.nonEmpty) {
      val firstSrc = sources.head
      debug(s"[ZincBridge] Current source sample: type=${firstSrc.getClass.getName}, id=${firstSrc.id}, hashCode=${firstSrc.hashCode()}")
    }

    // Check previous MiniSetup if available
    if (previousResult.setup().isPresent) {
      val prevSetup = previousResult.setup().get()
      val prevOutput = prevSetup.output.getSingleOutputAsPath.map(_.toString).orElse("unknown")
      val prevOrder = prevSetup.order.toString
      val prevScalacOpts = prevSetup.options.scalacOptions.mkString(", ")

      debug(s"[ZincBridge] COMPARING SETUPS:")
      debug(s"[ZincBridge]   Previous output: $prevOutput, Current output: $outputDir")
      debug(s"[ZincBridge]   Previous order: $prevOrder, Current order: Mixed")
      debug(s"[ZincBridge]   Scalac options match: ${prevScalacOpts == scalacOptions.mkString(", ")}")
      debug(s"[ZincBridge]   Classpath count: prev=${prevSetup.options.classpathHash.length}, curr=${classpathVf.length}")

      // Check if output dirs match
      val outputMatch = prevOutput == outputDir.toString
      debug(s"[ZincBridge]   Output dirs match: $outputMatch")
    }

    Inputs.of(compilers, options, setup, previousResult)
  }

  private def loadPreviousResult(analysisFile: Path): PreviousResult = {
    debug(s"[ZincBridge] Looking for analysis at: $analysisFile, exists=${Files.exists(analysisFile)}")
    if (Files.exists(analysisFile)) {
      try {
        val store = sbt.internal.inc.FileAnalysisStore.binary(analysisFile.toFile, analysisMappers)
        val contents = store.get()
        if (contents.isPresent) {
          val analysis = contents.get().getAnalysis.asInstanceOf[sbt.internal.inc.Analysis]
          val miniSetup = contents.get().getMiniSetup
          debug(s"[ZincBridge] Loaded analysis with ${analysis.apis.internal.size} internal APIs")
          debug(s"[ZincBridge] Previous MiniSetup: output=${miniSetup.output}, order=${miniSetup.order}, storeApis=${miniSetup.storeApis}")
          debug(s"[ZincBridge] Previous scalacOptions: ${miniSetup.options.scalacOptions.mkString(", ")}")
          // Log source stamps to understand invalidation
          val stamps = analysis.stamps
          val sourceStamps = stamps.sources
          debug(s"[ZincBridge] Source stamps count: ${sourceStamps.size}")
          if (sourceStamps.nonEmpty) {
            val firstFew = sourceStamps
              .take(3)
              .map { case (vf, stamp) =>
                s"${vf.id}:$stamp"
              }
              .mkString(", ")
            debug(s"[ZincBridge] Sample source stamps: $firstFew")
            // Log VirtualFileRef types and verify hashCode compatibility
            val firstVf = sourceStamps.head._1
            debug(s"[ZincBridge] Stored VirtualFileRef type: ${firstVf.getClass.getName}, id=${firstVf.id}, hashCode=${firstVf.hashCode()}")
            // Verify that PlainVirtualFile would compute the same hashCode
            val plainVfHashCode = java.util.Objects.hash("xsbti.BasicVirtualFileRef", firstVf.id)
            debug(s"[ZincBridge] PlainVirtualFile hashCode would be: $plainVfHashCode (matches stored: ${plainVfHashCode == firstVf.hashCode()})")
          }
          // Return the analysis directly - PlainVirtualFile.hashCode() now matches BasicVirtualFileRef.hashCode()
          // so HashMap lookups work correctly across the serialization boundary
          PreviousResult.of(
            Optional.of(analysis),
            Optional.of(miniSetup)
          )
        } else {
          debug(s"[ZincBridge] Analysis file exists but store.get() returned empty")
          PreviousResult.of(Optional.empty(), Optional.empty())
        }
      } catch {
        case e: Exception =>
          debug(s"[ZincBridge] Failed to load analysis: ${e.getMessage}")
          PreviousResult.of(Optional.empty(), Optional.empty())
      }
    } else {
      debug(s"[ZincBridge] No analysis file found")
      PreviousResult.of(Optional.empty(), Optional.empty())
    }
  }

  private def saveAnalysis(analysisFile: Path, analysis: CompileAnalysis, setup: MiniSetup): Unit = {
    // Write to a temp file first, then atomic rename.
    // This prevents corrupted analysis.zip when compilation is cancelled mid-write.
    val tempFile = analysisFile.resolveSibling(analysisFile.getFileName.toString + ".tmp")
    try {
      val cpHashes = setup.options.classpathHash
      if (cpHashes.nonEmpty) {
        val sample = cpHashes.take(3).map(fh => s"${fh.file}:${fh.hash}").mkString(", ")
        debug(s"[ZincBridge] Saving with classpath hashes: $sample")
      }
      val store = sbt.internal.inc.FileAnalysisStore.binary(tempFile.toFile, analysisMappers)
      store.set(AnalysisContents.create(analysis, setup))
      Files.move(tempFile, analysisFile, StandardCopyOption.REPLACE_EXISTING, StandardCopyOption.ATOMIC_MOVE)
      // Update soft reference cache so dependents see the fresh analysis
      analysisCache.put(analysisFile, new SoftReference(analysis))
    } catch {
      case e: Exception =>
        try Files.deleteIfExists(tempFile)
        catch { case _: Exception => () }
        throw new IOException(s"Failed to save analysis to $analysisFile: ${e.getMessage}", e)
    }
  }

  private def problemToError(problem: Problem): CompilerError = {
    val pos = problem.position()
    val severity = problem.severity() match {
      case xsbti.Severity.Error => CompilerError.Severity.Error
      case xsbti.Severity.Warn  => CompilerError.Severity.Warning
      case xsbti.Severity.Info  => CompilerError.Severity.Info
    }
    CompilerError(
      path = pos.sourcePath().toScala.map(Path.of(_)),
      line = pos.line().toScala.map(_.intValue).getOrElse(0),
      column = pos.pointer().toScala.map(_.intValue).getOrElse(0),
      message = problem.message(),
      rendered = problem.rendered().toScala,
      severity = severity
    )
  }

  /** Logger that forwards to DiagnosticListener */
  private class BleepLogger(listener: DiagnosticListener) extends Logger {
    def debug(msg: java.util.function.Supplier[String]): Unit = ()
    def error(msg: java.util.function.Supplier[String]): Unit =
      listener.onDiagnostic(CompilerError(None, 0, 0, msg.get(), None, CompilerError.Severity.Error))
    def info(msg: java.util.function.Supplier[String]): Unit = ()
    def trace(err: java.util.function.Supplier[Throwable]): Unit = ()
    def warn(msg: java.util.function.Supplier[String]): Unit =
      listener.onDiagnostic(CompilerError(None, 0, 0, msg.get(), None, CompilerError.Severity.Warning))
  }

  /** Reporter that forwards problems to DiagnosticListener */
  private class BleepReporter(listener: DiagnosticListener) extends Reporter {
    private var problemList = List.empty[Problem]

    def reset(): Unit = problemList = Nil
    def hasErrors: Boolean = problemList.exists(_.severity == xsbti.Severity.Error)
    def hasWarnings: Boolean = problemList.exists(_.severity == xsbti.Severity.Warn)
    def printSummary(): Unit = ()
    def problems(): Array[Problem] = problemList.toArray

    def log(problem: Problem): Unit = {
      problemList = problemList :+ problem
      val error = problemToError(problem)
      listener.onDiagnostic(error)
    }

    def comment(pos: Position, msg: String): Unit = ()
  }
}

/** ECJ (Eclipse Compiler for Java) wrapper for Zinc.
  *
  * Uses ECJ's batch Main with a CompilationProgress bridge (generated via ASM at runtime) to get per-file progress and native cancellation support.
  */
private class EcjCompiler(
    ecjJars: Seq[Path],
    ecjClassLoader: ClassLoader,
    cancellationToken: CancellationToken,
    progressListener: ProgressListener
) extends xsbti.compile.JavaCompiler {
  import scala.jdk.OptionConverters.*
  import scala.collection.mutable

  def run(
      sources: Array[VirtualFile],
      options: Array[String],
      output: xsbti.compile.Output,
      incToolOptions: xsbti.compile.IncToolOptions,
      reporter: Reporter,
      log: Logger
  ): Boolean = {
    if (sources.isEmpty) return true

    val outputDir = output.getSingleOutputAsPath.toScala.getOrElse(
      throw new RuntimeException("ECJ requires a single output directory")
    )
    Files.createDirectories(outputDir)

    // Build arguments for ECJ batch compiler
    val args = mutable.ArrayBuffer[String]()
    args += "-d"
    args += outputDir.toString

    // Filter and add options (skip -J flags which are for JVM)
    options.foreach { opt =>
      if (!opt.startsWith("-J")) {
        args += opt
      }
    }

    // Add source files
    sources.foreach { vf =>
      val path = vf match {
        case pvf: PlainVirtualFile => pvf.path.toString
        case other                 => other.id()
      }
      args += path
    }

    ZincBridge.debug(s"[ECJ] Compiling ${sources.length} sources with args: ${args.mkString(" ")}")

    // Use ECJ's batch Main class with custom streams to capture output
    val mainClass = ecjClassLoader.loadClass("org.eclipse.jdt.internal.compiler.batch.Main")

    val errStream = new java.io.ByteArrayOutputStream()
    val outStream = new java.io.ByteArrayOutputStream()
    val errPrint = new java.io.PrintWriter(errStream, true)
    val outPrint = new java.io.PrintWriter(outStream, true)

    val compileMethod = mainClass.getMethod("compile", classOf[Array[String]])

    // Create CompilationProgress bridge via ASM for per-file progress and native cancellation.
    // The bridge class is generated in ECJ's classloader and communicates back via AtomicInteger/AtomicBoolean.
    val workedSoFar = new java.util.concurrent.atomic.AtomicInteger(0)
    val totalWorkUnits = new java.util.concurrent.atomic.AtomicInteger(0)
    val cancelFlag = new java.util.concurrent.atomic.AtomicBoolean(false)
    cancellationToken.onCancel(() => cancelFlag.set(true))

    def runEcj(ecjMain: AnyRef, hasProgress: Boolean): java.lang.Boolean = {
      @volatile var success: java.lang.Boolean = java.lang.Boolean.FALSE
      @volatile var compileException: Throwable = null

      val compileThread = new Thread("ecj-compiler") {
        override def run(): Unit =
          try
            success = compileMethod.invoke(ecjMain, args.toArray).asInstanceOf[java.lang.Boolean]
          catch {
            case _: InterruptedException                                                                         => ()
            case e: java.lang.reflect.InvocationTargetException if e.getCause.isInstanceOf[InterruptedException] => ()
            case e: Throwable                                                                                    => compileException = e
          }
      }

      // IMPORTANT: Do NOT call compileThread.interrupt(). Java NIO spec says
      // interrupting a thread blocked on a channel closes that channel. ECJ reads
      // jrt-fs.jar via ZipFileSystem which the JDK caches globally. Interrupting
      // closes the channel and poisons the cache for all subsequent ECJ compiles.
      // Instead, rely on CompilationProgress.isCancelled() via the cancelFlag.
      compileThread.start()

      // Poll with timeout so we can report progress and detect cancellation.
      while (compileThread.isAlive && !cancellationToken.isCancelled) {
        compileThread.join(500)
        if (hasProgress && !cancellationToken.isCancelled) {
          val worked = workedSoFar.get()
          val total = totalWorkUnits.get()
          if (total > 0) {
            progressListener.onProgress(worked, total, "ecj")
          }
        }
      }

      // Wait for ECJ to notice cancellation via CompilationProgress.isCancelled().
      // If the bridge failed (hasProgress=false), ECJ won't check cancellation
      // and we have to wait for it to finish naturally — but with a timeout to
      // prevent hanging the BSP server if ECJ is stuck (e.g., deadlocked or
      // blocked on I/O from a network-mounted JAR).
      if (cancellationToken.isCancelled && compileThread.isAlive) {
        compileThread.join(30000) // 30 second timeout
        if (compileThread.isAlive) {
          System.err.println(s"[ZincBridge] WARNING: ECJ thread did not exit within 30s after cancellation")
        }
      }

      if (cancellationToken.isCancelled) {
        throw new RuntimeException("ECJ compilation cancelled")
      }

      if (compileException != null) {
        throw compileException
      }

      success
    }

    val (ecjMain, hasProgress) = EcjCompiler.createMainWithProgress(
      mainClass,
      ecjClassLoader,
      outPrint,
      errPrint,
      workedSoFar,
      totalWorkUnits,
      cancelFlag
    )

    /** Check if an exception is caused by the ASM-generated bridge class */
    def isBridgeError(e: Throwable): Boolean = {
      val msg = Option(e.getMessage).getOrElse("") + Option(e.getCause).flatMap(c => Option(c.getMessage)).getOrElse("")
      msg.contains("EcjCompilationProgressBridge") || msg.contains("CompilationProgress")
    }

    val success: java.lang.Boolean =
      if (!hasProgress) runEcj(ecjMain, false)
      else {
        try runEcj(ecjMain, true)
        catch {
          case e: Throwable if !cancellationToken.isCancelled && isBridgeError(e) =>
            // CompilationProgress bridge failed at runtime — retry without it
            ZincBridge.debug(s"[ECJ] CompilationProgress bridge failed (${e.getClass.getSimpleName}), retrying without progress")
            val constructor3 = mainClass.getConstructor(
              classOf[java.io.PrintWriter],
              classOf[java.io.PrintWriter],
              classOf[Boolean]
            )
            // Reset output streams for retry
            errStream.reset()
            outStream.reset()
            val fallbackMain = constructor3.newInstance(outPrint, errPrint, java.lang.Boolean.FALSE)
            runEcj(fallbackMain, false)
        }
      }

    errPrint.flush()
    outPrint.flush()

    val errOutput = errStream.toString
    val outOutput = outStream.toString

    ZincBridge.debug(s"[ECJ] Compile finished: success=$success")
    if (errOutput.nonEmpty) ZincBridge.debug(s"[ECJ] stderr:\n$errOutput")
    if (outOutput.nonEmpty) ZincBridge.debug(s"[ECJ] stdout:\n$outOutput")

    // Parse ECJ's structured text output
    // ECJ format:
    // ----------
    // 1. ERROR in /path/to/File.java (at line 10)
    //     source line here
    //     ^^^^^
    // Error message here
    // ----------
    val allOutput = outOutput + "\n" + errOutput
    parseEcjOutput(allOutput, reporter)

    // If compilation failed but we found no errors, report the raw output
    if (!success && !reporter.hasErrors) {
      val msg = if (allOutput.trim.nonEmpty) allOutput.trim else "ECJ compilation failed with no error message"
      reporter.log(createProblem(None, 0, 0, 0, msg, isErr = true))
    }

    success.booleanValue()
  }

  /** Parse ECJ's text output format to extract structured errors */
  private def parseEcjOutput(output: String, reporter: Reporter): Unit = {
    // ECJ separates problems with "----------"
    val blocks = output.split("----------").map(_.trim).filter(_.nonEmpty)

    for (block <- blocks) {
      val lines = block.linesIterator.toList
      if (lines.nonEmpty) {
        // First line format: "N. ERROR in /path/file.java (at line M)"
        // or: "N. WARNING in /path/file.java (at line M)"
        val headerPattern = """(\d+)\.\s+(ERROR|WARNING)\s+in\s+(.+?)\s+\(at line (\d+)\)""".r

        lines.head match {
          case headerPattern(_, severityStr, filePath, lineStr) =>
            val lineNum = lineStr.toInt
            val isErr = severityStr == "ERROR"

            // Find the message - it's after the source line and caret pointer
            // Lines: [header, source line, caret line (^^^), message lines...]
            val messageStartIdx = lines.indexWhere(_.trim.startsWith("^")) + 1
            val messageLines = if (messageStartIdx > 0 && messageStartIdx < lines.length) {
              lines.drop(messageStartIdx).takeWhile(_.nonEmpty)
            } else {
              // No caret found, try to get message from remaining lines
              lines.drop(1).filter(l => l.nonEmpty && !l.forall(c => c == '^' || c == ' '))
            }
            val message = messageLines.mkString(" ").trim

            if (message.nonEmpty) {
              reporter.log(createProblem(Some(filePath), lineNum, 0, 0, message, isErr))
            }

          case _ =>
          // Non-standard block (e.g. summary line "N problems (X errors, Y warnings)").
          // Do NOT guess severity here — the success flag + hasErrors fallback at the
          // call site already handles the case where ECJ fails without any parsed errors.
        }
      }
    }
  }

  private def createProblem(
      filePath: Option[String],
      lineNum: Int,
      startOff: Int,
      endOff: Int,
      msg: String,
      isErr: Boolean
  ): xsbti.Problem = {
    val sev = if (isErr) xsbti.Severity.Error else xsbti.Severity.Warn

    new xsbti.Problem {
      def category(): String = "ECJ"
      def severity(): xsbti.Severity = sev
      def message(): String = msg
      def position(): xsbti.Position = new xsbti.Position {
        def line(): Optional[Integer] = if (lineNum > 0) Optional.of(Integer.valueOf(lineNum)) else Optional.empty()
        def lineContent(): String = ""
        def offset(): Optional[Integer] = if (startOff > 0) Optional.of(Integer.valueOf(startOff)) else Optional.empty()
        def pointer(): Optional[Integer] = Optional.empty()
        def pointerSpace(): Optional[String] = Optional.empty()
        def sourcePath(): Optional[String] = filePath.map(f => Optional.of[String](f)).getOrElse(Optional.empty())
        def sourceFile(): Optional[java.io.File] = filePath.map(f => Optional.of[java.io.File](new java.io.File(f))).getOrElse(Optional.empty())
        override def startOffset(): Optional[Integer] = if (startOff > 0) Optional.of(Integer.valueOf(startOff)) else Optional.empty()
        override def endOffset(): Optional[Integer] = if (endOff > 0) Optional.of(Integer.valueOf(endOff)) else Optional.empty()
        override def startLine(): Optional[Integer] = if (lineNum > 0) Optional.of(Integer.valueOf(lineNum)) else Optional.empty()
        override def startColumn(): Optional[Integer] = Optional.empty()
        override def endLine(): Optional[Integer] = Optional.empty()
        override def endColumn(): Optional[Integer] = Optional.empty()
      }
      override def rendered(): Optional[String] = {
        val loc = filePath.map(f => s"$f:$lineNum: ").getOrElse("")
        Optional.of(s"$loc$msg")
      }
      override def diagnosticCode(): Optional[xsbti.DiagnosticCode] = Optional.empty()
      override def diagnosticRelatedInformation(): java.util.List[xsbti.DiagnosticRelatedInformation] =
        java.util.Collections.emptyList()
      override def actions(): java.util.List[xsbti.Action] = java.util.Collections.emptyList()
    }
  }
}

private[analysis] object EcjCompiler {
  private val BridgeClassName = "bleep/analysis/EcjCompilationProgressBridge"
  private val ProgressSuperClass = "org/eclipse/jdt/core/compiler/CompilationProgress"
  private val AtomicIntDesc = "Ljava/util/concurrent/atomic/AtomicInteger;"
  private val AtomicBoolDesc = "Ljava/util/concurrent/atomic/AtomicBoolean;"

  /** Try to create a Main with CompilationProgress for per-file progress. Falls back to the 3-param constructor if CompilationProgress is not available.
    * Returns (mainInstance, hasProgress).
    */
  def createMainWithProgress(
      mainClass: Class[?],
      ecjClassLoader: ClassLoader,
      outPrint: java.io.PrintWriter,
      errPrint: java.io.PrintWriter,
      workedSoFar: java.util.concurrent.atomic.AtomicInteger,
      totalWorkUnits: java.util.concurrent.atomic.AtomicInteger,
      cancelFlag: java.util.concurrent.atomic.AtomicBoolean
  ): (AnyRef, Boolean) =
    try {
      val progressClass = ecjClassLoader.loadClass("org.eclipse.jdt.core.compiler.CompilationProgress")

      // Generate bridge class via ASM in a child classloader of ECJ's classloader
      val bridgeBytes = generateBridgeClass()
      val bridgeClassLoader = new ClassLoader(ecjClassLoader) {
        override def findClass(name: String): Class[?] =
          if (name == BridgeClassName.replace('/', '.'))
            defineClass(name, bridgeBytes, 0, bridgeBytes.length)
          else
            throw new ClassNotFoundException(name)
      }
      val bridgeClass = bridgeClassLoader.loadClass(BridgeClassName.replace('/', '.'))

      val bridgeInstance = bridgeClass
        .getConstructor(
          classOf[java.util.concurrent.atomic.AtomicInteger],
          classOf[java.util.concurrent.atomic.AtomicInteger],
          classOf[java.util.concurrent.atomic.AtomicBoolean]
        )
        .newInstance(workedSoFar, totalWorkUnits, cancelFlag)

      // Use 5-param Main constructor: (PrintWriter, PrintWriter, boolean, Map, CompilationProgress)
      val constructor5 = mainClass.getConstructor(
        classOf[java.io.PrintWriter],
        classOf[java.io.PrintWriter],
        classOf[Boolean],
        classOf[java.util.Map[?, ?]],
        progressClass
      )
      val ecjMain = constructor5.newInstance(
        outPrint,
        errPrint,
        java.lang.Boolean.FALSE,
        null, // customDefaultOptions — null uses defaults
        bridgeInstance
      )
      ZincBridge.debug("[ECJ] Using CompilationProgress for per-file progress")
      (ecjMain, true)
    } catch {
      case e: Exception =>
        ZincBridge.debug(s"[ECJ] CompilationProgress not available (${e.getClass.getSimpleName}: ${e.getMessage}), using basic Main constructor")
        val constructor3 = mainClass.getConstructor(
          classOf[java.io.PrintWriter],
          classOf[java.io.PrintWriter],
          classOf[Boolean]
        )
        (constructor3.newInstance(outPrint, errPrint, java.lang.Boolean.FALSE), false)
    }

  /** Generate bytecode for a CompilationProgress subclass that bridges to AtomicInteger/AtomicBoolean.
    *
    * The generated class has three fields (workedSoFar, totalWork, cancelFlag) passed via constructor, and implements the five abstract methods of
    * CompilationProgress:
    *   - begin(int) — sets totalWork
    *   - done() — no-op
    *   - isCancelled() — reads cancelFlag
    *   - setTaskName(String) — no-op
    *   - worked(int, int) — updates workedSoFar and totalWork
    */
  private[analysis] def generateBridgeClass(): Array[Byte] = {
    import org.objectweb.asm.{ClassWriter, MethodVisitor, Opcodes}

    val cw = new ClassWriter(ClassWriter.COMPUTE_FRAMES | ClassWriter.COMPUTE_MAXS)
    cw.visit(Opcodes.V17, Opcodes.ACC_PUBLIC, BridgeClassName, null, ProgressSuperClass, null)

    // Fields
    cw.visitField(Opcodes.ACC_PRIVATE | Opcodes.ACC_FINAL, "workedSoFar", AtomicIntDesc, null, null).visitEnd()
    cw.visitField(Opcodes.ACC_PRIVATE | Opcodes.ACC_FINAL, "totalWork", AtomicIntDesc, null, null).visitEnd()
    cw.visitField(Opcodes.ACC_PRIVATE | Opcodes.ACC_FINAL, "cancelFlag", AtomicBoolDesc, null, null).visitEnd()

    // Constructor(AtomicInteger, AtomicInteger, AtomicBoolean)
    locally {
      val ctorDesc = s"($AtomicIntDesc$AtomicIntDesc$AtomicBoolDesc)V"
      val mv = cw.visitMethod(Opcodes.ACC_PUBLIC, "<init>", ctorDesc, null, null)
      mv.visitCode()
      mv.visitVarInsn(Opcodes.ALOAD, 0)
      mv.visitMethodInsn(Opcodes.INVOKESPECIAL, ProgressSuperClass, "<init>", "()V", false)
      putField(mv, "workedSoFar", AtomicIntDesc, 1)
      putField(mv, "totalWork", AtomicIntDesc, 2)
      putField(mv, "cancelFlag", AtomicBoolDesc, 3)
      mv.visitInsn(Opcodes.RETURN)
      mv.visitMaxs(0, 0)
      mv.visitEnd()
    }

    // begin(int remainingWork) — totalWork.set(remainingWork)
    locally {
      val mv = cw.visitMethod(Opcodes.ACC_PUBLIC, "begin", "(I)V", null, null)
      mv.visitCode()
      getField(mv, "totalWork", AtomicIntDesc)
      mv.visitVarInsn(Opcodes.ILOAD, 1)
      mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "java/util/concurrent/atomic/AtomicInteger", "set", "(I)V", false)
      mv.visitInsn(Opcodes.RETURN)
      mv.visitMaxs(0, 0)
      mv.visitEnd()
    }

    // done() — no-op
    locally {
      val mv = cw.visitMethod(Opcodes.ACC_PUBLIC, "done", "()V", null, null)
      mv.visitCode()
      mv.visitInsn(Opcodes.RETURN)
      mv.visitMaxs(0, 0)
      mv.visitEnd()
    }

    // isCancelled() — return cancelFlag.get()
    locally {
      val mv = cw.visitMethod(Opcodes.ACC_PUBLIC, "isCancelled", "()Z", null, null)
      mv.visitCode()
      getField(mv, "cancelFlag", AtomicBoolDesc)
      mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "java/util/concurrent/atomic/AtomicBoolean", "get", "()Z", false)
      mv.visitInsn(Opcodes.IRETURN)
      mv.visitMaxs(0, 0)
      mv.visitEnd()
    }

    // setTaskName(String) — no-op
    locally {
      val mv = cw.visitMethod(Opcodes.ACC_PUBLIC, "setTaskName", "(Ljava/lang/String;)V", null, null)
      mv.visitCode()
      mv.visitInsn(Opcodes.RETURN)
      mv.visitMaxs(0, 0)
      mv.visitEnd()
    }

    // worked(int workIncrement, int remainingWork) — update atomics
    locally {
      val mv = cw.visitMethod(Opcodes.ACC_PUBLIC, "worked", "(II)V", null, null)
      mv.visitCode()
      // workedSoFar.addAndGet(workIncrement)
      getField(mv, "workedSoFar", AtomicIntDesc)
      mv.visitVarInsn(Opcodes.ILOAD, 1)
      mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "java/util/concurrent/atomic/AtomicInteger", "addAndGet", "(I)I", false)
      mv.visitInsn(Opcodes.POP)
      // totalWork.set(workedSoFar.get() + remainingWork)
      getField(mv, "totalWork", AtomicIntDesc)
      getField(mv, "workedSoFar", AtomicIntDesc)
      mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "java/util/concurrent/atomic/AtomicInteger", "get", "()I", false)
      mv.visitVarInsn(Opcodes.ILOAD, 2)
      mv.visitInsn(Opcodes.IADD)
      mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "java/util/concurrent/atomic/AtomicInteger", "set", "(I)V", false)
      mv.visitInsn(Opcodes.RETURN)
      mv.visitMaxs(0, 0)
      mv.visitEnd()
    }

    cw.visitEnd()
    cw.toByteArray
  }

  /** Helper: emit ALOAD 0; GETFIELD */
  private def getField(mv: org.objectweb.asm.MethodVisitor, name: String, desc: String): Unit = {
    mv.visitVarInsn(org.objectweb.asm.Opcodes.ALOAD, 0)
    mv.visitFieldInsn(org.objectweb.asm.Opcodes.GETFIELD, BridgeClassName, name, desc)
  }

  /** Helper: emit ALOAD 0; ALOAD slot; PUTFIELD */
  private def putField(mv: org.objectweb.asm.MethodVisitor, name: String, desc: String, slot: Int): Unit = {
    mv.visitVarInsn(org.objectweb.asm.Opcodes.ALOAD, 0)
    mv.visitVarInsn(org.objectweb.asm.Opcodes.ALOAD, slot)
    mv.visitFieldInsn(org.objectweb.asm.Opcodes.PUTFIELD, BridgeClassName, name, desc)
  }
}

/** Simple VirtualFile implementation wrapping a Path. Implements PathBasedFile so Scala 3 compiler can get the path back.
  */
private class PlainVirtualFile(val path: Path) extends VirtualFile with xsbti.PathBasedFile {
  // On Windows, Path.toString uses backslashes which break javac filename matching in Zinc's
  // Java compilation pipeline. Normalize to forward slashes so id() is platform-independent.
  def id(): String = java.io.File.separatorChar match {
    case '\\' => path.toString.replace('\\', '/')
    case _    => path.toString
  }
  def name(): String = path.getFileName.toString
  def names(): Array[String] = {
    val count = path.getNameCount
    (0 until count).map(i => path.getName(i).toString).toArray
  }
  def contentHash(): Long =
    if (Files.exists(path)) {
      val content = Files.readAllBytes(path)
      val result = sbt.internal.inc.HashUtil.farmHash(content)
      // Only log occasionally to avoid spam - just first few calls per session
      if (path.toString.contains("Build.scala")) {
        ZincBridge.debug(s"[ZincBridge] contentHash for ${path.getFileName}: $result (hex: ${java.lang.Long.toHexString(result)})")
      }
      result
    } else {
      0L
    }
  def input(): java.io.InputStream = Files.newInputStream(path)

  // PathBasedFile interface - required for Scala 3 compiler bridge
  def toPath(): Path = path

  override def toString: String = path.toString

  // CRITICAL: hashCode MUST match BasicVirtualFileRef.hashCode() for HashMap lookups to work
  // BasicVirtualFileRef uses: Objects.hash("xsbti.BasicVirtualFileRef", id)
  // This allows stamp lookups to work when Zinc serializes analysis with BasicVirtualFileRef
  // and we query it with PlainVirtualFile
  override def hashCode(): Int = java.util.Objects.hash("xsbti.BasicVirtualFileRef", id())

  // equals must work across different VirtualFileRef implementations
  override def equals(obj: Any): Boolean = obj match {
    case other: VirtualFileRef => id() == other.id()
    case _                     => false
  }
}

object PlainVirtualFile {
  def apply(path: Path): PlainVirtualFile = new PlainVirtualFile(path)
}
