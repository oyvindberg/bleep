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
  case class FileStatEntry(ctimeMillis: Long, mtimeMillis: Long, size: Long)

  case class NoopManifest(
      sourceStats: Map[Path, FileStatEntry],
      sourceDirStats: Map[Path, FileStatEntry], // source dir → stat (detects file add/delete)
      outputDirStats: Map[Path, FileStatEntry], // output dir + subdirs → stat (detects class file add/delete)
      depAnalysisStats: Map[Path, Long], // outputDir → dep analysis mtime millis
      optionsHash: Long,
      cachedResult: ProjectCompileSuccess
  )

  /** True on macOS/Linux. False on Windows where Java's `unix:ctime` attribute is unsupported. When false, [[isNoop]] short-circuits to None and the manifest
    * mechanism is fully disabled.
    */
  val ctimeAvailable: Boolean =
    !System.getProperty("os.name", "").toLowerCase.contains("win")

  private val NoopManifestMagic: Int = 0x4e4f4f50
  private val NoopManifestVersion: Byte = 3

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
      size = attrs.size()
    )
  }

  /** Drop source directories that are nested under another source directory. E.g. given {src, src/java}, drop src/java since walking src already covers it. */
  def removeNestedDirs(dirs: Set[Path]): Set[Path] = {
    val abs = dirs.map(_.toAbsolutePath.normalize())
    abs.filter(d => !abs.exists(other => other != d && d.startsWith(other)))
  }

  /** FNV-1a hash of compiler options for cheap equality check. */
  def computeOptionsHash(language: ProjectLanguage.ScalaJava, ecjVersion: Option[String]): Long = {
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
    hash
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
      result: ProjectCompileSuccess
  ): Option[NoopManifest] = {
    if (!ctimeAvailable) return None

    val sourceStats = new java.util.HashMap[Path, FileStatEntry](sourceFiles.length * 2)
    var i = 0
    while (i < sourceFiles.length) {
      val path = sourceFiles(i)
      sourceStats.put(path, statFile(path))
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
      manifest.depAnalysisStats.foreach { case (outputDir, mtime) =>
        out.writeUTF(outputDir.toString)
        out.writeLong(mtime)
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
        sourceStatsBuilder += path -> FileStatEntry(ctime, mtime, size)
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
        sourceDirStatsBuilder += dir -> FileStatEntry(dirCtime, dirMtime, dirSize)
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
        outputDirStatsBuilder += dir -> FileStatEntry(outCtime, outMtime, outSize)
        i += 1
      }

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
