package bleep

import java.nio.file.{Files, Path}
import java.security.MessageDigest
import scala.collection.immutable.SortedMap
import scala.collection.mutable
import scala.jdk.StreamConverters.*
import scala.util.control.NonFatal

/** Computes a per-project SHA-256 digest capturing everything that affects compilation output.
  *
  * The digest includes:
  *   - Project configuration (deps, compiler flags, scala version, platform, etc.)
  *   - Source file content hashes (via `git ls-tree` for clean dirs, filesystem for dirty/generated)
  *   - Resource file content hashes (affects key but resources are NOT cached)
  *   - Transitive dependency project digests (if B depends on A, B's digest includes A's digest)
  *
  * Computed bottom-up through the dependency DAG so leaf projects are digested first.
  *
  * For performance on large repos, uses `git ls-tree` to get precomputed content hashes when the working tree is clean for a directory. Falls back to
  * filesystem hashing for directories with uncommitted changes or generated sources not tracked by git.
  */
object ProjectDigest {

  /** Compute digests for all projects in the build.
    *
    * @return
    *   map from project name to hex-encoded SHA-256 digest
    */
  def computeAll(
      build: model.Build,
      buildPaths: BuildPaths
  ): SortedMap[model.CrossProjectName, String] = {
    val digests = mutable.Map.empty[model.CrossProjectName, String]

    // Pre-compute set of dirty paths once (much cheaper than per-directory git status calls)
    val dirtyPaths = gitDirtyPaths(buildPaths.buildDir)

    // Bottom-up: compute leaves first, then projects that depend on them
    def compute(crossName: model.CrossProjectName): String =
      digests.getOrElseUpdate(
        crossName, {
          val project = build.explodedProjects(crossName)
          val projectPaths = buildPaths.project(crossName, project)

          val md = MessageDigest.getInstance("SHA-256")

          // 1. Project config (deterministic YAML, excluding publish which doesn't affect compilation)
          val configForDigest = project.copy(publish = None)
          val configYaml = yaml.encodeShortened(configForDigest)
          md.update(configYaml.getBytes("UTF-8"))

          // 2. Source file content hashes
          hashDirectories(md, buildPaths.buildDir, projectPaths.sourcesDirs.all, dirtyPaths)

          // 3. Resource file content hashes (affects digest, but resources are not cached)
          hashDirectories(md, buildPaths.buildDir, projectPaths.resourcesDirs.all, dirtyPaths)

          // 4. Transitive dependency digests (sorted for determinism)
          val deps = build.resolvedDependsOn.getOrElse(crossName, Set.empty)
          deps.toList.sorted.foreach { dep =>
            md.update(compute(dep).getBytes("UTF-8"))
          }

          // 5. Sourcegen dependency digests
          project.sourcegen.values.foreach { case model.ScriptDef.Main(sourcegenProject, _, _) =>
            md.update(compute(sourcegenProject).getBytes("UTF-8"))
          }

          Checksums.byteArrayToHexString(md.digest())
        }
      )

    build.explodedProjects.keys.foreach(compute)
    SortedMap.from(digests)
  }

  /** Hash all files under the given directories into the MessageDigest.
    *
    * For each directory: if the working tree is clean (no dirty files under it), uses `git ls-tree` for precomputed content hashes (fast, no file I/O).
    * Otherwise falls back to filesystem hashing.
    */
  private def hashDirectories(
      md: MessageDigest,
      buildDir: Path,
      dirs: collection.Set[Path],
      dirtyPaths: Option[Set[Path]]
  ): Unit =
    dirs.toList.sorted.foreach { dir =>
      if (Files.isDirectory(dir)) {
        val isDirty = dirtyPaths match {
          case Some(dirty) => dirty.exists(_.startsWith(dir))
          case None        => true // not in a git repo, always use filesystem
        }

        if (!isDirty) {
          // Clean directory — use git blob hashes (fast, no file I/O)
          val gitHashes = gitLsTree(buildDir, dir)
          if (gitHashes.nonEmpty) {
            // git ls-tree returns paths relative to repo root. Make them relative to dir.
            val dirRelToRepo = buildDir.relativize(dir).toString
            val dirPrefix = if (dirRelToRepo.isEmpty) "" else dirRelToRepo + "/"
            gitHashes.foreach { case (repoRelPath, hash) =>
              val relPath = if (repoRelPath.startsWith(dirPrefix)) repoRelPath.substring(dirPrefix.length) else repoRelPath
              md.update(relPath.getBytes("UTF-8"))
              md.update(hash.getBytes("UTF-8"))
            }
          } else {
            // Directory exists but git doesn't know about it (generated sources)
            hashFilesystem(md, dir)
          }
        } else {
          // Dirty directory — must read from filesystem to capture uncommitted changes
          hashFilesystem(md, dir)
        }
      }
    }

  /** Get all dirty (modified, staged, untracked) file paths in the repository. Returns None if not in a git repo.
    *
    * Uses `git status --porcelain` which is fast and gives us all dirty paths in one call.
    */
  private def gitDirtyPaths(buildDir: Path): Option[Set[Path]] =
    try {
      val output = scala.sys.process
        .Process(
          List("git", "status", "--porcelain", "-u"),
          buildDir.toFile
        )
        .!!
      val paths = output.linesIterator
        .filter(_.length > 3)
        .map { line =>
          // Format: "XY <path>" or "XY <path> -> <path>" (for renames)
          val pathPart = line.substring(3).split(" -> ").last
          buildDir.resolve(pathPart).normalize()
        }
        .toSet
      Some(paths)
    } catch {
      case NonFatal(_) => None // not in a git repo
    }

  /** Use `git ls-tree -r HEAD -- <dir>` to get content hashes for all files under a directory.
    *
    * @return
    *   sorted list of (relative-path, blob-hash) pairs, or empty if the directory isn't tracked by git
    */
  private def gitLsTree(buildDir: Path, dir: Path): List[(String, String)] =
    try {
      val output = scala.sys.process
        .Process(
          List("git", "ls-tree", "-r", "HEAD", "--", dir.toString),
          buildDir.toFile
        )
        .!!
      output.linesIterator
        .filter(_.nonEmpty)
        .map { line =>
          // Format: <mode> <type> <hash>\t<path>
          val tabIdx = line.indexOf('\t')
          val hash = line.substring(12, tabIdx) // skip "<mode> blob "
          val path = line.substring(tabIdx + 1)
          (path, hash)
        }
        .toList
        .sortBy(_._1)
    } catch {
      case NonFatal(_) => Nil
    }

  /** Hash all files under a directory using git-compatible blob hashes. Files are sorted by relative path for determinism.
    *
    * Computes the same hash as `git hash-object` for each file: `SHA-1("blob <size>\0" + content)`. This ensures filesystem hashing produces the same digest as
    * git ls-tree hashing for identical content.
    */
  private def hashFilesystem(md: MessageDigest, dir: Path): Unit = {
    val files = scala.util
      .Using(Files.walk(dir)) { stream =>
        stream.toScala(List).filter(Files.isRegularFile(_)).sorted
      }
      .getOrElse(Nil)

    files.foreach { file =>
      val relPath = dir.relativize(file).toString
      val content = Files.readAllBytes(file)
      val blobHash = gitBlobHash(content)
      md.update(relPath.getBytes("UTF-8"))
      md.update(blobHash.getBytes("UTF-8"))
    }
  }

  /** Compute the git blob hash for file content: `SHA-1("blob <size>\0" + content)`.
    *
    * This matches what `git hash-object` produces, ensuring consistency between filesystem and git ls-tree hashing paths.
    */
  private def gitBlobHash(content: Array[Byte]): String = {
    val header = s"blob ${content.length}\u0000"
    val sha1 = MessageDigest.getInstance("SHA-1")
    sha1.update(header.getBytes("UTF-8"))
    sha1.update(content)
    Checksums.byteArrayToHexString(sha1.digest())
  }
}
