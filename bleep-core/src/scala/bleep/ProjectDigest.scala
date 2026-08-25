package bleep

import coursier.jvm.JvmChannel

import java.nio.file.{Files, Path}
import java.security.MessageDigest
import scala.collection.immutable.SortedMap
import scala.collection.mutable
import scala.jdk.StreamConverters.*
import scala.util.control.NonFatal

/** Computes a per-project SHA-256 digest capturing everything that affects compilation output.
  *
  * The digest includes:
  *   - The bleep version
  *   - The build-level toolchain JVM (`jvm:` in `bleep.yaml`) — the JDK that compiles and runs everything
  *   - Project configuration (deps, compiler flags, scala version, platform, etc.)
  *   - Source file content hashes (via `git ls-tree` for clean dirs, filesystem for dirty/generated)
  *   - Resource file content hashes (affects key but resources are NOT cached)
  *   - Content hashes of directories declared under `sourceGlobs` on a `sourcegen:` entry, because a generator that reads them produces different sources when
  *     they change
  *   - Transitive dependency project digests (if B depends on A, B's digest includes A's digest)
  *
  * What goes in is content, never location: every file contributes its path *relative to the declared directory* plus a git blob hash of its bytes. That is
  * what makes the digest a portable cache key — two checkouts of the same commit at different absolute paths, on different operating systems, must agree. Any
  * new input has to meet the same bar, which is why `sourceGlobs` directories are hashed exactly like source directories rather than, say, by mtime.
  *
  * Computed bottom-up through the dependency DAG so leaf projects are digested first.
  *
  * For performance on large repos, uses `git ls-tree` to get precomputed content hashes when the working tree is clean for a directory. Falls back to
  * filesystem hashing for directories with uncommitted changes or generated sources not tracked by git.
  *
  * ==Why the JVM is in here==
  *
  * `jvm:` is a *build*-level field, so it does not appear in any project's configuration YAML. Left out of the digest, bumping the toolchain JDK changed no
  * project digest at all: `bleep remote-cache pull` happily served classes compiled by the previous JDK, and nothing anywhere reported a change. Both the name
  * and the index url are hashed, because the index is what maps a name like `temurin:21` to an actual distribution — point it at a different index and the same
  * name is a different JDK. A build with no `jvm:` cannot be digested at all: the toolchain is then whatever `java` happens to be on `PATH`, which is not an
  * input we can hash, so [[computeAll]] throws instead of pretending the toolchain is known.
  *
  * ==Why `resolvers:` is deliberately NOT in here==
  *
  * Do not "fix" this by adding it. A Maven coordinate is expected to resolve to identical bytes whichever repository serves it, and if an artifact is missing
  * from the configured repositories then resolution fails hard before anything is compiled — there is no path where a changed resolver list silently produces
  * different output. Hashing resolvers would, on the other hand, invalidate every project in the build the moment someone adds a repository for one new
  * dependency, buying nothing in return. The one case it would catch — two repositories serving different bytes under one coordinate — is a supply-chain
  * incident, not a cache-correctness problem, and a cache key is the wrong place to defend against it.
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

    // The toolchain JDK is a build-level input shared by every project. Determined once, up front, so a build without one fails before any hashing happens.
    val jvm: model.Jvm = build.jvm.getOrElse(
      throw new BleepException.Text(
        s"no `jvm:` in ${BuildLoader.BuildFileName}. Compilation output depends on which JDK compiles it, so a build which leaves that to whatever `java` is on PATH cannot be given a content digest. Pin the toolchain, for instance:\n  jvm:\n    name: graalvm-community:24.0.1"
      )
    )
    // `index` selects the channel a name like `temurin:21` is looked up in. `None` means coursier's default index, which is exactly what [[FetchJvm]] resolves
    // it to, so hash that url rather than a stand-in for "unset" — an explicitly configured default index is the same toolchain as an omitted one.
    val jvmIndex: String = jvm.index.getOrElse(JvmChannel.gitHubIndexUrl)

    // Pre-compute set of dirty paths once (much cheaper than per-directory git status calls)
    val dirtyPaths = gitDirtyPaths(buildPaths.buildDir)

    // Bottom-up: compute leaves first, then projects that depend on them
    def compute(crossName: model.CrossProjectName): String =
      digests.getOrElseUpdate(
        crossName, {
          val project = build.explodedProjects(crossName)
          val projectPaths = buildPaths.project(crossName, project)

          val md = MessageDigest.getInstance("SHA-256")

          // 0. Bleep version (different versions produce different compilation output)
          md.update(build.$version.value.getBytes("UTF-8"))

          // 1. Build-level toolchain JVM (a different JDK produces different class files, and `jvm:` reaches no project's config)
          md.update(jvm.name.getBytes("UTF-8"))
          md.update(jvmIndex.getBytes("UTF-8"))

          // 2. Project config (deterministic YAML, excluding publish which doesn't affect compilation)
          val configForDigest = project.copy(publish = None)
          val configYaml = yaml.encodeShortened(configForDigest)
          md.update(configYaml.getBytes("UTF-8"))

          // 3. Source file content hashes
          hashDirectories(md, buildPaths.buildDir, projectPaths.sourcesDirs.all, dirtyPaths)

          // 4. Resource file content hashes (affects digest, but resources are not cached)
          hashDirectories(md, buildPaths.buildDir, projectPaths.resourcesDirs.all, dirtyPaths)

          // 5. Directories this project declared as sourcegen inputs via `sourceGlobs`.
          // Kept as its own step rather than folded into 2/3 so the bytes fed for projects without
          // `sourceGlobs` — which is nearly all of them — are byte-for-byte what they were before.
          hashDirectories(md, buildPaths.buildDir, ProjectInputs.declaredSourcegenInputs(project, projectPaths).toSet, dirtyPaths)

          // 6. Transitive dependency digests (sorted for determinism)
          val deps = build.resolvedDependsOn.getOrElse(crossName, Set.empty)
          deps.toList.sorted.foreach { dep =>
            md.update(compute(dep).getBytes("UTF-8"))
          }

          // 7. Sourcegen dependency digests
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
            // git ls-tree returns paths relative to repo root with '/' separators on every OS.
            // Normalize the dir prefix so the strip works on Windows.
            val dirRelToRepo = buildDir.relativize(dir).toString.replace('\\', '/')
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
        stream.toScala(List).filter(Files.isRegularFile(_))
      }
      .getOrElse(Nil)
      // Normalize to '/' so the digest matches git ls-tree's representation across OSes, and sort by that normalized string rather than by `Path`.
      // `Path.compareTo` is case-insensitive on Windows and byte-wise on Unix, so sorting Paths fed the digest in a different order per OS — and in a
      // different order than [[gitLsTree]], which sorts these very same strings. A digest that depends on the OS is not portable across machines, which is the
      // whole point of it. Unix ordering is unchanged (comparing full paths that share a prefix is equivalent to comparing the relative parts), so only the
      // previously-wrong Windows digests move.
      .map(file => (dir.relativize(file).toString.replace('\\', '/'), file))
      .sortBy { case (relPath, _) => relPath }

    files.foreach { case (relPath, file) =>
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
