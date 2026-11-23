package bleep

import coursier.jvm.JvmChannel

import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.{FileVisitResult, Files, Path, SimpleFileVisitor}
import java.security.MessageDigest
import scala.collection.immutable.SortedMap
import scala.collection.mutable
import scala.jdk.StreamConverters.*

/** Computes a per-project SHA-256 digest capturing everything that affects compilation output.
  *
  * The digest includes:
  *   - The bleep version
  *   - The build-level toolchain JVM (`jvm:` in `bleep.yaml`) — the JDK that compiles and runs everything
  *   - Project configuration (deps, compiler flags, scala version, platform, etc.)
  *   - Source file content hashes (via `git ls-tree` for clean dirs, filesystem for dirty content) — excluding sourcegen output, see below
  *   - Resource file content hashes (affects key but resources are NOT cached) — excluding sourcegen output, see below
  *   - Per `sourcegen:` declaration, a generator digest folded in place of that declaration's `generated-sources`/`generated-resources` directories: the script
  *     project's own digest, each declared `inputs` project's digest, and the content of any `sourceGlobs` directories (see [[computeGeneratorDigest]]). A
  *     generator's output is a deterministic function of those — never hashed directly, so computing this digest never requires the generator to have actually
  *     run. This is also what [[RemoteCache]]'s generator-output cache keys its entries on.
  *   - Transitive dependency project digests (if B depends on A, B's digest includes A's digest)
  *   - Indirect dependency project digests ([[model.IndirectDependency]]): whatever compiles or rewrites this project's code, other than a sourcegen script —
  *     those are folded via the generator digest above instead
  *
  * What goes in is content, never location: every file contributes its path *relative to the declared directory* plus a git blob hash of its bytes. That is
  * what makes the digest a portable cache key — two checkouts of the same commit at different absolute paths, on different operating systems, must agree. Any
  * new input has to meet the same bar, which is why `sourceGlobs` directories are hashed exactly like source directories rather than, say, by mtime.
  *
  * Computed bottom-up through the dependency DAG so leaf projects are digested first.
  *
  * For performance on large repos, uses `git ls-tree` to get precomputed content hashes when the working tree is clean for a directory. Falls back to
  * filesystem hashing for directories with uncommitted changes or generated sources not tracked by git. For a historical ref (see [[computeAllAt]]), a
  * directory inside a git submodule is resolved through the submodule's own history instead (see [[gitLsTreeThroughSubmodules]]) — `git ls-tree` never descends
  * into one from the superproject, at any ref, no matter how deep `-r` is asked to go.
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
      buildPaths: BuildPaths,
      baseReplacements: model.Replacements
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
          val projectPaths = buildPaths.project(crossName, project, build.crossPlatforms(crossName.name), baseReplacements)

          val md = MessageDigest.getInstance("SHA-256")

          // 0. Bleep version (different versions produce different compilation output)
          md.update(build.$version.value.getBytes("UTF-8"))

          // 1. Build-level toolchain JVM (a different JDK produces different class files, and `jvm:` reaches no project's config)
          md.update(jvm.name.getBytes("UTF-8"))
          md.update(jvmIndex.getBytes("UTF-8"))

          // 2. Project config (deterministic YAML). Excludes what cannot change a class file: `publish`, and `stamp` — declaring a stamp only decides what
          // gets written into the artifact, so turning one on must not evict the project's cache entry. And `extends`: the project is already exploded, so
          // the templates' settings are in it; their names are only how the build file spells them, and renaming or inlining one changes no output.
          val configForDigest = project.copy(publish = None, stamp = model.JsonSet.empty, `extends` = model.JsonSet.empty)
          val configYaml = yaml.encodeShortened(configForDigest)
          md.update(configYaml.getBytes("UTF-8"))

          // 3. Source file content hashes, excluding sourcegen output (folded via its own generator digest in step 5 instead — see below)
          hashDirectories(md, buildPaths.buildDir, projectPaths.sourcesDirs.all(Usage.Input) -- projectPaths.sourcesDirs.generated.values, dirtyPaths)

          // 4. Resource file content hashes (affects digest, but resources are not cached), same exclusion as step 3
          hashDirectories(md, buildPaths.buildDir, projectPaths.resourcesDirs.all(Usage.Input) -- projectPaths.resourcesDirs.generated.values, dirtyPaths)

          // 5. Per sourcegen declaration, a generator digest folded in place of hashing `generated-sources`/`generated-resources` directly — see
          // [[computeGeneratorDigest]]. Requires the generator to have actually run only when its output is actually missing from disk at compile time,
          // never merely to compute this digest.
          project.sourcegen.values.toList.foreach { case s: model.ScriptDef.Main =>
            md.update(computeGeneratorDigest(buildPaths.buildDir, s, projectPaths, dirtyPaths, compute).getBytes("UTF-8"))
          }

          // 6. Transitive dependency digests (sorted for determinism)
          val deps = build.resolvedDependsOn.getOrElse(crossName, Set.empty)
          deps.toList.sorted.foreach { dep =>
            md.update(compute(dep).getBytes("UTF-8"))
          }

          // 7. Indirect dependency digests other than a sourcegen script or its inputs (those are folded via step 5 instead): a compiler, a post-compile
          // script, or its inputs, each decide this project's output without reaching its classpath.
          build.resolvedIndirectDependencies(crossName).foreach {
            case dep if isSourcegenReason(dep.reason) => ()
            case dep                                  => md.update(compute(dep.project).getBytes("UTF-8"))
          }

          Checksums.byteArrayToHexString(md.digest())
        }
      )

    build.explodedProjects.keys.foreach(compute)
    SortedMap.from(digests)
  }

  /** Like [[computeAll]], but as of a historical commit rather than the working tree — used by `bleep remote-cache pull --unsafe-seed-ref` to find a cache
    * entry pushed at an ancestor commit that can seed Zinc's incremental compiler after an exact-digest miss.
    *
    * Reuses the *current* exploded project model (config, dependency graph) for every project; only source/resource/sourceGlobs content is read as of `ref`
    * instead of the working tree. This is a deliberate approximation, not a shortcut taken for lack of a better one: if a project's own config changed since
    * `ref`, the digest computed here will not equal what was actually pushed back then — but a wrong digest only ever misses the lookup, since the cache is
    * content-addressed. It can never produce a false hit. Reconstructing the historical build model (re-exploding `bleep.yaml` as it stood at `ref`, were it to
    * have changed) is not attempted, because the only thing it would buy is a few more true hits, never a safer one.
    *
    * A project's digest is left out of the result (rather than guessed at) when any of its source/resource/sourceGlobs directories currently holds real content
    * that git has nothing tracked under at `ref` — generated/gitignored output, or a project created after `ref`. There is no way to know what such a directory
    * held at a past commit without checking it out, and silently hashing the current filesystem under a historical label would produce a digest that
    * corresponds to no build that ever actually ran.
    *
    * @return
    *   map from project name to hex-encoded SHA-256 digest, omitting projects whose digest-at-`ref` could not be determined
    */
  def computeAllAt(
      build: model.Build,
      buildPaths: BuildPaths,
      ref: String,
      baseReplacements: model.Replacements
  ): SortedMap[model.CrossProjectName, String] = {
    val digests = mutable.Map.empty[model.CrossProjectName, Option[String]]

    val jvm: model.Jvm = build.jvm.getOrElse(
      throw new BleepException.Text(
        s"no `jvm:` in ${BuildLoader.BuildFileName}. Compilation output depends on which JDK compiles it, so a build which leaves that to whatever `java` is on PATH cannot be given a content digest. Pin the toolchain, for instance:\n  jvm:\n    name: graalvm-community:24.0.1"
      )
    )
    val jvmIndex: String = jvm.index.getOrElse(JvmChannel.gitHubIndexUrl)

    def compute(crossName: model.CrossProjectName): Option[String] =
      digests.getOrElseUpdate(
        crossName, {
          val project = build.explodedProjects(crossName)
          val projectPaths = buildPaths.project(crossName, project, build.crossPlatforms(crossName.name), baseReplacements)

          val md = MessageDigest.getInstance("SHA-256")
          md.update(build.$version.value.getBytes("UTF-8"))
          md.update(jvm.name.getBytes("UTF-8"))
          md.update(jvmIndex.getBytes("UTF-8"))

          val configForDigest = project.copy(publish = None, stamp = model.JsonSet.empty, `extends` = model.JsonSet.empty)
          md.update(yaml.encodeShortened(configForDigest).getBytes("UTF-8"))

          // Same exclusion as [[computeAll]]'s steps 3/4: generated output is folded via its own generator digest below, never hashed directly — which is
          // exactly what lets a project whose generated output isn't git-tracked (every one of them) still get a historical digest at all.
          val ownContentResolvable =
            hashDirectoriesAt(md, buildPaths.buildDir, ref, projectPaths.sourcesDirs.all(Usage.Input) -- projectPaths.sourcesDirs.generated.values) &&
              hashDirectoriesAt(md, buildPaths.buildDir, ref, projectPaths.resourcesDirs.all(Usage.Input) -- projectPaths.resourcesDirs.generated.values)

          if (!ownContentResolvable) None
          else {
            val sourcegenDigests: List[Option[String]] = project.sourcegen.values.toList.collect { case s: model.ScriptDef.Main =>
              computeGeneratorDigestAt(buildPaths.buildDir, ref, s, projectPaths, compute)
            }

            val deps = build.resolvedDependsOn.getOrElse(crossName, Set.empty)
            val depDigests = deps.toList.sorted.map(compute)
            val indirectDigests = build.resolvedIndirectDependencies(crossName).toList.collect {
              case dep if !isSourcegenReason(dep.reason) => compute(dep.project)
            }

            if ((depDigests ++ indirectDigests ++ sourcegenDigests).exists(_.isEmpty)) None
            else {
              // Folded in the same order as computeAll's steps 5/6/7: sourcegen digests, then deps, then non-sourcegen indirect deps. MessageDigest is
              // order-sensitive, so this isn't cosmetic — a different fold order here would make this function disagree with computeAll on identical
              // inputs, which defeats the one thing it exists to do (reproduce a digest a real push could have produced).
              sourcegenDigests.foreach(d => md.update(d.get.getBytes("UTF-8")))
              (depDigests ++ indirectDigests).foreach(d => md.update(d.get.getBytes("UTF-8")))
              Some(Checksums.byteArrayToHexString(md.digest()))
            }
          }
        }
      )

    build.explodedProjects.keys.foreach(compute)
    SortedMap.from(digests.collect { case (crossName, Some(digest)) => crossName -> digest })
  }

  private def isSourcegenReason(reason: model.IndirectDependency.Reason): Boolean = reason match {
    case _: model.IndirectDependency.Reason.Sourcegen      => true
    case _: model.IndirectDependency.Reason.SourcegenInput => true
    case _                                                 => false
  }

  /** Every `(consumer, declaration)` pair for sourcegen entries across the build. Declarations live on [[model.Project.sourcegen]] per-project, not in one
    * global list, so this is the one place that walks every project asking "do you have one".
    */
  private def allSourcegenEntries(build: model.Build): List[(model.CrossProjectName, model.ScriptDef.Main)] =
    build.explodedProjects.toList.flatMap { case (crossName, project) =>
      project.sourcegen.values.toList.collect { case s: model.ScriptDef.Main => crossName -> s }
    }

  /** A digest for one sourcegen declaration's output, as consumed by `consumerPaths`'s project — what [[RemoteCache]] keys its generator-output cache entries
    * on, and what [[computeAll]] folds in place of hashing that declaration's `generated-sources`/`generated-resources` directories directly.
    *
    * Deliberately independent of everything else about the consuming project: only the generator's own declared inputs decide what it produces — the script
    * project's digest (recursively, via `resolveDigest`, so a change to the generator's own implementation busts this too), each declared `inputs` project's
    * digest, and the content of any `sourceGlobs` directories (resolved against the consumer, since a declaration can point `sourceGlobs` at consumer-specific
    * paths). This is a deliberate equivalence, not an approximation: a sourcegen script is required to be a deterministic function of exactly these inputs for
    * any of bleep's existing digest-based invalidation to be sound at all (today's [[ProjectDigest]] already trusts that a generator produces the same output
    * from the same inputs on every machine, just as it trusts the Scala/Java compiler to) — so hashing the inputs here is equivalent to hashing the real output
    * bytes, and unlike the real bytes, never requires the generator to have actually run.
    */
  private def computeGeneratorDigest(
      buildDir: Path,
      script: model.ScriptDef.Main,
      consumerPaths: ProjectPaths,
      dirtyPaths: Option[Set[Path]],
      resolveDigest: model.CrossProjectName => String
  ): String = {
    val md = MessageDigest.getInstance("SHA-256")
    md.update(script.main.getBytes("UTF-8"))
    md.update(resolveDigest(script.project).getBytes("UTF-8"))
    script.inputs.values.toList.sorted.foreach(p => md.update(resolveDigest(p).getBytes("UTF-8")))
    hashDirectories(md, buildDir, ProjectInputs.declaredSourcegenInputs(script, consumerPaths).toSet, dirtyPaths)
    Checksums.byteArrayToHexString(md.digest())
  }

  /** Like [[computeGeneratorDigest]], but as of a historical ref — see [[computeAllAt]] for why this can come back empty rather than guessing. Unlike
    * [[computeAllAt]]'s own refusal (which is unavoidable for actual generated content, never git-tracked), this one only fails when the generator's own
    * declared inputs — ordinary projects and `sourceGlobs`, almost always git-tracked — cannot be resolved as of `ref`.
    */
  private def computeGeneratorDigestAt(
      buildDir: Path,
      ref: String,
      script: model.ScriptDef.Main,
      consumerPaths: ProjectPaths,
      resolveDigest: model.CrossProjectName => Option[String]
  ): Option[String] = {
    val md = MessageDigest.getInstance("SHA-256")
    md.update(script.main.getBytes("UTF-8"))

    val depDigests = (script.project :: script.inputs.values.toList.sorted).map(resolveDigest)
    if (depDigests.exists(_.isEmpty)) None
    else {
      depDigests.foreach(d => md.update(d.get.getBytes("UTF-8")))
      if (hashDirectoriesAt(md, buildDir, ref, ProjectInputs.declaredSourcegenInputs(script, consumerPaths).toSet))
        Some(Checksums.byteArrayToHexString(md.digest()))
      else None
    }
  }

  /** A generator digest (see [[computeGeneratorDigest]]) for every sourcegen declaration in the build, as of the working tree. Used by
    * `bleep remote-cache pull`/`push` to populate or publish generated output *before* a consuming project's own digest is computed — which is what lets the
    * cache serve a project with a sourcegen dependency without ever having run the generator locally first.
    *
    * `projectDigests` supplies already-computed project digests rather than recomputing them, so this never duplicates [[computeAll]]'s own bottom-up walk;
    * pass it the result of a single prior call to [[computeAll]].
    */
  def computeSourcegenDigests(
      build: model.Build,
      buildPaths: BuildPaths,
      projectDigests: model.CrossProjectName => String,
      baseReplacements: model.Replacements
  ): Map[(model.CrossProjectName, model.ScriptDef.Main), String] = {
    val dirtyPaths = gitDirtyPaths(buildPaths.buildDir)
    allSourcegenEntries(build).map { case (crossName, script) =>
      val project = build.explodedProjects(crossName)
      val consumerPaths = buildPaths.project(crossName, project, build.crossPlatforms(crossName.name), baseReplacements)
      (crossName, script) -> computeGeneratorDigest(buildPaths.buildDir, script, consumerPaths, dirtyPaths, projectDigests)
    }.toMap
  }

  /** Like [[computeSourcegenDigests]], but as of a historical ref — pass the result of a prior call to [[computeAllAt]] (as `.get`) as `projectDigestsAt`.
    * Omits declarations whose digest-at-`ref` could not be determined, same convention as [[computeAllAt]] itself.
    */
  def computeSourcegenDigestsAt(
      build: model.Build,
      buildPaths: BuildPaths,
      ref: String,
      projectDigestsAt: model.CrossProjectName => Option[String],
      baseReplacements: model.Replacements
  ): Map[(model.CrossProjectName, model.ScriptDef.Main), String] =
    allSourcegenEntries(build).flatMap { case (crossName, script) =>
      val project = build.explodedProjects(crossName)
      val consumerPaths = buildPaths.project(crossName, project, build.crossPlatforms(crossName.name), baseReplacements)
      computeGeneratorDigestAt(buildPaths.buildDir, ref, script, consumerPaths, projectDigestsAt).map(d => (crossName, script) -> d)
    }.toMap

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
          val gitHashes = gitLsTree(buildDir, "HEAD", dir)
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
            hashFilesystem(buildDir, md, dir)
          }
        } else {
          // Dirty directory — must read from filesystem to capture uncommitted changes
          hashFilesystem(buildDir, md, dir)
        }
      }
    }

  /** Like [[hashDirectories]], but for a historical ref: no dirty-tree overlay, no filesystem fallback. Returns `false` the moment a directory currently holds
    * real file content that git has nothing tracked under at `ref` — that content cannot be known as of a past commit, so the caller must discard whatever has
    * been hashed into `md` so far rather than pass off current bytes as historical ones.
    *
    * @return
    *   `true` if every directory's content as of `ref` was fully known (possibly empty)
    */
  private def hashDirectoriesAt(
      md: MessageDigest,
      buildDir: Path,
      ref: String,
      dirs: collection.Set[Path]
  ): Boolean =
    dirs.toList.sorted.forall { dir =>
      if (!Files.isDirectory(dir)) true // doesn't exist in the current checkout either — contributes nothing, same as the working-tree computation
      else {
        val gitHashes = gitLsTreeThroughSubmodules(buildDir, ref, dir)
        if (gitHashes.nonEmpty) {
          val dirRelToRepo = buildDir.relativize(dir).toString.replace('\\', '/')
          val dirPrefix = if (dirRelToRepo.isEmpty) "" else dirRelToRepo + "/"
          gitHashes.foreach { case (repoRelPath, hash) =>
            val relPath = if (repoRelPath.startsWith(dirPrefix)) repoRelPath.substring(dirPrefix.length) else repoRelPath
            md.update(relPath.getBytes("UTF-8"))
            md.update(hash.getBytes("UTF-8"))
          }
          true
        } else {
          // Nothing tracked under `dir` at `ref`. Safe only if the directory is genuinely empty right now too — otherwise its current content is
          // untracked/generated, and substituting it here would mislabel current bytes as historical ones.
          val hasAnyFileNow = scala.util.Using.resource(Files.walk(dir))(_.toScala(List)).exists(Files.isRegularFile(_))
          !hasAnyFileNow
        }
      }
    }

  /** Get all dirty (modified, staged, untracked) file paths in the repository. Returns None if not in a git repo.
    *
    * Uses `git status --porcelain -z` which is fast and gives us all dirty paths in one call.
    *
    * ==Why `-z` is not optional==
    *
    * Without it, git *quotes* any path it considers unusual — a space is enough, and so is any byte above ASCII — and escapes the bytes in octal:
    * {{{
    *  M "shared-data/caf\303\251.txt"
    *  M "shared-data/with space.txt"
    * }}}
    * Resolving that string gives a path matching no file on disk, so [[hashDirectories]] finds nothing dirty under the directory, takes the `git ls-tree` fast
    * path, and hashes HEAD's blobs instead of the modified working tree. The digest then describes content other than what was compiled — which is precisely
    * what a content-addressed cache exists to prevent. `-z` emits NUL-terminated records with paths verbatim, so there is nothing to unquote and
    * `core.quotePath` cannot reach the digest.
    */
  private def gitDirtyPaths(buildDir: Path): Option[Set[Path]] =
    internal.gitOutput.attempt(buildDir, List("git", "status", "--porcelain", "-u", "-z")).map { output =>
      val fields = output.split('\u0000').iterator.filter(_.nonEmpty)
      val paths = Set.newBuilder[Path]
      while (fields.hasNext) {
        // Format: "XY <path>", with renames and copies followed by a *second* NUL-terminated field holding the source path — `-z` has no " -> " infix.
        val record = fields.next()
        if (record.length < 4)
          throw new BleepException.Text(s"could not parse `git status --porcelain -z` record ${'"'}$record${'"'}")
        paths += buildDir.resolve(record.substring(3)).normalize()
        val status = record.substring(0, 2)
        // Both ends of a rename moved: the source directory lost a file.
        if (status.contains('R') || status.contains('C'))
          paths += buildDir.resolve(fields.next()).normalize()
      }
      paths.result()
    }

  /** Use `git ls-tree -r -z <ref> -- <dir>` to get content hashes for all files under a directory.
    *
    * `-z` for the same reason as in [[gitDirtyPaths]], and here it is worse than a missed dirty path: the quoted spelling of a file name goes *straight into
    * the digest*. Whether a path arrives as `café.txt` or as `"caf\303\251.txt"` is decided by `core.quotePath` — a setting in the developer's own
    * `~/.gitconfig` — so without `-z` the same commit hashes differently on two machines, and the cache key stops being portable. Note also that `status` and
    * `ls-tree` do not quote alike, so there is no single unquoting rule to write even if we wanted one.
    *
    * @return
    *   sorted list of (relative-path, blob-hash) pairs, or empty if the directory isn't tracked by git at `ref`
    */
  private def gitLsTree(buildDir: Path, ref: String, dir: Path): List[(String, String)] =
    internal.gitOutput
      .attempt(buildDir, List("git", "ls-tree", "-r", "-z", ref, "--", dir.toString))
      .getOrElse("")
      .split('\u0000')
      .iterator
      .filter(_.nonEmpty)
      .map { record =>
        // Format: <mode> SP <type> SP <hash> TAB <path>. Split on the fields rather than a fixed offset: a submodule is `160000 commit <hash>`, whose type is
        // two characters wider than `blob`, and slicing at 12 would have fed the digest a fragment of the sha.
        val tabIdx = record.indexOf('\t')
        if (tabIdx < 0)
          throw new BleepException.Text(s"could not parse `git ls-tree -z` record ${'"'}$record${'"'}")
        val hash = record.substring(0, tabIdx).split(' ').last
        val path = record.substring(tabIdx + 1)
        (path, hash)
      }
      .toList
      .sortBy(_._1)

  /** Like [[gitLsTree]], but when `dir` has nothing tracked directly, also tries resolving it as content inside a git submodule — which `git ls-tree` never
    * descends into from the superproject, no matter how deep `-r` is asked to go. Finds the nearest ancestor of `dir` that is a submodule mount point as of
    * `ref` ([[findSubmodule]]), then asks THAT submodule's own git history for the exact commit the superproject pinned there — never whatever the submodule
    * happens to be checked out to right now, which could be a different commit on a different machine, or mid-update on this one. Returns paths relative to
    * `buildDir`, the same convention as [[gitLsTree]], so every caller of it needs no changes beyond calling this instead.
    *
    * Deliberately not used by [[hashDirectories]] (the current-digest path, via [[gitLsTree]] directly): that path already gets correct content for
    * submodule-sourced directories through its filesystem fallback, and giving it this fast path too would need its own check for a *dirty* submodule working
    * tree to stay correct — not worth the risk for a performance-only win when the slow path there already works. [[hashDirectoriesAt]] has no such fallback to
    * fall back to — a past ref is never "the working tree" — so it needs this to work at all, not just to go faster.
    *
    * Never touches the filesystem beyond what a plain [[gitLsTree]] call already does (one extra git process, run against the submodule's own working directory
    * instead of the superproject's). If the submodule's local clone doesn't have the pinned commit — never fetched that far back — this comes back empty,
    * exactly like an ordinary untracked directory, and [[hashDirectoriesAt]]'s existing refusal takes over. Never a crash, never a guess.
    */
  private def gitLsTreeThroughSubmodules(buildDir: Path, ref: String, dir: Path): List[(String, String)] = {
    val direct = gitLsTree(buildDir, ref, dir)
    if (direct.nonEmpty) direct
    else
      findSubmodule(buildDir, ref, dir) match {
        case None                                => Nil
        case Some((mountPoint, submoduleCommit)) =>
          val submoduleDir = buildDir.resolve(mountPoint)
          val relInSubmodule = submoduleDir.relativize(dir)
          val pathInSubmodule = if (relInSubmodule.toString.isEmpty) submoduleDir else submoduleDir.resolve(relInSubmodule)
          val mountPrefix = mountPoint.toString.replace('\\', '/')
          gitLsTree(submoduleDir, submoduleCommit, pathInSubmodule).map { case (p, h) => (s"$mountPrefix/$p", h) }
      }
  }

  /** The nearest ancestor of `dir` (including `dir` itself) that is a git submodule mount point as of `ref`, if any, paired with the commit the superproject
    * pinned there at that ref. A project's declared source directory is almost always several levels *inside* a submodule
    * (`liberated/zinc/internal/zinc-core/src/main/scala`), not the submodule's own root, which is why this walks upward rather than checking `dir` alone.
    *
    * A submodule mount point is the one shape `git ls-tree` always hands back unexpanded, no matter how it's asked: mode `160000`, type `commit` — `-r` only
    * decides whether to expand a *tree* entry further, and a submodule was never a tree to begin with.
    */
  private def findSubmodule(buildDir: Path, ref: String, dir: Path): Option[(Path, String)] = {
    val relDir = buildDir.relativize(dir)
    Iterator
      .iterate(relDir)(_.getParent)
      .takeWhile(_ != null)
      .flatMap(candidate => gitLsTreeEntry(buildDir, ref, candidate).map(candidate -> _))
      .collectFirst { case (candidate, ("160000", "commit", sha)) => candidate -> sha }
  }

  /** The single tree entry naming `path` (relative to `buildDir`) in its parent tree as of `ref`, without recursing — `git ls-tree <ref> -- <path>` (no `-r`)
    * names `path` itself rather than expanding it, whether it's a blob, an ordinary directory, or a submodule. Used only to classify `path`, never to read file
    * content ([[gitLsTree]] does that, with `-r`).
    *
    * @return
    *   `(mode, type, hash)`, or `None` if `path` is not tracked at `ref`
    */
  private def gitLsTreeEntry(buildDir: Path, ref: String, path: Path): Option[(String, String, String)] =
    internal.gitOutput
      .attempt(buildDir, List("git", "ls-tree", "-z", ref, "--", path.toString))
      .getOrElse("")
      .split('\u0000')
      .iterator
      .filter(_.nonEmpty)
      .flatMap { record =>
        val tabIdx = record.indexOf('\t')
        if (tabIdx < 0) None
        else
          record.substring(0, tabIdx).split(' ') match {
            case Array(mode, tpe, hash) => Some((mode, tpe, hash))
            case _                      => None
          }
      }
      .nextOption()

  /** Hash all files under a directory using git-compatible blob hashes. Files are sorted by relative path for determinism.
    *
    * Computes the same hash as `git hash-object` for each file: `SHA-1("blob <size>\0" + content)`. This ensures filesystem hashing produces the same digest as
    * git ls-tree hashing for identical content.
    *
    * Which files counts as much as how they're hashed. Enumerated via [[gitLsFilesUnder]] — tracked plus untracked-but-not-ignored, exactly what git itself
    * considers part of the working tree — rather than a raw filesystem walk, so gitignored content sitting inside a declared directory (build output, IDE junk,
    * and critically a nested git worktree or any other embedded repository, which is always expected to be gitignored) never enters the digest. Only when there
    * is no git repository at all to ask does this fall back to a plain walk, with an explicit check that still refuses to descend into a nested repository's
    * own working directory — the one case with no ignore rules to lean on at all.
    */
  private def hashFilesystem(buildDir: Path, md: MessageDigest, dir: Path): Unit = {
    val dirRelToRepo = buildDir.relativize(dir).toString.replace('\\', '/')
    val dirPrefix = if (dirRelToRepo.isEmpty) "" else dirRelToRepo + "/"

    val files: List[(String, Path)] = gitLsFilesUnder(buildDir, dir) match {
      case Some(repoRelPaths) =>
        repoRelPaths
          .map { repoRelPathRaw =>
            val repoRelPath = repoRelPathRaw.replace('\\', '/')
            val relPath = if (repoRelPath.startsWith(dirPrefix)) repoRelPath.substring(dirPrefix.length) else repoRelPath
            (relPath, buildDir.resolve(repoRelPath))
          }
          .filter { case (_, file) => Files.isRegularFile(file) }
      case None =>
        walkIgnoringNestedRepos(dir).map(file => (dir.relativize(file).toString.replace('\\', '/'), file))
    }

    files
      // Normalize to '/' so the digest matches git ls-tree's representation across OSes, and sort by that normalized string rather than by `Path`.
      // `Path.compareTo` is case-insensitive on Windows and byte-wise on Unix, so sorting Paths fed the digest in a different order per OS — and in a
      // different order than [[gitLsTree]], which sorts these very same strings. A digest that depends on the OS is not portable across machines, which is
      // the whole point of it. Unix ordering is unchanged (comparing full paths that share a prefix is equivalent to comparing the relative parts), so only
      // the previously-wrong Windows digests move.
      .sortBy { case (relPath, _) => relPath }
      .foreach { case (relPath, file) =>
        val content = Files.readAllBytes(file)
        val blobHash = gitBlobHash(content)
        md.update(relPath.getBytes("UTF-8"))
        md.update(blobHash.getBytes("UTF-8"))
      }
  }

  /** `git ls-files --cached --others --exclude-standard -z -- <dir>`, as paths relative to `buildDir` — every file [[hashFilesystem]] should consider part of
    * `dir`: tracked (regardless of dirty state) plus untracked-but-not-ignored. `None` if there is no git repository to ask at all, as opposed to an empty (but
    * successful) answer for a directory with nothing in it.
    *
    * git's own traversal for this already refuses to descend into a nested repository it does not know as a submodule (an embedded `.git` directory, or another
    * worktree) — it reports the nested root as a single entry and stops there, the same way `git status` does — so this gets that protection for free, with no
    * bleep-side notion of "what counts as a repo boundary" to maintain. A submodule it *does* know about is the one exception: `ls-files` doesn't descend into
    * those either, which would silently undo the one thing [[hashDirectories]]'s filesystem fallback already got right about submodule-sourced directories
    * (reading them is what the raw walk this replaces did without any git-awareness at all) — so this recurses into the submodule's own working directory the
    * same way [[gitLsTreeThroughSubmodules]] does for a historical ref, just asking for its current state instead of a pinned commit.
    */
  private def gitLsFilesUnder(buildDir: Path, dir: Path): Option[List[String]] =
    internal.gitOutput.attempt(buildDir, List("git", "ls-files", "--cached", "--others", "--exclude-standard", "-z", "--", dir.toString)) match {
      case None         => None // no git repository at buildDir at all
      case Some(output) =>
        val direct = output.split('\u0000').iterator.filter(_.nonEmpty).toList
        if (direct.nonEmpty) Some(direct)
        else
          findSubmodule(buildDir, "HEAD", dir) match {
            case None                  => Some(Nil)
            case Some((mountPoint, _)) =>
              val submoduleDir = buildDir.resolve(mountPoint)
              val relInSubmodule = submoduleDir.relativize(dir)
              val pathInSubmodule = if (relInSubmodule.toString.isEmpty) submoduleDir else submoduleDir.resolve(relInSubmodule)
              val mountPrefix = mountPoint.toString.replace('\\', '/')
              // `.orElse(Some(Nil))`: the submodule directory existing but not being its own git repo (never `submodule update --init`) means nothing is
              // checked out there yet, not that `buildDir` itself has no repository — the `None` case above is strictly about `buildDir`.
              gitLsFilesUnder(submoduleDir, pathInSubmodule).map(_.map(p => s"$mountPrefix/$p")).orElse(Some(Nil))
          }
    }

  /** Plain filesystem walk for the one case [[gitLsFilesUnder]] can't help with: no git repository here at all, so no ignore rules and no repo-boundary answer
    * to lean on. Still refuses to descend into a directory that is itself the root of a *different* repository (its own `.git` file or directory) — a nested
    * git worktree most likely, since that's the shape this exists to guard against — so this degraded path isn't left with zero protection just because git
    * itself isn't available to ask.
    */
  private def walkIgnoringNestedRepos(dir: Path): List[Path] = {
    val files = List.newBuilder[Path]
    Files.walkFileTree(
      dir,
      new SimpleFileVisitor[Path] {
        override def preVisitDirectory(d: Path, attrs: BasicFileAttributes): FileVisitResult =
          if (d != dir && Files.exists(d.resolve(".git"))) FileVisitResult.SKIP_SUBTREE else FileVisitResult.CONTINUE
        override def visitFile(f: Path, attrs: BasicFileAttributes): FileVisitResult = {
          if (Files.isRegularFile(f)) files += f: Unit
          FileVisitResult.CONTINUE
        }
      }
    ): Unit
    files.result()
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
