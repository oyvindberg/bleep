package bleep

import java.nio.file.Path

/** The single authority on which parts of a project's compiled state may be SHARED with another workspace or machine, consumed by both sharing mechanisms:
  * `bleep copy-state` (seeding a fresh worktree) and the remote cache (push/pull archives).
  *
  * Before this existed, each mechanism kept its own hand-maintained list and they had already drifted: copy-state correctly skipped `.zinc/cache`, KSP caches
  * and lock files, while the remote cache shipped everything in the variant dir except the noop manifest — cache junk, per-machine KSP state, and even a
  * `.bleep-lock` if one was present at pack time. Two lists that must agree is drift by design; one allow-list consulted by both is not.
  *
  * ==Deny by default==
  *
  * Classification is an ALLOW-list over paths relative to a project's variant build dir (`.bleep/projects/<cross>/builds/<variant>/`). Anything not explicitly
  * listed is workspace-private: new state added in the future is unshared until someone consciously declares it shareable, never leaked by omission. The
  * private set today, for the record:
  *
  *   - `noop-manifest.bin` — validates against THIS workspace's absolute paths; copied, it fakes a noop pointing at the source workspace's classes
  *   - `.zinc/cache/` — regenerates; also full of absolute paths
  *   - `ksp/` — per-variant KSP caches, per-machine
  *   - `.bleep-lock` — a lock file must never be inherited
  *   - `bloop.json` and anything else that regenerates on the next compile
  *
  * Workspace-level state (`.bleep/builds/<variant>/` — request transcripts, logs) is ALL private and outside this classification entirely: both sharing
  * mechanisms operate per-project and never touch it. [[BuildPaths.requestsDir]] documents why transcripts must stay put.
  *
  * Generated sources/resources (`.bleep/projects/<cross>/generated-*`) live outside the variant dir; copy-state clones them wholesale (they are outputs keyed
  * by the build, portable by construction) and the remote cache deliberately does not archive them — that asymmetry is intentional and encoded at the call
  * sites, not here.
  */
object StateSharing {

  /** A shareable entry, relative to the variant build dir, unix separators. */
  sealed trait Shared { def rel: String }
  case class SharedDir(rel: String) extends Shared
  case class SharedFile(rel: String) extends Shared

  /** Everything under a variant build dir that may leave this workspace. */
  val variantDirEntries: List[Shared] = List(
    SharedDir("classes"),
    SharedDir("test-classes"),
    SharedFile(".zinc/analysis.zip")
  )

  /** Is a path relative to the variant build dir (unix separators) shareable? Used by the remote cache's archive filter. */
  def isShareableRel(rel: String): Boolean =
    variantDirEntries.exists {
      case SharedDir(d)  => rel == d || rel.startsWith(d + "/")
      case SharedFile(f) => rel == f
    }

  /** Archive filter over absolute paths under `variantBuildDir`: the remote cache packs exactly the shareable set. */
  def isShareableIn(variantBuildDir: Path)(p: Path): Boolean =
    isShareableRel(variantBuildDir.relativize(p).toString.replace('\\', '/'))
}
