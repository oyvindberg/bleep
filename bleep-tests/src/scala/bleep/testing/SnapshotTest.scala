package bleep
package testing

import coursier.paths.CoursierPaths
import org.scalactic.TripleEqualsSupport
import org.scalatest.Assertion
import org.scalatest.funsuite.AnyFunSuite
import ryddig.{LogLevel, LogPatterns, Logger, Loggers}

import java.nio.channels.{FileChannel, FileLock}
import java.nio.file.{Files, Path, Paths, StandardOpenOption}
import java.time.Instant
import scala.util.Properties

trait SnapshotTest extends AnyFunSuite with TripleEqualsSupport {
  val logger0 = Loggers.stdout(LogPatterns.interface(Some(Instant.now), noColor = false), disableProgress = true).acquire().value.withMinLogLevel(LogLevel.info)

  val isCi: Boolean =
    sys.env.contains("BUILD_NUMBER") || sys.env.contains("CI") // from sbt

  val outFolder: Path =
    Paths.get("snapshot-tests").toAbsolutePath

  val absolutePaths: model.Replacements =
    model.Replacements.ofReplacements(
      List(
        (CoursierPaths.cacheDirectory().toString, "<COURSIER>"),
        (CoursierPaths.archiveCacheDirectory().toString, "<COURSIER_ARC>"),
        (System.getProperty("user.dir"), "<BLEEP_GIT>"),
        (System.getProperty("user.home"), "<HOME>"),
        (System.getProperty("java.home"), "<JAVA_HOME>"),
        ("darwin", "<MASKED_OS>"),
        ("linux", "<MASKED_OS>"),
        ("windows", "<MASKED_OS>"),
        ("x64", "<MASKED_ARCHITECTURE>"),
        ("amd64", "<MASKED_ARCHITECTURE>"),
        ("arm64", "<MASKED_ARCHITECTURE>")
      )
    )

  def writeAndCompare(in: Path, fileMap: Map[Path, String], logger: Logger): Assertion = {
    FileSync.syncPaths(in, fileMap, deleteUnknowns = FileSync.DeleteUnknowns.Yes(maxDepth = None), soft = true).discard()

    var from = in
    while (!Files.isDirectory(from))
      from = from.getParent

    // plain `git`, not `/usr/bin/env git` — ProcessBuilder resolves it via PATH on every platform, whereas /usr/bin/env does not exist on Windows and made
    // every snapshot test fail there with `CreateProcess error=2`.
    gitWithRetry("git add", from, List("git", "add", in.toString), logger)

    if (Properties.isWin) succeed // let's deal with this later
    else if (isCi) {
      gitWithRetry("git diff", from, List("git", "diff", "--exit-code", "HEAD", in.toString), logger)
      succeed
    } else succeed
  }

  /** Run a git subprocess under [[GitLock]], retrying when git fails with the transient `.git/index.lock` collision. The cross-process FileLock serializes
    * concurrent SnapshotTest calls cleanly, but operations outside it — `git status --porcelain` from `ProjectDigest`, an editor / shell / external tool the
    * developer happens to be running — still create their own `.git/index.lock` briefly, and our git invocation can't recover from that. We can't lock against
    * those, so retry on the transient error with exponential backoff. Any other git failure (real diff mismatch, missing path, …) propagates immediately.
    */
  private def gitWithRetry(action: String, from: Path, cmd: List[String], logger: Logger): Unit = {
    val maxAttempts = 10
    val baseSleepMs = 100L
    var attempt = 1
    var lastError: Throwable = null
    while (attempt <= maxAttempts)
      try {
        GitLock.withLock {
          cli(action, from, cmd, logger, out = cli.Out.ViaLogger(logger), env = List(("PATH", sys.env("PATH")))).discard()
        }
        return
      } catch {
        case e: BleepException.Text if e.getMessage.contains("index.lock") =>
          lastError = e
          if (attempt < maxAttempts) {
            Thread.sleep(baseSleepMs * attempt)
            attempt += 1
          } else throw e
      }
    if (lastError != null) throw lastError
  }
}

/** Cross-process file lock for git operations.
  *
  * Tests may run in separate forked JVMs, so JVM-level synchronization is not sufficient. Uses java.nio.channels.FileLock on a dedicated lock file to serialize
  * git index access across all test processes.
  *
  * The lock lives in the per-worktree git dir, not the shared common dir, so concurrent test runs in different worktrees of the same repo don't block each
  * other — each worktree has its own `index.lock`, so they don't actually conflict.
  */
private object GitLock {
  private val lockFile: Path = {
    val cwd = Paths.get(System.getProperty("user.dir"))
    val dotGit = cwd.resolve(".git")
    val gitDir: Path =
      if (Files.isDirectory(dotGit)) dotGit
      else if (Files.isRegularFile(dotGit)) {
        // Worktree: .git is a file containing `gitdir: <path-to-per-worktree-gitdir>`.
        val line = Files.readAllLines(dotGit).get(0)
        require(line.startsWith("gitdir:"), s"Unexpected .git file contents: $line")
        val raw = line.substring("gitdir:".length).trim
        val p = Paths.get(raw)
        if (p.isAbsolute) p else cwd.resolve(p).normalize()
      } else sys.error(s"Not a git repo or worktree: $cwd")
    gitDir.resolve("bleep-test.lock")
  }

  def withLock[A](body: => A): A = synchronized {
    val channel = FileChannel.open(lockFile, StandardOpenOption.CREATE, StandardOpenOption.WRITE)
    var lock: FileLock = null
    try {
      lock = channel.lock()
      body
    } finally {
      if (lock != null) lock.release()
      channel.close()
    }
  }
}
