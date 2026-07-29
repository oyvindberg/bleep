package bleep.analysis

import java.io.IOException
import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.{DirectoryNotEmptyException, FileVisitResult, Files, Path, SimpleFileVisitor}

/** Recursively delete a file or directory tree. Top-level, so every test in `bleep.analysis` gets it without an import.
  *
  * Replaces 29 copy-pasted helpers that all did:
  * {{{
  * Files.list(path).toScala(List).foreach(deleteRecursively)
  * }}}
  * `Files.list` returns a stream backed by an open directory handle, and materialising it with `.toScala(List)` never closes it — the handle then lives until
  * GC gets around to it. `walkFileTree` is the JDK's own recursive-delete recipe, and it closes each directory stream before `postVisitDirectory` runs.
  *
  * ==Why it retries==
  *
  * The tree may still be growing while we delete it. `Outcome.runInFreshThread` implements cancellation as `cancel()` + `Thread.interrupt()` and returns
  * `ThreadOutcome.Cancelled` *without waiting for the worker to stop* — it parks the thread in `runawayThreads` instead, on the grounds that "most native
  * compilers ignore interrupt and keep running". So a cancellation test that asserts `Cancelled` and then deletes its output directory is racing a kotlinc that
  * is still emitting into it: the walk empties the directory, the writer refills it, and unlinking the directory then fails with `DirectoryNotEmptyException`.
  * Seen on both windows-latest and ubuntu, which is what made these tests look flaky rather than broken.
  *
  * Retrying the whole walk lets the runaway writer finish and the delete converge. It is bounded — if the tree will not settle, the exception is rethrown and
  * the test fails rather than leaving cleanup silently half-done.
  */
def deleteRecursively(path: Path): Unit = {
  var remaining = 10
  var deleted = false
  while (!deleted)
    try {
      deleteTreeOnce(path)
      deleted = true
    } catch {
      case _: DirectoryNotEmptyException if remaining > 0 =>
        remaining -= 1
        Thread.sleep(100L)
    }
}

private def deleteTreeOnce(path: Path): Unit =
  if (Files.exists(path))
    Files.walkFileTree(
      path,
      new SimpleFileVisitor[Path] {
        override def visitFile(file: Path, attrs: BasicFileAttributes): FileVisitResult = {
          // deleteIfExists rather than delete: a runaway writer may have removed or replaced the entry since the walk listed it.
          Files.deleteIfExists(file): Unit
          FileVisitResult.CONTINUE
        }

        override def postVisitDirectory(dir: Path, exc: IOException): FileVisitResult = {
          // Propagate a failure from iterating the directory rather than deleting on top of it.
          if (exc != null) throw exc
          Files.deleteIfExists(dir): Unit
          FileVisitResult.CONTINUE
        }
      }
    ): Unit
