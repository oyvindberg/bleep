package bleep.analysis

import java.io.IOException
import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.{FileVisitResult, Files, Path, SimpleFileVisitor}

/** Recursively delete a file or directory tree. Top-level, so every test in `bleep.analysis` gets it without an import.
  *
  * Implemented with `Files.walkFileTree` rather than `Files.list`. `Files.list` returns a stream backed by an open directory handle, and materialising it with
  * `.toScala(List)` — as 29 copies of this helper used to do — never closes it; the handle then lives until GC gets around to it. On POSIX that is merely
  * untidy. On Windows a directory entry with a live handle is not removed when you delete it, only marked "delete pending", so deleting the parent afterwards
  * fails with `DirectoryNotEmptyException`.
  *
  * That is how it surfaced: `PlatformCancellationTest` passed its assertion and then threw out of the `finally` block, failing an otherwise green run on
  * windows-latest. Any of the 29 could do the same depending on GC timing, which is what made Windows look flaky rather than broken.
  *
  * `walkFileTree` closes each directory stream before `postVisitDirectory` runs, so nothing is held open by the time we delete.
  */
def deleteRecursively(path: Path): Unit =
  if (Files.exists(path))
    Files.walkFileTree(
      path,
      new SimpleFileVisitor[Path] {
        override def visitFile(file: Path, attrs: BasicFileAttributes): FileVisitResult = {
          Files.delete(file)
          FileVisitResult.CONTINUE
        }

        override def postVisitDirectory(dir: Path, exc: IOException): FileVisitResult = {
          // Propagate a failure from iterating the directory rather than deleting on top of it.
          if (exc != null) throw exc
          Files.delete(dir)
          FileVisitResult.CONTINUE
        }
      }
    ): Unit
