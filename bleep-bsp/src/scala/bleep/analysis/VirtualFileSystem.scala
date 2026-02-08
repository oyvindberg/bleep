package bleep.analysis

import java.nio.file.Path
import scala.collection.mutable

/** A virtual in-memory file system for testing.
  *
  * Allows simulating file operations without touching the real filesystem.
  */
class VirtualFileSystem {
  private val files: mutable.Map[Path, String] = mutable.Map.empty
  private val directories: mutable.Set[Path] = mutable.Set.empty

  /** Write content to a file */
  def writeFile(path: Path, content: String): Unit = {
    val normalized = path.normalize()
    // Ensure parent directories exist
    Option(normalized.getParent).foreach(mkdirs)
    files(normalized) = content
  }

  /** Read content from a file */
  def readFile(path: Path): Option[String] =
    files.get(path.normalize())

  /** Check if a file exists */
  def exists(path: Path): Boolean = {
    val normalized = path.normalize()
    files.contains(normalized) || directories.contains(normalized)
  }

  /** Delete a file */
  def deleteFile(path: Path): Boolean =
    files.remove(path.normalize()).isDefined

  /** Create directories */
  def mkdirs(path: Path): Unit = {
    var current = path.normalize()
    while current != null do {
      directories += current
      current = current.getParent
    }
  }

  /** List all files in a directory (non-recursive) */
  def listFiles(dir: Path): List[Path] = {
    val normalized = dir.normalize()
    files.keys.filter { path =>
      Option(path.getParent).exists(_.normalize() == normalized)
    }.toList
  }

  /** List all files recursively */
  def listFilesRecursive(dir: Path): List[Path] = {
    val normalized = dir.normalize()
    files.keys.filter { path =>
      path.normalize().startsWith(normalized)
    }.toList
  }

  /** Get all files with their content */
  def allFiles: Map[Path, String] = files.toMap

  /** Clear all files */
  def clear(): Unit = {
    files.clear()
    directories.clear()
  }

  /** Clone this filesystem */
  def snapshot(): VirtualFileSystem = {
    val copy = new VirtualFileSystem
    copy.files ++= this.files
    copy.directories ++= this.directories
    copy
  }
}

object VirtualFileSystem {
  def apply(): VirtualFileSystem = new VirtualFileSystem

  /** Create a VFS with initial files */
  def apply(files: (Path, String)*): VirtualFileSystem = {
    val vfs = new VirtualFileSystem
    files.foreach { case (path, content) => vfs.writeFile(path, content) }
    vfs
  }

  /** Create a VFS with initial files from string paths */
  def fromStrings(files: (String, String)*): VirtualFileSystem =
    apply(files.map { case (p, c) => Path.of(p) -> c }*)
}
