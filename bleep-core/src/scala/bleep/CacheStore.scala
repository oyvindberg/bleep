package bleep

import ryddig.Logger

import java.nio.file.{Files, Path, StandardCopyOption}

/** Storage backend for the build cache: content-addressed blobs under string keys.
  *
  * Implementations: [[S3Client]] (S3-compatible HTTP services) and [[LocalDirStore]] (a directory on the local filesystem, for sharing compiled state between
  * checkouts/worktrees on one machine).
  */
trait CacheStore {

  /** Check if an object exists. */
  def headObject(key: String): Boolean

  /** Download an object. Throws if missing. */
  def getObject(key: String): Array[Byte]

  /** Upload an object. Throws on failure. */
  def putObject(key: String, content: Array[Byte]): Unit
}

/** Cache backend backed by a local directory. Keys map directly to file paths under `root`.
  *
  * Writes are atomic: content goes to a temp file in the target directory, then an atomic move to the final name. Concurrent pushes of the same key (e.g. two
  * worktrees compiling the same digest) both succeed; content is identical because keys are content digests.
  */
class LocalDirStore(logger: Logger, root: Path) extends CacheStore {

  private def pathFor(key: String): Path = {
    val resolved = root.resolve(key).normalize()
    if (!resolved.startsWith(root)) throw new BleepException.Text(s"Cache key '$key' escapes cache root $root")
    resolved
  }

  override def headObject(key: String): Boolean =
    Files.isRegularFile(pathFor(key))

  override def getObject(key: String): Array[Byte] = {
    val path = pathFor(key)
    if (!Files.isRegularFile(path)) throw new BleepException.Text(s"Cache object not found: $path")
    Files.readAllBytes(path)
  }

  override def putObject(key: String, content: Array[Byte]): Unit = {
    val path = pathFor(key)
    Files.createDirectories(path.getParent)
    val temp = Files.createTempFile(path.getParent, s".${path.getFileName.toString}", ".tmp")
    try {
      Files.write(temp, content)
      Files.move(temp, path, StandardCopyOption.ATOMIC_MOVE, StandardCopyOption.REPLACE_EXISTING)
      logger.debug(s"cache PUT $path (${content.length} bytes)")
    } finally Files.deleteIfExists(temp): Unit
  }
}

object LocalDirStore {

  /** Interpret a `file://` cache URI as an absolute directory. */
  def fromUri(logger: Logger, uri: java.net.URI): LocalDirStore = {
    val root =
      try Path.of(uri).toAbsolutePath.normalize()
      catch {
        case e: IllegalArgumentException =>
          throw new BleepException.Text(s"Invalid file cache uri '$uri': ${e.getMessage}. Expected an absolute path like file:///path/to/cache")
      }
    new LocalDirStore(logger, root)
  }
}
