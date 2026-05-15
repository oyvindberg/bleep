package bleep
package internal

import ryddig.Logger

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, IOException}
import java.nio.charset.StandardCharsets
import java.nio.file.*
import java.nio.file.attribute.BasicFileAttributes
import java.util.zip.{GZIPInputStream, GZIPOutputStream}
import scala.util.Properties

object FileUtils {
  lazy val TempDir = Path.of(System.getProperty("java.io.tmpdir"))
  lazy val Home = Path.of(System.getProperty("user.home"))

  lazy val cwd: Path = {
    val base = Paths.get(System.getProperty("user.dir"))
    if (Properties.isWin)
      base.toFile.getCanonicalFile.toPath // todo: why? copied from scala-cli
    else
      base
  }

  def writeBytes(path: Path, newContent: Array[Byte]): Unit = {
    // Skip write if file already has identical content — preserves timestamps
    // and avoids triggering unnecessary downstream recompilation
    if (path.toFile.exists()) {
      val existing = Files.readAllBytes(path)
      if (java.util.Arrays.equals(existing, newContent)) return
    }
    Files.createDirectories(path.getParent)
    Files.write(path, newContent, StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING)
    ()
  }

  /** Atomic counterpart to [[writeBytes]]: writes to a sibling temp file, then renames into place. A JVM crash or OS interruption mid-write leaves either the
    * prior content or the new content — never a half-written file. Use this for state files where a partial write would parse as garbage and silently force a
    * rebuild / cache wipe on the next read (e.g. incremental-compile manifests).
    *
    * Falls back to a plain `REPLACE_EXISTING` rename if the underlying filesystem doesn't support `ATOMIC_MOVE` (some FUSE mounts, a few Windows network
    * drives). Same "no-op if identical" short-circuit as `writeBytes`.
    */
  def writeBytesAtomic(path: Path, newContent: Array[Byte]): Unit = {
    if (path.toFile.exists()) {
      val existing = Files.readAllBytes(path)
      if (java.util.Arrays.equals(existing, newContent)) return
    }
    Files.createDirectories(path.getParent)
    val tmp = path.resolveSibling(path.getFileName.toString + ".tmp")
    Files.write(tmp, newContent, StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING)
    try Files.move(tmp, path, StandardCopyOption.ATOMIC_MOVE, StandardCopyOption.REPLACE_EXISTING)
    catch {
      case _: AtomicMoveNotSupportedException => Files.move(tmp, path, StandardCopyOption.REPLACE_EXISTING)
    }
    ()
  }

  def writeGzippedBytes(path: Path, newContent: Array[Byte]): Unit = {
    val bos = new ByteArrayOutputStream(1024)
    val gos = new GZIPOutputStream(bos)
    gos.write(newContent)
    gos.close()
    Files.createDirectories(path.getParent)
    Files.write(path, bos.toByteArray, StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING)
    ()
  }

  def readGzippedBytes(path: Path): Array[Byte] = {
    val is = new GZIPInputStream(new ByteArrayInputStream(Files.readAllBytes(path)), 1024)
    val ret = is.readAllBytes()
    is.close()
    ret
  }

  def writeString(logger: Logger, message: Option[String], path: Path, newContent: String): Unit = {
    writeBytes(path, newContent.getBytes(StandardCharsets.UTF_8))
    message match {
      case Some(message) => logger.withContext("path", path).info(message)
      case None          => logger.withContext("path", path).debug("wrote file")
    }
  }

  def deleteDirectory(dir: Path): Unit =
    if (FileUtils.exists(dir)) {
      Files.walkFileTree(
        dir,
        new SimpleFileVisitor[Path] {
          override def visitFile(file: Path, attrs: BasicFileAttributes): FileVisitResult = {
            Files.deleteIfExists(file)
            FileVisitResult.CONTINUE
          }

          override def postVisitDirectory(dir: Path, exc: IOException): FileVisitResult = {
            Files.deleteIfExists(dir)
            FileVisitResult.CONTINUE
          }
        }
      )
      ()
    }

  // Files.exists is too slow because it throws exceptions behind the scenes
  def exists(path: Path): Boolean = path.toFile.exists()
}
