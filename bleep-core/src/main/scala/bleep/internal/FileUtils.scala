package bleep
package internal

import java.io.IOException
import java.nio.charset.StandardCharsets
import java.nio.file._
import java.nio.file.attribute.BasicFileAttributes
import java.util

object FileUtils {
  sealed trait Synced
  object Synced {
    case object New extends Synced
    case object Changed extends Synced
    case object Unchanged extends Synced
    case object Deleted extends Synced
  }

  sealed trait DeleteUnknowns
  object DeleteUnknowns {
    case object No extends DeleteUnknowns
    case class Yes(maxDepth: Option[Int]) extends DeleteUnknowns
  }

  // Files.exists is too slow because it throws exceptions behind the scenes
  def exists(path: Path): Boolean = path.toFile.exists()

  def syncPaths(folder: Path, fileMap: Map[Path, String], deleteUnknowns: DeleteUnknowns, soft: Boolean): Map[Path, Synced] = {
    val fileRelMap = fileMap.map { case (path, content) =>
      require(path.startsWith(folder), s"${path} not within $folder")
      RelPath.relativeTo(folder, path) -> content
    }
    sync(folder, fileRelMap, deleteUnknowns, soft)
  }

  /** @param soft
    *   compare to existing content in order to not change timestamps. tooling may care a lot about this
    */
  def sync(folder: Path, fileRelMap: Map[RelPath, String], deleteUnknowns: DeleteUnknowns, soft: Boolean): Map[Path, Synced] = {
    val ret = scala.collection.mutable.Map.empty[Path, Synced]
    val fileMap = fileRelMap.map { case (relPath, string) => (folder / relPath, string) }

    deleteUnknowns match {
      case DeleteUnknowns.Yes(maybeMaxDepth) if FileUtils.exists(folder) =>
        val stream = maybeMaxDepth match {
          case Some(maxDepth) => Files.walk(folder, maxDepth)
          case None           => Files.walk(folder)
        }

        stream.forEach {
          case p if Files.isRegularFile(p) && !fileMap.contains(p) =>
            Files.delete(p)
            ret(p) = Synced.Deleted
          case _ => ()
        }

      case _ => ()
    }

    fileMap.foreach { case (file, content) =>
      val bytes = content.getBytes(StandardCharsets.UTF_8)
      val synced =
        if (soft) softWriteBytes(file, bytes)
        else {
          writeBytes(file, bytes)
          Synced.New
        }
      ret(file) = synced
    }

    ret.toMap
  }

  def softWriteBytes(path: Path, newContent: Array[Byte]): Synced =
    if (FileUtils.exists(path)) {
      val existingContent = Files.readAllBytes(path)
      if (util.Arrays.equals(existingContent, newContent)) Synced.Unchanged
      else {
        writeBytes(path, newContent)
        Synced.Changed
      }
    } else {
      writeBytes(path, newContent)
      Synced.New
    }

  def writeBytes(path: Path, newContent: Array[Byte]): Unit = {
    Files.createDirectories(path.getParent)
    Files.write(path, newContent, StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING)
    ()
  }

  def writeString(path: Path, newContent: String): Unit =
    writeBytes(path, newContent.getBytes(StandardCharsets.UTF_8))

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
}
