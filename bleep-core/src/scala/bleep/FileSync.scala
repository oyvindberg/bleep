package bleep

import bleep.internal.FileUtils
import ryddig.Logger
import sourcecode.{Enclosing, File, Line}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.util

object FileSync {
  sealed trait Synced

  object Synced {
    case object New extends Synced

    case object Changed extends Synced

    case object Unchanged extends Synced

    case object Deleted extends Synced

    implicit class SyncedOps(private val synced: Map[Path, Synced]) extends AnyVal {
      def log(logger: Logger, msg: String)(implicit l: Line, f: File, e: Enclosing): Unit = {
        synced.foreach { case (path, synced) =>
          logger.withContext("change", synced.toString).debug(path)
        }

        val summary = synced.toArray
          .groupBy { case (_, synced) => synced }
          .map { case (synced, files) => s"$synced: ${files.length}" }
          .mkString(", ")

        logger.withContext("summary", summary).info(msg)
      }
    }
  }

  sealed trait DeleteUnknowns

  object DeleteUnknowns {
    case object No extends DeleteUnknowns

    case class Yes(maxDepth: Option[Int]) extends DeleteUnknowns
  }

  def syncPaths(folder: Path, fileMap: Map[Path, String], deleteUnknowns: DeleteUnknowns, soft: Boolean): Map[Path, Synced] = {
    val fileRelMap = fileMap.map { case (path, content) =>
      require(path.startsWith(folder), s"$path not within $folder")
      RelPath.relativeTo(folder, path) -> content
    }
    syncStrings(folder, fileRelMap, deleteUnknowns, soft)
  }

  /** @param soft
    *   compare to existing content in order to not change timestamps. tooling may care a lot about this
    */
  def syncStrings(folder: Path, fileRelMap: Map[RelPath, String], deleteUnknowns: DeleteUnknowns, soft: Boolean): Map[Path, Synced] =
    syncBytes(folder, fileRelMap.map { case (k, v) => (k, v.getBytes(StandardCharsets.UTF_8)) }, deleteUnknowns, soft)

  /** @param soft
    *   compare to existing content in order to not change timestamps. tooling may care a lot about this
    */
  def syncBytes(folder: Path, fileRelMap: Map[RelPath, Array[Byte]], deleteUnknowns: DeleteUnknowns, soft: Boolean): Map[Path, Synced] = {
    val ret = scala.collection.mutable.Map.empty[Path, Synced]
    val fileMap = fileRelMap.map { case (relPath, content) => (folder / relPath, content) }

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

    fileMap.foreach { case (file, bytes) =>
      val synced =
        if (soft) softWriteBytes(file, bytes)
        else {
          FileUtils.writeBytes(file, bytes)
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
        FileUtils.writeBytes(path, newContent)
        Synced.Changed
      }
    } else {
      FileUtils.writeBytes(path, newContent)
      Synced.New
    }
}
