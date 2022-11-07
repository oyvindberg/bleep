package bleep.plugin.nosbt.io

import bleep.plugin.nosbt.Using.{fileInputStream, OpenFile}

import java.io.{BufferedOutputStream, File, FileOutputStream, IOException}
import java.util.Properties
import scala.util.control.Exception.catching

object IO {

  /** Creates directory `dir` and all parent directories.  It tries to work around a race condition in `File.mkdirs()` by retrying up to a limit. */
  def createDirectory(dir: File): Unit = {
    def failBase = "Could not create directory " + dir
    // Need a retry because mkdirs() has a race condition
    var tryCount = 0
    while (!dir.exists && !dir.mkdirs() && tryCount < 100) tryCount += 1
    if (dir.isDirectory)
      ()
    else if (dir.exists) {
      sys.error(failBase + ": file exists and is not a directory.")
    } else
      sys.error(failBase)
  }

  def translate[T](msg: => String)(f: => T) =
    try f
    catch {
      case e: IOException => throw new TranslatedIOException(msg + e.toString, e)
      case e: Exception   => throw new TranslatedException(msg + e.toString, e)
    }

  def delete(files: Iterable[File]): Unit = files.foreach(delete)

  /** Deletes `file`, recursively if it is a directory. */
  def delete(file: File): Unit =
    translate("Error deleting file " + file + ": ") {
      val deleted = file.delete()
      if (!deleted && file.isDirectory) {
        delete(listFiles(file))
        file.delete
        ()
      }
    }

  def listFiles(filter: java.io.FileFilter)(dir: File): Array[File] =
    wrapNull(dir.listFiles(filter))

  /** Returns the children of directory `dir` that match `filter` in a non-null array. */
  def listFiles(dir: File, filter: java.io.FileFilter): Array[File] =
    wrapNull(dir.listFiles(filter))

  /** Returns the children of directory `dir` in a non-null array. */
  def listFiles(dir: File): Array[File] = wrapNull(dir.listFiles())

  private[nosbt] def wrapNull(a: Array[File]) = if (a == null) new Array[File](0) else a

  /** Returns the path for `file` relative to directory `base` or None if `base` is not a parent of `file`. If `file` or `base` are not absolute, they are first
    * resolved against the current working directory.
    */
  def relativize(base: File, file: File): Option[String] = {
    val basePath = (if (base.isAbsolute) base else base.getCanonicalFile).toPath
    val filePath = (if (file.isAbsolute) file else file.getCanonicalFile).toPath
    if ((filePath startsWith basePath) || (filePath.normalize() startsWith basePath.normalize())) {
      val relativePath = catching(classOf[IllegalArgumentException]) opt (basePath relativize filePath)
      relativePath map (_.toString)
    } else None
  }
  private def closeCloseable[T <: AutoCloseable]: T => Unit = _.close()

  def file[T <: AutoCloseable](openF: File => T): OpenFile[T] = file(openF, closeCloseable)

  def file[T](openF: File => T, closeF: T => Unit): OpenFile[T] =
    new OpenFile[T] {
      def openImpl(file: File) = openF(file)
      def close(t: T) = closeF(t)
    }

  def fileOutputStream(append: Boolean = false): OpenFile[BufferedOutputStream] =
    file(f => new BufferedOutputStream(new FileOutputStream(f, append)))

  def write(properties: Properties, label: String, to: File) =
    fileOutputStream()(to)(output => properties.store(output, label))

  /** Reads the properties in `from` into `properties`.  If `from` does not exist, `properties` is left unchanged. */
  def load(properties: Properties, from: File): Unit =
    if (from.exists)
      fileInputStream(from)(input => properties.load(input))
}

private[nosbt] sealed class TranslatedException private[nosbt] (msg: String, cause: Throwable) extends RuntimeException(msg, cause) {
  override def toString = msg
}

private[nosbt] final class TranslatedIOException private[nosbt] (msg: String, cause: IOException) extends TranslatedException(msg, cause)
