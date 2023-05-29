package bleep.nosbt

import bleep.nosbt.io.IO

import java.io.{BufferedInputStream, File, FileInputStream}

abstract class Using[Source, T] {
  protected def open(src: Source): T
  def apply[R](src: Source)(f: T => R): R = {
    val resource = open(src)
    try f(resource)
    finally close(resource)
  }
  protected def close(out: T): Unit
}

object Using {
  private[nosbt] trait OpenFile[T] extends Using[File, T] {
    protected def openImpl(file: File): T
    protected final def open(file: File): T = {
      val parent = file.getParentFile
      if (parent != null)
        IO.createDirectory(parent)
      openImpl(file)
    }
  }
  private def closeCloseable[T <: AutoCloseable]: T => Unit = _.close()

  def file[T <: AutoCloseable](openF: File => T): OpenFile[T] = file(openF, closeCloseable)

  def file[T](openF: File => T, closeF: T => Unit): OpenFile[T] =
    new OpenFile[T] {
      def openImpl(file: File) = openF(file)
      def close(t: T) = closeF(t)
    }

  val fileInputStream: OpenFile[BufferedInputStream] =
    file(f => new BufferedInputStream(new FileInputStream(f)))
}
