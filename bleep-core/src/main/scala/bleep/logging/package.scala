package bleep

import java.io.{StringWriter, Writer}

import bleep.logging.Logger.{AppendableLogger, Stored, StoringLogger, WriterLogger}
import fansi.Str

package object logging {
  type Ctx = Map[Str, Str]

  private[logging] val emptyContext: Ctx = Map.empty

  def stdout: Logger.Aux[Unit] =
    appendable(System.out).void

  def appendable[A <: Appendable](
      appendable: A,
      pattern: Pattern = Pattern.default,
      ctx: Ctx = emptyContext
  ): Logger.Aux[A] =
    new AppendableLogger(appendable, pattern, ctx)

  def writer[W <: Writer](
      writer: W = System.out,
      pattern: Pattern = Pattern.default,
      ctx: Ctx = emptyContext
  ): Logger.Aux[W] =
    new WriterLogger(new AppendableLogger(writer, pattern, ctx))

  def stringWriter(pattern: Pattern = Pattern.default, ctx: Ctx = emptyContext): Logger.Aux[StringWriter] =
    writer(new StringWriter, pattern, ctx)

  def storing(ctx: Ctx = emptyContext): Logger.Aux[Array[Stored]] =
    new StoringLogger(new Logger.Store, ctx)
}
