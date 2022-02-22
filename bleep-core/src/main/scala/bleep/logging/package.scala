package bleep

import fansi.Str

import java.io.{BufferedWriter, PrintStream, StringWriter}
import java.nio.file.{Files, Path, StandardOpenOption}

package object logging {
  type Ctx = Map[Str, Str]
  type Logger = TypedLogger[Unit]
  val Logger = TypedLogger
  type LoggerResource = TypedLoggerResource[Unit]
  val LoggerResource = TypedLoggerResource

  private[logging] val emptyContext: Ctx = Map.empty

  def stdout(pattern: Pattern, ctx: Ctx = emptyContext): TypedLogger[PrintStream] =
    new TypedLogger.ConsoleLogger(System.out, pattern, ctx)

  def stderr(pattern: Pattern, ctx: Ctx = emptyContext): TypedLogger[PrintStream] =
    new TypedLogger.ConsoleLogger(System.err, pattern, ctx)

  def path(logFile: Path, pattern: Pattern, ctx: Ctx = emptyContext): TypedLoggerResource[BufferedWriter] =
    new TypedLoggerResource[BufferedWriter] {
      override def use[T](f: TypedLogger[BufferedWriter] => T): T = {
        Files.createDirectories(logFile.getParent)
        val writer: BufferedWriter = Files.newBufferedWriter(logFile, StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING)
        try f(appendable(writer, pattern, ctx))
        finally writer.close()
      }
    }

  def appendable[A <: Appendable](
      appendable: A,
      pattern: Pattern,
      ctx: Ctx = emptyContext
  ): TypedLogger[A] =
    new TypedLogger.AppendableLogger(appendable, pattern, ctx)

  def stringWriter(pattern: Pattern, ctx: Ctx = emptyContext): TypedLogger[StringWriter] =
    appendable(new StringWriter, pattern, ctx)

  def storing(ctx: Ctx = emptyContext): TypedLogger[Array[TypedLogger.Stored]] =
    new TypedLogger.StoringLogger(new TypedLogger.Store, ctx)
}
