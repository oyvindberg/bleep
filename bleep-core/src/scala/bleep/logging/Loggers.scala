package bleep.logging

import java.io.{BufferedWriter, Flushable, PrintStream, StringWriter}
import java.nio.file.{Files, Path, StandardOpenOption}

object Loggers {
  private[logging] val emptyContext: Ctx = Map.empty

  // this is a resource since we absolutely should flush it before we exit
  def stdout(pattern: Pattern, disableProgress: Boolean, ctx: Ctx = emptyContext): TypedLoggerResource[PrintStream] =
    TypedLoggerResource.flushable {
      new TypedLogger.ConsoleLogger(System.out, pattern, ctx, Nil, disableProgress)
    }

  // this is unbuffered, so I don't think there is any reason to care further
  def stderr(pattern: Pattern, ctx: Ctx = emptyContext): TypedLogger[PrintStream] =
    new TypedLogger.ConsoleLogger(System.err, pattern, ctx, Nil, disableProgress = true)

  // this is a resource since we absolutely should flush it before we exit
  def path(logFile: Path, pattern: Pattern, ctx: Ctx = emptyContext): TypedLoggerResource[BufferedWriter] =
    TypedLoggerResource.autoCloseable {
      Files.createDirectories(logFile.getParent)
      val writer = Files.newBufferedWriter(logFile, StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING)
      appendable(writer, pattern, ctx)
    }

  // wrap in TypedLoggerResource if you need it flushed/closed
  def appendable[A <: Appendable](appendable: A, pattern: Pattern, ctx: Ctx = emptyContext): TypedLogger[A] =
    new TypedLogger.AppendableLogger(appendable, pattern, ctx, Nil)

  def stringWriter(pattern: Pattern, ctx: Ctx = emptyContext): TypedLogger[StringWriter] =
    appendable(new StringWriter, pattern, ctx)

  def storing(ctx: Ctx = emptyContext): TypedLogger[Array[TypedLogger.Stored]] =
    new TypedLogger.StoringLogger(new TypedLogger.Store, ctx, Nil)

  def printJsonStream[U <: Flushable with Appendable](to: U, ctx: Ctx = emptyContext): TypedLogger[U] =
    new jsonEvents.SerializeLogEvents[U](to, ctx, Nil)

  def decodeJsonStream[U](next: TypedLogger[U]): TypedLogger[U] =
    jsonEvents.DeserializeLogEvents(next)
}
