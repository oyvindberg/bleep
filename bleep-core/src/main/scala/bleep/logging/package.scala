package bleep

import fansi.Str

import java.io.{PrintStream, StringWriter}

package object logging {
  type Ctx = Map[Str, Str]

  private[logging] val emptyContext: Ctx = Map.empty

  def stdout(pattern: Pattern, ctx: Ctx = emptyContext): Logger.Aux[PrintStream] =
    new Logger.ConsoleLogger(System.out, pattern, ctx)

  def appendable[A <: Appendable](
      appendable: A,
      pattern: Pattern,
      ctx: Ctx = emptyContext
  ): Logger.Aux[A] =
    new Logger.AppendableLogger(appendable, pattern, ctx)

  def stringWriter(pattern: Pattern, ctx: Ctx = emptyContext): Logger.Aux[StringWriter] =
    appendable(new StringWriter, pattern, ctx)

  def storing(ctx: Ctx = emptyContext): Logger.Aux[Array[Logger.Stored]] =
    new Logger.StoringLogger(new Logger.Store, ctx)
}
