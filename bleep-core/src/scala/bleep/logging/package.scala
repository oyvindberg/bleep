package bleep

import fansi.Str

import java.io.{PrintWriter, StringWriter}

package object logging {
  type Ctx = Map[String, Str]
  type Logger = TypedLogger[Unit]
  val Logger = TypedLogger
  type LoggerResource = TypedLoggerResource[Unit]
  val LoggerResource = TypedLoggerResource

  private[bleep] def formatThrowable(th: Throwable): String = {
    val sw = new StringWriter()
    val pw = new PrintWriter(sw)
    th.printStackTrace(pw)
    sw.toString
  }
}
