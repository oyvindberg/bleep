package bleep.logging

import java.io.{PrintWriter, StringWriter}

object Throwables {
  def tryExtract[T <: Throwable](clazz: Class[T])(th: Throwable): Option[T] =
    if (clazz.isInstance(th)) Some(th.asInstanceOf[T])
    else Option(th.getCause).flatMap(tryExtract(clazz)).orElse(th.getSuppressed.toList.flatMap(tryExtract(clazz)).headOption)

  def messagesFrom(th: Throwable): List[String] = {
    def rec(th: Throwable): List[String] = th.getMessage :: Option(th.getCause).toList.flatMap(rec)
    rec(th).distinct
  }

  def asString(th: Throwable): String = {
    val sw = new StringWriter()
    val pw = new PrintWriter(sw)
    th.printStackTrace(pw)
    sw.toString
  }
}
