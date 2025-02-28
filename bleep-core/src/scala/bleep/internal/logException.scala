package bleep.internal

import bleep.BleepException
import ryddig.{Logger, Throwables}
import sourcecode.{Enclosing, File, Line}

object logException {
  def apply(context: String, logger: Logger, throwable: Throwable)(implicit l: Line, f: File, e: Enclosing): Unit =
    throwable match {
      case buildException: BleepException =>
        logger.debug(context, buildException)
        logger.error(s"$context: ${Throwables.messagesFrom(buildException).mkString(": ")}")
      case unexpected =>
        logger.error(context, unexpected)
    }

}
