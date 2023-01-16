package bleep
package internal

import bleep.BleepException
import bleep.logging.Logger

object fatal {
  def apply(context: String, logger: Logger, throwable: Throwable): ExitCode.Failure.type = {
    log(context, logger, throwable)
    ExitCode.Failure
  }

  def apply(context: String, logger: Logger): ExitCode.Failure.type = {
    logger.error(context)
    ExitCode.Failure
  }

  def log(context: String, logger: Logger, throwable: Throwable): Unit =
    throwable match {
      case buildException: BleepException =>
        logger.debug(context, buildException)
        logger.error(throwableMessages(buildException).mkString(": "))
      case unexpected =>
        logger.error(context, unexpected)
    }
}
