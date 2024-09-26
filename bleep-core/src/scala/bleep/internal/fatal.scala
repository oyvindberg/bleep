package bleep
package internal

import bleep.logging.{Logger, Throwables}

object fatal {
  def apply(context: String, logger: Logger, throwable: Throwable): ExitCode.Failure.type = {
    logException(context, logger, throwable)
    ExitCode.Failure
  }

  def apply(context: String, logger: Logger): ExitCode.Failure.type = {
    logger.error(context)
    ExitCode.Failure
  }
}
