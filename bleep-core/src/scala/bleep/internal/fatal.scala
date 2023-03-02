package bleep
package internal

import bleep.logging.Logger

object fatal {
  def apply(context: String, logger: Logger, throwable: Throwable): ExitCode.Failure.type = {
    Throwables.log(context, logger, throwable)
    ExitCode.Failure
  }

  def apply(context: String, logger: Logger): ExitCode.Failure.type = {
    logger.error(context)
    ExitCode.Failure
  }
}
