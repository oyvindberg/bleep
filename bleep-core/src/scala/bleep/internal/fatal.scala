package bleep
package internal

import bleep.BleepException
import bleep.logging.Logger

object fatal {
  def apply(context: String, logger: Logger, throwable: Throwable): Nothing = {
    throwable match {
      case buildException: BleepException =>
        logger.debug(context, buildException)
        logger.error(throwableMessages(buildException).mkString(": "))
      case unexpected =>
        logger.error(context, unexpected)
    }
    sys.exit(1)
  }

}
