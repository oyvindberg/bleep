package bleep

import bleep.internal.bleepLoggers.CallerProcessAcceptsJsonEvents
import bleep.internal.propsOrEnv

/** Logging configuration flags parsed by decline (not manually pre-parsed).
  *
  * These flags are available on every command and control how bleep logs. They are parsed AFTER bootstrap completes, allowing the logger to be configured based
  * on the full parsed command.
  */
case class LoggingOpts(
    noColor: Boolean,
    debug: Boolean,
    noBspProgress: Boolean,
    logAsJson: Boolean
)

object LoggingOpts {
  val defaultLogAsJson: Boolean = propsOrEnv(CallerProcessAcceptsJsonEvents).nonEmpty
}
