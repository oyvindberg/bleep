package bleep

import bleep.internal.bleepLoggers.CallerProcessAcceptsJsonEvents
import bleep.internal.propsOrEnv

/** Pre-bootstrap argument parser. Extracts all flags from args but only strips `--directory`/`-d` and `--dev` from the passthrough (those must be consumed
  * before decline runs). Logging flags (--no-color, --debug, etc.) are recorded here AND left in the passthrough for decline to parse via LoggingOpts.
  */
case class PreBootstrapOpts(
    noColor: Boolean,
    debug: Boolean,
    directory: Option[String],
    dev: Boolean,
    noBspProgress: Boolean,
    logAsJson: Boolean
) {
  def toLoggingOpts: LoggingOpts =
    LoggingOpts(
      noColor = noColor,
      debug = debug,
      noBspProgress = noBspProgress,
      logAsJson = logAsJson
    )
}

object PreBootstrapOpts {
  def parse(args: List[String]): (PreBootstrapOpts, List[String]) = {
    var noColor = false
    var debug = false
    var directory = Option.empty[String]
    val keepArgs = List.newBuilder[String]
    var dev = false
    var noBspProgress = false
    var logAsJson = propsOrEnv(CallerProcessAcceptsJsonEvents).nonEmpty
    var idx = 0
    while (idx < args.length) {
      args(idx) match {
        // These are stripped from passthrough (consumed before decline)
        case "--dev"                                           => dev = true
        case "-d" | "--directory" if args.isDefinedAt(idx + 1) =>
          directory = Some(args(idx + 1))
          idx += 1
        // These are recorded but NOT stripped (decline also parses them via loggingOpts)
        case "--no-color"        => noColor = true; keepArgs += args(idx)
        case "--debug"           => debug = true; keepArgs += args(idx)
        case "--no-bsp-progress" => noBspProgress = true; keepArgs += args(idx)
        case "--log-as-json"     => logAsJson = true; keepArgs += args(idx)
        case "--"                =>
          keepArgs ++= args.drop(idx)
          idx = Int.MaxValue - 1
        case definedProps if definedProps.startsWith("-D") => ()
        case other                                         => keepArgs += other
      }
      idx += 1
    }

    (PreBootstrapOpts(noColor, debug, directory, dev, noBspProgress, logAsJson), keepArgs.result())
  }
}
