package bleep

import bleep.internal.bleepLoggers.CallerProcessAcceptsJsonEvents
import bleep.internal.propsOrEnv

/** Pre-bootstrap argument parser. Strips all known flags from the passthrough and records their values. The remaining args are passed to decline for subcommand
  * parsing. Logging flags are consumed here (via `toLoggingOpts`) and NOT forwarded to decline.
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
    // Honor the no-color.org standard: any non-empty `NO_COLOR` env var means "no ANSI please". `--no-color` on the CLI is a hard override that always wins.
    // Carrying this in PreBootstrapOpts means every place that uses LoggingOpts.noColor (the CLI logger, BSP-daemon-forked sourcegen scripts launched as their
    // own JVMs, the native-image script, …) gets the same answer whether the user typed `--no-color` or set the env or both.
    var noColor = sys.env.get("NO_COLOR").exists(_.nonEmpty)
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
        // These are stripped from passthrough (consumed here, not by decline)
        case "--no-color"        => noColor = true
        case "--debug"           => debug = true
        case "--no-bsp-progress" => noBspProgress = true
        case "--log-as-json"     => logAsJson = true
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
