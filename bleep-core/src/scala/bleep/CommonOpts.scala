package bleep

import bleep.internal.bleepLoggers.CallerProcessAcceptsJsonEvents
import bleep.internal.propsOrEnv

import java.nio.file.Path

case class CommonOpts(
    noColor: Boolean,
    debug: Boolean,
    directory: Option[String],
    dev: Boolean,
    noBspProgress: Boolean,
    logAsJson: Boolean,
    startedByNative: Option[Path]
)

object CommonOpts {
  // we apparently need to parse (and remove from input) before the rest of the app is launched
  def parse(args: List[String]): (CommonOpts, List[String]) = {
    var noColor = false
    var debug = false
    var directory = Option.empty[String]
    val keepArgs = List.newBuilder[String]
    var dev = false
    var noBspProgress = false
    var logAsJson = propsOrEnv(CallerProcessAcceptsJsonEvents).nonEmpty
    var startedByNative = propsOrEnv(BleepExecutable.BLEEP_STARTED_BY_NATIVE)
    var idx = 0
    while (idx < args.length) {
      args(idx) match {
        case "--no-color"                             => noColor = true
        case "--debug"                                => debug = true
        case "--dev"                                  => dev = true
        case "--no-bsp-progress"                      => noBspProgress = true
        case "--log-as-json"                          => logAsJson = true
        case x if x.startsWith("--started-by-native") => startedByNative = x.split("=").lastOption
        case "-d" | "--directory" if args.isDefinedAt(idx + 1) =>
          directory = Some(args(idx + 1))
          idx += 1
        case "--" =>
          keepArgs ++= args.drop(idx)
          idx = Int.MaxValue - 1
        case definedProps if definedProps.startsWith("-D") => ()
        case other                                         => keepArgs += other
      }
      idx += 1
    }

    (CommonOpts(noColor, debug, directory, dev, noBspProgress, logAsJson, startedByNative.map(Path.of(_))), keepArgs.result())
  }
}
