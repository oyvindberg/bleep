package bleep

case class CommonOpts(
    noColor: Boolean,
    debug: Boolean,
    directory: Option[String],
    dev: Boolean,
    noBspProgress: Boolean
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
    var idx = 0
    while (idx < args.length) {
      args(idx) match {
        case "--no-color"        => noColor = true
        case "--debug"           => debug = true
        case "--dev"             => dev = true
        case "--no-bsp-progress" => noBspProgress = true
        case "-d" | "--directory" if args.isDefinedAt(idx + 1) =>
          directory = Some(args(idx + 1))
          idx += 1
        case "--" =>
          keepArgs ++= args.drop(idx)
          idx = Int.MaxValue - 1
        case other => keepArgs += other
      }
      idx += 1
    }

    (CommonOpts(noColor, debug, directory, dev, noBspProgress), keepArgs.result())
  }
}
