package bleep

/** Minimal flags that must be parsed before bootstrap (before decline can run).
  *
  * Only `--directory`/`-d` (to find bleep.yaml) and `--dev` (to skip version switching) truly need early parsing. Everything else (--no-color, --debug, etc.)
  * is handled by decline after bootstrap.
  */
case class PreBootstrapOpts(
    directory: Option[String],
    dev: Boolean
)

object PreBootstrapOpts {
  def parse(args: List[String]): (PreBootstrapOpts, List[String]) = {
    var directory = Option.empty[String]
    val keepArgs = List.newBuilder[String]
    var dev = false
    var idx = 0
    while (idx < args.length) {
      args(idx) match {
        case "--dev"                                           => dev = true
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

    (PreBootstrapOpts(directory, dev), keepArgs.result())
  }
}
