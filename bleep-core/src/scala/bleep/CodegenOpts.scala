package bleep

case class CodegenOpts(
    projectNames: List[model.CrossProjectName]
)

object CodegenOpts {
  // we apparently need to parse (and remove from input) before the rest of the app is launched
  def parse(args: List[String]): (CodegenOpts, List[String]) = {
    val projects = List.newBuilder[String]
    val keepArgs = List.newBuilder[String]
    var idx = 0
    while (idx < args.length) {
      args(idx) match {
        case "-p" | "--project" if args.isDefinedAt(idx + 1) =>
          projects += args(idx + 1)
          idx += 1
        case "--" =>
          keepArgs ++= args.drop(idx)
          idx = Int.MaxValue - 1
        case other => keepArgs += other
      }
      idx += 1
    }

    projects.result() match {
      case Nil => throw new IllegalAccessException("A project name should have been passed")
      case nonEmptyProjectStrings =>
        val projectNames = nonEmptyProjectStrings.map { p =>
          model.CrossProjectName.fromString(p).getOrElse {
            throw new IllegalArgumentException(s"Illegal crossName : $p")
          }
        }
        (CodegenOpts(projectNames), keepArgs.result())
    }
  }
}
