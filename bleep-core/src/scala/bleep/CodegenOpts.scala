package bleep

import bleep.model.CrossProjectName

case class CodegenOpts(
    crossName: CrossProjectName
)

object CodegenOpts {
  // we apparently need to parse (and remove from input) before the rest of the app is launched
  def parse(args: List[String]): (CodegenOpts, List[String]) = {
    var project: String = null
    val keepArgs = List.newBuilder[String]
    var idx = 0
    while (idx < args.length) {
      args(idx) match {
        case "-p" | "--project" if args.isDefinedAt(idx + 1) =>
          project = args(idx + 1)
          idx += 1
        case "--" =>
          keepArgs ++= args.drop(idx)
          idx = Int.MaxValue - 1
        case other => keepArgs += other
      }
      idx += 1
    }

    if (project == null) throw new IllegalAccessException("A project name should have been passed")
    else {
      CrossProjectName.fromString(project) match {
        case None => throw new IllegalArgumentException(s"Illegal crossName : $project")
        case Some(crossName) =>
          (CodegenOpts(crossName), keepArgs.result())
      }
    }
  }
}
