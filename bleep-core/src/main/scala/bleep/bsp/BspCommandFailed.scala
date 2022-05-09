package bleep.bsp

import bleep.{model, BuildException}
import ch.epfl.scala.bsp4j

class BspCommandFailed(what: String, projects: List[model.CrossProjectName], code: bsp4j.StatusCode)
    extends BuildException(
      code match {
        case bsp4j.StatusCode.OK        => sys.error("unexpected")
        case bsp4j.StatusCode.ERROR     => s"$what ${projects.map(_.value).mkString(", ")} failed"
        case bsp4j.StatusCode.CANCELLED => s"$what ${projects.map(_.value).mkString(", ")} was cancelled"
      }
    )
