package bleep
package internal

import bleep.logging.Logger
import bloop.config.Config

import java.nio.file.Path
import scala.jdk.CollectionConverters.IteratorHasAsScala

object findOriginalTargetDir {
  // the bloop sbt plugin overwrites output directories, but it may be referenced in scalacOptions, and we'll need to recognize it
  // we could compute this ourselves as well, but this was easy to write
  def apply(logger: Logger, bloopProject: Config.Project): Option[Path] = {

    val ret = bloopProject.sources.firstDefined { p =>
      // holy shit what an API `Path` has. Lets revisit this someday
      val parts = p.iterator().asScala.toList.map(_.toString)

      parts.indexOf("src_managed") match {
        case -1 => None
        case n  => Some(parts.take(n).foldLeft(p.getRoot)(_ / _))
      }
    }
    if (ret.isEmpty) logger.warn(s"Couldn't determine original output directory of project ${bloopProject.name}")
    ret
  }
}
