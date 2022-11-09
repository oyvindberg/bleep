package bleep
package sbtimport

import bleep.internal.IterableOps
import bloop.config.Config

import java.nio.file.Path
import scala.jdk.CollectionConverters.IteratorHasAsScala

// the bloop sbt plugin overwrites output directories, but it may be referenced in scalacOptions, and we'll need to recognize it
object findOriginalTargetDir {
  // we could compute this ourselves as well, but this was easy to write
  def apply(bloopProject: Config.Project): Option[Path] =
    bloopProject.sources.firstDefined { p =>
      val parts = p.iterator().asScala.toList.map(_.toString)

      parts.indexOf("src_managed") match {
        case -1 => None
        case n  => Some(parts.take(n).foldLeft(p.getRoot)(_ / _))
      }
    }

  def force(crossProjectName: model.CrossProjectName, bloopProject: Config.Project): Path =
    apply(bloopProject).getOrElse(throw new BleepException.TargetFolderNotDetermined(crossProjectName))
}
