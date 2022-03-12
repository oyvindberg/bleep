package bleep
package internal

import bleep.internal
import bloop.config.Config

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters._

object findGeneratedFiles {
  def apply(bloopFilesByProjectName: Map[model.CrossProjectName, Config.File]): Map[model.CrossProjectName, Vector[GeneratedFile]] =
    bloopFilesByProjectName.flatMap { case (crossName, p) =>
      val originalTargetDir = findOriginalTargetDir.force(crossName, p.project)

      def findFiles(dirs: Iterable[Path], isResource: Boolean): Iterable[GeneratedFile] =
        for {
          dir <- dirs
          if dir.startsWith(originalTargetDir) && FileUtils.exists(dir)
          file <- Files.walk(dir).iterator().asScala.filter(Files.isRegularFile(_))
        } yield internal.GeneratedFile(isResource = isResource, Files.readString(file), RelPath.relativeTo(dir, file))

      val sources = findFiles(p.project.sources, isResource = false)
      val resources = findFiles(p.project.resources.getOrElse(Nil), isResource = true)
      val all = (sources ++ resources).toVector
      if (all.nonEmpty) Some((crossName, all)) else None
    }
}
