package bleep
package internal

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters._

object findGeneratedFiles {
  def apply(inputProjects: ImportInputProjects): Map[model.CrossProjectName, Vector[GeneratedFile]] =
    inputProjects.values.flatMap { case (crossName, inputProject) =>
      val bloopProject = inputProject.bloopFile.project
      val originalTargetDir = findOriginalTargetDir.force(crossName, bloopProject)

      def findFiles(dirs: Iterable[Path], isResource: Boolean): Iterable[GeneratedFile] =
        for {
          dir <- dirs
          if dir.startsWith(originalTargetDir) && FileUtils.exists(dir)
          file <- Files.walk(dir).iterator().asScala.filter(Files.isRegularFile(_))
        } yield GeneratedFile(isResource = isResource, Files.readString(file), RelPath.relativeTo(dir, file))

      val sources = findFiles(bloopProject.sources, isResource = false)
      val resources = findFiles(bloopProject.resources.getOrElse(Nil), isResource = true)
      val all = (sources ++ resources).toVector
      if (all.nonEmpty) Some((crossName, all)) else None
    }
}
