package bleep
package commands

import bleep.RelPath
import bleep.internal.{FileUtils, Templates}
import bleep.model.{ExplodedBuild, JsonMap, JsonSet}
import bleep.rewrites.normalizeBuild
import bleep.toYaml.asYamlString

import java.nio.file.{Files, Path}
import scala.collection.immutable.SortedMap

case class BuildMoveFilesIntoBleepLayout(started: Started) extends BleepCommand {
  override def run(): Either[BleepException, Unit] = {
    val (build1, filesToMove) = BuildMoveFilesIntoBleepLayout.newBuildAndFilesToMove(started.build, started.buildPaths)
    val normalizedBuild = normalizeBuild(build1)
    val newTemplates = started.rawBuild.templates.value.map { case (templateId, p) =>
      (templateId, p.copy(sources = JsonSet.empty, resources = JsonSet.empty, `sbt-scope` = None, folder = None))
    }

    val build2 = Templates.reapply(normalizedBuild, JsonMap(newTemplates))

    // commit below
    filesToMove.foreach { case (from, to) =>
      started.logger.info(s"$from => $to")
      Files.createDirectories(to.getParent)
      scala.sys.process.Process(List("git", "mv", from.toString, to.toString), started.buildPaths.buildDir.toFile).!!
    }

    FileUtils.writeString(started.buildPaths.bleepYamlFile, asYamlString(build2))
    started.logger.withContext(started.buildPaths.bleepYamlFile).debug(s"wrote")
    Right(())
  }
}

object BuildMoveFilesIntoBleepLayout {
  def newBuildAndFilesToMove(build: ExplodedBuild, buildPaths: BuildPaths): (ExplodedBuild, SortedMap[Path, Path]) = {

    val moves = collection.mutable.Map.empty[Path, Path]
    def registerMove(from: Path, to: Path): Unit =
      if (!FileUtils.exists(from) || from == to) ()
      else
        moves.get(from) match {
          case None       => moves(from) = to
          case Some(`to`) => ()
          case Some(differentTo) =>
            sys.error(s"cannot move $from to both $to and $differentTo")
        }

    val newProjects = build.projects.map { case (crossName, p0) =>
      // compute paths for the same project after we remove `folder` and `sbt-scope`
      val p1 = p0.copy(folder = None, `sbt-scope` = None)
      val fromDirs = buildPaths.project(crossName, p0)
      val toDirs = buildPaths.project(crossName, p1)

      // move source folders from source layout
      // assume ordering stays the same before and after removing `folder` and `sbt-scope` here. it should.
      fromDirs.sourcesDirs.fromSourceLayout.toList.zip(toDirs.sourcesDirs.fromSourceLayout).foreach { case (from, to) => registerMove(from, to) }
      fromDirs.resourcesDirs.fromSourceLayout.toList.zip(toDirs.resourcesDirs.fromSourceLayout).foreach { case (from, to) => registerMove(from, to) }

      // then move explicit source folders. ordering may change here, and if files are located outside of project we compute
      // a new relative path instead
      def newPath(relPath: RelPath): RelPath =
        // outside of project dir we calculate new relative path and do no moving
        if (relPath.segments.headOption.contains("..")) {
          val from = fromDirs.dir / relPath
          RelPath.relativeTo(toDirs.dir, from)
        } else {
          val shortenedRelPath = p0.`sbt-scope`.foldLeft(relPath)((relPath, scope) => relPath.filter(_ != scope))
          registerMove(fromDirs.dir / relPath, toDirs.dir / shortenedRelPath)
          shortenedRelPath
        }

      val p2 = p1.copy(
        sources = p0.sources.map(newPath),
        resources = p0.resources.map(newPath)
      )
      (crossName, p2)
    }

    (build.copy(projects = newProjects), moves.to(SortedMap))
  }
}
