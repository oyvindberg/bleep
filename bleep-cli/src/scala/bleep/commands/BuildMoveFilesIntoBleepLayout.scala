package bleep
package commands

import bleep.internal.FileUtils
import bleep.rewrites.BuildRewrite

import java.nio.file.Path
import scala.collection.immutable.SortedMap

object BuildMoveFilesIntoBleepLayout extends BleepBuildCommand {
  override def run(started: Started): Either[BleepException, Unit] = {
    val build = started.build.requireFileBacked(ctx = "command move-files-into-bleep-layout")
    val (rewrittenBuild, filesToMove) = newBuildAndFilesToMove(build, started.buildPaths)
    commit(started.logger, started.buildPaths, filesToMove, rewrittenBuild.file)
    Right(())
  }

  def newBuildAndFilesToMove(build: model.Build.FileBacked, buildPaths: BuildPaths): (model.Build.FileBacked, SortedMap[Path, Path]) = {

    val moves = collection.mutable.Map.empty[Path, Path]
    def registerMove(from: Path, to: Path): Unit =
      if (!FileUtils.exists(from) || from == to) ()
      else
        moves.get(from) match {
          case None              => moves(from) = to
          case Some(`to`)        => ()
          case Some(differentTo) => sys.error(s"cannot move $from to both $to and $differentTo")
        }

    val newProjects = build.explodedProjects.map { case (crossName, p0) =>
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
      def newPath(isResource: Boolean)(relPath: RelPath): RelPath = {
        val from = (if (isResource) fromDirs.resourcesDirs else fromDirs.sourcesDirs).fromJson(relPath)
        val to = (if (isResource) toDirs.resourcesDirs else toDirs.sourcesDirs).fromJson(relPath)

        // outside of project dir we calculate new relative path and do no moving
        if (!from.startsWith(fromDirs.dir)) {
          // note: commits
          RelPath.relativeTo(toDirs.dir, from)
        } else {
          registerMove(from, to)
          relPath.filter(_ != model.Replacements.known.Scope)
        }
      }

      val p2 = p1.copy(
        sources = p0.sources.map(newPath(isResource = false)),
        resources = p0.resources.map(newPath(isResource = true))
      )
      (crossName, p2)
    }

    val build1 = BuildRewrite.withProjects(build, newProjects)
    val filesToMove = moves.to(SortedMap)
    (build1, filesToMove)
  }
}
