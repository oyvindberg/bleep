package bleep

import bleep.internal.TransitiveProjects

import java.nio.file.Path
import scala.collection.compat._

object BleepFileWatching {
  model.assertUsed(immutable.LazyList) // silence warning

  def projectPathsMapping(started: Started, projects: TransitiveProjects): Map[Path, Seq[model.CrossProjectName]] = {
    val sourceProjectPairs: Iterator[(Path, model.CrossProjectName)] =
      projects.all.iterator.flatMap { name =>
        val p = started.build.explodedProjects(name)
        val paths = started.buildPaths.project(name, p)
        val allPaths = paths.sourcesDirs.all ++ paths.resourcesDirs.all

        allPaths.iterator.map(path => (path, name))
      }

    sourceProjectPairs.toSeq.groupMap { case (p, _) => p } { case (_, name) => name }
  }

  def projects(started: Started, projects: TransitiveProjects)(
      onChange: Set[model.CrossProjectName] => Unit
  ): FileWatching.TypedWatcher[model.CrossProjectName] =
    FileWatching(started.logger, projectPathsMapping(started, projects))(onChange)

  def build(pre: Prebootstrapped)(onChange: () => Unit): FileWatching.Watcher = {
    val files = List(
      pre.existingBuild.bleepYaml,
      pre.userPaths.configYaml,
      pre.buildPaths.bspProjectSelectionYaml
    )
    FileWatching(pre.logger, files.map(f => (f, List(()))).toMap)(_ => onChange())
  }
}
