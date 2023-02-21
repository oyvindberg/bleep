package bleep

import bleep.internal.TransitiveProjects
import bleep.logging.Logger

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

  def build(logger: Logger, existingBuild: BuildLoader.Existing)(onChange: () => Unit): FileWatching.Watcher =
    FileWatching(logger, Map(existingBuild.bleepYaml -> List(())))(_ => onChange())
}
