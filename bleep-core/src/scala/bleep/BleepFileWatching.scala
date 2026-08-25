package bleep

import bleep.internal.TransitiveProjects

import java.nio.file.Path

object BleepFileWatching {
  def projectPathsMapping(started: Started, projects: TransitiveProjects): Map[Path, Seq[model.CrossProjectName]] = {
    val sourceProjectPairs: Iterator[(Path, model.CrossProjectName)] =
      projects.all.iterator.flatMap { name =>
        val p = started.build.explodedProjects(name)
        val paths = started.buildPaths.project(name, p)
        ProjectInputs.all(p, paths).iterator.map(path => (path, name))
      }

    sourceProjectPairs.toSeq.groupMap { case (p, _) => p } { case (_, name) => name }
  }

  def projects(started: Started, projects: TransitiveProjects)(
      onChange: Set[model.CrossProjectName] => Unit
  ): FileWatching.TypedWatcher[model.CrossProjectName] =
    FileWatching(started.logger, projectPathsMapping(started, projects))(onChange)

  def build(pre: Prebootstrapped)(onChange: Set[String] => Unit): FileWatching.Watcher =
    FileWatching(
      pre.logger,
      mapping = Map(
        pre.existingBuild.bleepYaml -> List("bleep build"),
        pre.userPaths.configYaml -> List("bleep config"),
        pre.buildPaths.bspProjectSelectionYaml -> List("project selection in IDE")
      )
    )(onChange)
}
