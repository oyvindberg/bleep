package bleep

import bleep.internal.TransitiveProjects

import java.nio.file.Path

object BleepFileWatching {
  def projectPathsMapping(started: Started, projects: TransitiveProjects): Map[Path, Seq[model.CrossProjectName]] = {
    val sourceProjectPairs: Iterator[(Path, model.CrossProjectName)] =
      projects.all.iterator.flatMap { name =>
        val p = started.build.explodedProjects(name)
        val paths = started.buildPaths.project(name, p)
        val allPaths = paths.sourcesDirs.all.iterator ++ paths.resourcesDirs.all
        val fromSourceGen: Iterator[Path] =
          p.sourcegen.values.iterator.flatMap { case model.ScriptDef.Main(_, _, sourceGlobs) =>
            sourceGlobs.values.iterator.map(relPath => paths.dir / relPath)
          }
        (allPaths ++ fromSourceGen).map(path => (path, name))
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
