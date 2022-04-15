package bleep

import bleep.internal.Lazy
import bleep.logging.Logger
import bloop.config.Config

import scala.collection.immutable.SortedMap

/** @param rawBuild
  *   non-exploded variant
  * @param bloopFiles
  *   will either all be resolved and written immediately if outdated, or read and parsed on demand
  */
case class Started(
    buildPaths: BuildPaths,
    rewrites: List[Rewrite],
    rawBuild: model.Build,
    build: ExplodedBuild,
    bloopFiles: GenBloopFiles.Files,
    activeProjectsFromPath: List[model.CrossProjectName],
    resolver: Lazy[CoursierResolver],
    userPaths: UserPaths,
    logger: Logger
) {
  def projectPaths(crossName: model.CrossProjectName): ProjectPaths =
    buildPaths.from(crossName, build.projects(crossName))

  lazy val bloopProjects: SortedMap[model.CrossProjectName, Config.Project] =
    bloopFiles.map { case (path, lazyProject) => (path, lazyProject.forceGet.project) }

  lazy val bloopProjectsList: List[Config.Project] =
    bloopProjects.values.toList

  def chosenProjects(maybeFromCommandLine: Option[List[model.CrossProjectName]]): List[model.CrossProjectName] =
    maybeFromCommandLine match {
      case Some(fromCommandLine) => fromCommandLine.sorted
      case None =>
        activeProjectsFromPath match {
          case Nil      => build.projects.keys.toList.sorted
          case nonEmpty => nonEmpty
        }
    }

  def chosenTestProjects(maybeFromCommandLine: Option[List[model.CrossProjectName]]): List[model.CrossProjectName] =
    chosenProjects(maybeFromCommandLine).filter(projectName => build.projects(projectName).isTestProject.getOrElse(false))
}
