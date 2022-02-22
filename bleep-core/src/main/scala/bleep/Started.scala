package bleep

import bleep.internal.Lazy
import bleep.logging.Logger
import bloop.config.Config

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
    bloopFiles: Map[model.CrossProjectName, Lazy[Config.File]],
    activeProjectsFromPath: List[model.CrossProjectName],
    lazyResolver: Lazy[CoursierResolver],
    userPaths: UserPaths,
    logger: Logger
) {
  def resolver = lazyResolver.forceGet

  lazy val bloopProjects: List[Config.Project] =
    bloopFiles.map { case (_, lazyProject) => lazyProject.forceGet.project }.toList

  def chosenProjects(maybeFromCommandLine: Option[List[model.CrossProjectName]]): List[model.CrossProjectName] =
    maybeFromCommandLine match {
      case Some(fromCommandLine) => fromCommandLine.sorted
      case None =>
        activeProjectsFromPath match {
          case Nil      => bloopFiles.keys.toList.sorted
          case nonEmpty => nonEmpty
        }
    }

  def chosenTestProjects(maybeFromCommandLine: Option[List[model.CrossProjectName]]): List[model.CrossProjectName] =
    chosenProjects(maybeFromCommandLine).filterNot(projectName => build.projects(projectName).testFrameworks.isEmpty)
}
