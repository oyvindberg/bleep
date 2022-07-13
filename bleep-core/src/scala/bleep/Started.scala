package bleep

import bloop.config.Config

import java.nio.file.Path
import scala.collection.immutable.SortedMap
import scala.concurrent.ExecutionContext

case class Started(
    prebootstrapped: Prebootstrapped,
    rewrites: List[Rewrite],
    build: ExplodedBuild,
    bloopFiles: GenBloopFiles.Files,
    activeProjectsFromPath: List[model.CrossProjectName],
    lazyConfig: Lazy[BleepConfig],
    resolver: Lazy[CoursierResolver],
    executionContext: ExecutionContext
) {
  def buildPaths: BuildPaths = prebootstrapped.buildPaths
  def userPaths: UserPaths = prebootstrapped.userPaths
  def rawBuild = build.build
  def logger = prebootstrapped.logger

  def projectPaths(crossName: model.CrossProjectName): ProjectPaths =
    buildPaths.project(crossName, build.projects(crossName))

  lazy val bloopProjects: SortedMap[model.CrossProjectName, Config.Project] =
    bloopFiles.map { case (name, lazyProject) => (name, lazyProject.forceGet.project) }

  lazy val bloopProjectsList: List[Config.Project] =
    bloopProjects.values.toList

  lazy val jvmCommand: Path = {
    val jvm = rawBuild.jvm.getOrElse {
      logger.warn(s"Using system JVM. You should specify your wanted JVM in ${constants.BuildFileName} to get reproducible builds")
      model.Jvm.System
    }
    FetchJvm(logger, jvm, executionContext)
  }

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
