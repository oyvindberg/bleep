package bleep

import bleep.logging.Logger
import bleep.rewrites.BuildRewrite
import bloop.config.Config
import coursier.jvm.JavaHome

import java.nio.file.Path
import scala.collection.immutable.SortedMap
import scala.concurrent.ExecutionContext

case class Started(
    prebootstrapped: Prebootstrapped,
    rewrites: List[BuildRewrite],
    build: model.Build,
    bloopFiles: GenBloopFiles.Files,
    activeProjectsFromPath: List[model.CrossProjectName],
    lazyConfig: Lazy[model.BleepConfig],
    resolver: Lazy[CoursierResolver],
    executionContext: ExecutionContext
) {
  def buildPaths: BuildPaths = prebootstrapped.buildPaths
  def userPaths: UserPaths = prebootstrapped.userPaths
  def logger: Logger = prebootstrapped.logger

  def projectPaths(crossName: model.CrossProjectName): ProjectPaths =
    buildPaths.project(crossName, build.explodedProjects(crossName))

  lazy val globs: model.ProjectGlobs =
    new model.ProjectGlobs(activeProjectsFromPath, build.explodedProjects)

  lazy val bloopProjects: SortedMap[model.CrossProjectName, Config.Project] =
    bloopFiles.map { case (name, lazyProject) => (name, lazyProject.forceGet.project) }

  lazy val bloopProjectsList: List[Config.Project] =
    bloopProjects.values.toList

  lazy val jvmCommand: Path = {
    val jvm = build.jvm.getOrElse {
      logger.warn(s"Using system JVM. You should specify your wanted JVM in ${BuildLoader.BuildFileName} to get reproducible builds")
      model.Jvm(JavaHome.systemId, None)
    }
    FetchJvm(new BleepCacheLogger(logger), jvm, executionContext)
  }

  def chosenProjects(maybeFromCommandLine: Option[List[model.CrossProjectName]]): List[model.CrossProjectName] =
    maybeFromCommandLine match {
      case Some(fromCommandLine) => fromCommandLine.sorted
      case None =>
        activeProjectsFromPath match {
          case Nil      => build.explodedProjects.keys.toList.sorted
          case nonEmpty => nonEmpty
        }
    }

  def chosenTestProjects(maybeFromCommandLine: Option[List[model.CrossProjectName]]): List[model.CrossProjectName] =
    chosenProjects(maybeFromCommandLine).filter(projectName => build.explodedProjects(projectName).isTestProject.getOrElse(false))
}
