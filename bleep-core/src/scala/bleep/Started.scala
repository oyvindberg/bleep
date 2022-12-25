package bleep

import bleep.internal.jvmOrSystem
import bleep.logging.Logger
import bleep.rewrites.BuildRewrite
import bloop.config.Config

import java.nio.file.Path
import scala.collection.immutable.SortedMap
import scala.concurrent.ExecutionContext

case class Started(
    pre: Prebootstrapped,
    rewrites: List[BuildRewrite],
    build: model.Build,
    bloopFiles: GenBloopFiles.Files,
    activeProjectsFromPath: Option[Array[model.CrossProjectName]],
    lazyConfig: Lazy[model.BleepConfig],
    resolver: Lazy[CoursierResolver],
    executionContext: ExecutionContext
)(reloadUsing: (Prebootstrapped, Lazy[model.BleepConfig], List[BuildRewrite]) => Either[BleepException, Started]) {
  def buildPaths: BuildPaths = pre.buildPaths
  def userPaths: UserPaths = pre.userPaths
  def logger: Logger = pre.logger

  def projectPaths(crossName: model.CrossProjectName): ProjectPaths =
    buildPaths.project(crossName, build.explodedProjects(crossName))

  lazy val globs: model.ProjectGlobs =
    new model.ProjectGlobs(activeProjectsFromPath, build.explodedProjects)

  lazy val bloopProjects: SortedMap[model.CrossProjectName, Config.Project] =
    bloopFiles.map { case (name, lazyProject) => (name, lazyProject.forceGet.project) }

  lazy val bloopProjectsList: List[Config.Project] =
    bloopProjects.values.toList

  lazy val jvmCommand: Path = {
    val jvm = jvmOrSystem(build, logger)
    FetchJvm(Some(userPaths.resolveJvmCacheDir), new BleepCacheLogger(logger), jvm, executionContext)
  }

  def chosenProjects(maybeFromCommandLine: Option[Array[model.CrossProjectName]]): Array[model.CrossProjectName] =
    maybeFromCommandLine match {
      case Some(fromCommandLine) => fromCommandLine.sorted
      case None =>
        activeProjectsFromPath match {
          case None           => build.explodedProjects.keys.toArray.sorted
          case Some(nonEmpty) => nonEmpty
        }
    }

  def chosenTestProjects(maybeFromCommandLine: Option[Array[model.CrossProjectName]]): Array[model.CrossProjectName] =
    chosenProjects(maybeFromCommandLine).filter(projectName => build.explodedProjects(projectName).isTestProject.getOrElse(false))

  def reloadFromDisk(rewrites: List[BuildRewrite]): Either[BleepException, Started] =
    for {
      pre <- pre.reloadFromDisk()
      lazyConfig = BleepConfigOps.lazyForceLoad(userPaths)
      reloaded <- reloadUsing(pre, lazyConfig, rewrites)
    } yield reloaded

  def reloadFromDisk(): Either[BleepException, Started] =
    reloadFromDisk(rewrites)
}
