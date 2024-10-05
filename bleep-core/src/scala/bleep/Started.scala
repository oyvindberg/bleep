package bleep

import bleep.rewrites.BuildRewrite
import bloop.config.Config
import ryddig.Logger

import java.nio.file.Path
import scala.concurrent.ExecutionContext

case class Started(
    pre: Prebootstrapped,
    rewrites: List[BuildRewrite],
    build: model.Build,
    bloopFiles: GenBloopFiles.Files,
    activeProjectsFromPath: Option[Array[model.CrossProjectName]],
    config: model.BleepConfig,
    resolver: CoursierResolver,
    bleepExecutable: Lazy[BleepExecutable]
)(reloadUsing: (Prebootstrapped, model.BleepConfig, List[BuildRewrite]) => Either[BleepException, Started]) {
  def buildPaths: BuildPaths = pre.buildPaths
  def userPaths: UserPaths = pre.userPaths
  def logger: Logger = pre.logger
  def executionContext: ExecutionContext = pre.ec
  def resolvedJvm: Lazy[ResolvedJvm] = pre.resolvedJvm

  def projectPaths(crossName: model.CrossProjectName): ProjectPaths =
    buildPaths.project(crossName, build.explodedProjects(crossName))

  lazy val globs: model.ProjectGlobs =
    new model.ProjectGlobs(activeProjectsFromPath, build.explodedProjects)

  def bloopProject(crossName: model.CrossProjectName): Config.Project =
    bloopFiles(crossName).forceGet.project

  lazy val jvmCommand: Path = resolvedJvm.forceGet.javaBin

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

  def chosenHasSourceGenProjects(maybeFromCommandLine: Option[Array[model.CrossProjectName]]): Array[model.CrossProjectName] =
    chosenProjects(maybeFromCommandLine).filterNot(projectName => build.explodedProjects(projectName).sourcegen.isEmpty)

  /** Will only reload if there are changes, as indicated in the `Option`
    */
  def reloadFromDisk(newRewrites: List[BuildRewrite]): Either[BleepException, Option[Started]] =
    for {
      maybeChangedPre <- pre.reloadFromDisk()
      newConfig <- BleepConfigOps.loadOrDefault(userPaths)
      configChanged = newConfig != config
      rewritesChanged = newRewrites != rewrites
      maybeReloaded <-
        if (maybeChangedPre.nonEmpty || configChanged || rewritesChanged)
          reloadUsing(maybeChangedPre.getOrElse(pre), newConfig, newRewrites).map(Some.apply)
        else Right(None)
    } yield maybeReloaded

  def reloadFromDisk(): Either[BleepException, Option[Started]] =
    reloadFromDisk(rewrites)
}
