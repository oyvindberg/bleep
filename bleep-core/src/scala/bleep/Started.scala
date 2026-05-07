package bleep

import bleep.rewrites.BuildRewrite
import ryddig.Logger

import java.nio.file.Path
import scala.concurrent.ExecutionContext

case class Started(
    pre: Prebootstrapped,
    rewrites: List[BuildRewrite],
    build: model.Build,
    resolvedProjects: ResolveProjects.Projects,
    activeProjectsFromPath: Option[Array[model.CrossProjectName]],
    config: model.BleepConfig,
    resolver: CoursierResolver,
    bleepExecutable: Lazy[BleepExecutable],
    bspServerClasspathSource: bsp.BspServerClasspathSource,
    jvmRunner: JvmRunner
)(reloadUsing: (Prebootstrapped, model.BleepConfig, List[BuildRewrite]) => Either[BleepException, Started]) {
  def buildPaths: BuildPaths = pre.buildPaths
  def userPaths: UserPaths = pre.userPaths
  def logger: Logger = pre.logger
  def executionContext: ExecutionContext = pre.ec
  def resolvedJvm: Lazy[ResolvedJvm] = pre.resolvedJvm

  /** Create a new Started with a different logger. Used to swap the StoringLogger for the real logger after decline parsing. */
  def withLogger(newLogger: Logger): Started = {
    val newPre = Prebootstrapped(newLogger, pre.userPaths, pre.buildPaths, pre.existingBuild, pre.ec)
    new Started(newPre, rewrites, build, resolvedProjects, activeProjectsFromPath, config, resolver, bleepExecutable, bspServerClasspathSource, jvmRunner)(
      reloadUsing
    )
  }

  /** Create a new Started with a different JvmRunner. Used by integration tests to swap the default forked runner for a heap-capped variant. */
  def withJvmRunner(newRunner: JvmRunner): Started =
    new Started(pre, rewrites, build, resolvedProjects, activeProjectsFromPath, config, resolver, bleepExecutable, bspServerClasspathSource, newRunner)(
      reloadUsing
    )

  def projectPaths(crossName: model.CrossProjectName): ProjectPaths =
    buildPaths.project(crossName, build.explodedProjects(crossName))

  lazy val globs: model.ProjectGlobs =
    new model.ProjectGlobs(activeProjectsFromPath, build.explodedProjects)

  def resolvedProject(crossName: model.CrossProjectName): ResolvedProject =
    resolvedProjects(crossName).forceGet

  /** @deprecated Use resolvedProject instead */
  def bloopProject(crossName: model.CrossProjectName): ResolvedProject =
    resolvedProject(crossName)

  lazy val jvmCommand: Path = resolvedJvm.forceGet.javaBin

  def chosenProjects(maybeFromCommandLine: Option[Array[model.CrossProjectName]]): Array[model.CrossProjectName] =
    maybeFromCommandLine match {
      case Some(fromCommandLine) => fromCommandLine.sorted
      case None                  =>
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
