package bleep.bsp

import bleep.Defaults
import bleep.internal.stderr
import ch.epfl.scala.bsp4j

import java.util.concurrent.CompletableFuture
import scala.build.bloop.BuildServer
import scala.build.blooprifle.internal.Constants
import scala.concurrent.{Future, Promise}

class BspForwardServer(remote: BuildServer, ensureBloopUpToDate: () => Unit) extends BuildServer {
  private def capabilities: bsp4j.BuildServerCapabilities =
    new bsp4j.BuildServerCapabilities

  var init: bsp4j.InitializeBuildParams = null
  override def buildInitialize(params: bsp4j.InitializeBuildParams): CompletableFuture[bsp4j.InitializeBuildResult] = {
    stderr.log(("onBuildInitialized", params))
    init = params
    CompletableFuture.completedFuture(new bsp4j.InitializeBuildResult("bleep", Defaults.version, Constants.bspVersion, capabilities))
  }

  override def workspaceBuildTargets(): CompletableFuture[bsp4j.WorkspaceBuildTargetsResult] = {
    stderr.log("workspaceBuildTargets");
    ensureBloopUpToDate();
    remote.buildInitialize(init);
    remote.workspaceBuildTargets()
  }

  //format: off
  override def onBuildInitialized(): Unit = {stderr.log("onBuildInitialized"); ensureBloopUpToDate(); remote.onBuildInitialized()}
  override def buildTargetCleanCache(params: bsp4j.CleanCacheParams): CompletableFuture[bsp4j.CleanCacheResult] = {stderr.log(("buildTargetCleanCache", params)); remote.buildTargetCleanCache(params)}
  override def buildTargetCompile(params: bsp4j.CompileParams): CompletableFuture[bsp4j.CompileResult] = {stderr.log(("buildTargetCompile", params)); remote.buildTargetCompile(params)}
  override def buildTargetDependencySources(params: bsp4j.DependencySourcesParams): CompletableFuture[bsp4j.DependencySourcesResult] = {stderr.log(("buildTargetDependencySources", params)); remote.buildTargetDependencySources(params)}
  override def buildTargetInverseSources(params: bsp4j.InverseSourcesParams): CompletableFuture[bsp4j.InverseSourcesResult] = {stderr.log(("buildTargetInverseSources", params)); remote.buildTargetInverseSources(params)}
  override def buildTargetResources(params: bsp4j.ResourcesParams): CompletableFuture[bsp4j.ResourcesResult] = {stderr.log(("buildTargetResources", params)); remote.buildTargetResources(params)}
  override def buildTargetRun(params: bsp4j.RunParams): CompletableFuture[bsp4j.RunResult] = {stderr.log(("buildTargetRun", params)); remote.buildTargetRun(params)}
  override def buildTargetSources(params: bsp4j.SourcesParams): CompletableFuture[bsp4j.SourcesResult] = {stderr.log(("buildTargetSources", params)); remote.buildTargetSources(params)}
  override def buildTargetTest(params: bsp4j.TestParams): CompletableFuture[bsp4j.TestResult] = {stderr.log(("buildTargetTest", params)); remote.buildTargetTest(params)}
  override def workspaceReload(): CompletableFuture[Object] = {stderr.log(("workspaceReload")); ensureBloopUpToDate(); remote.workspaceReload()}
  override def buildTargetDependencyModules(params: bsp4j.DependencyModulesParams): CompletableFuture[bsp4j.DependencyModulesResult] = {stderr.log(("buildTargetDependencyModules", params)); remote.buildTargetDependencyModules(params)}
  override def buildTargetJavacOptions(params: bsp4j.JavacOptionsParams): CompletableFuture[bsp4j.JavacOptionsResult] = {stderr.log(("buildTargetJavacOptions", params)); remote.buildTargetJavacOptions(params)}
  override def buildTargetScalaMainClasses(params: bsp4j.ScalaMainClassesParams): CompletableFuture[bsp4j.ScalaMainClassesResult] = {stderr.log(("buildTargetScalaMainClasses", params)); remote.buildTargetScalaMainClasses(params)}
  override def buildTargetScalaTestClasses(params: bsp4j.ScalaTestClassesParams): CompletableFuture[bsp4j.ScalaTestClassesResult] = {stderr.log(("buildTargetScalaTestClasses", params)); remote.buildTargetScalaTestClasses(params)}
  override def buildTargetScalacOptions(params: bsp4j.ScalacOptionsParams): CompletableFuture[bsp4j.ScalacOptionsResult] = {stderr.log(("buildTargetScalacOptions", params)); remote.buildTargetScalacOptions(params)}
  override def buildTargetDebugSession(params: bsp4j.DebugSessionParams): CompletableFuture[bsp4j.DebugSessionAddress] = {stderr.log(("buildTargetDebugSession", params)); remote.buildTargetDebugSession(params)}
  //format: on

  private val shutdownPromise = Promise[Unit]()

  override def buildShutdown(): CompletableFuture[Object] = {
    stderr.log("buildShutdown")
    if (!shutdownPromise.isCompleted)
      shutdownPromise.success(())
    remote.buildShutdown()
  }

  override def onBuildExit(): Unit = {
    stderr.log("onBuildExit")
    ()
  }

  def initiateShutdown: Future[Unit] =
    shutdownPromise.future
}
