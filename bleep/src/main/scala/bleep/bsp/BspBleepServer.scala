package bleep.bsp

import bleep.{BuildException, Defaults, Started}
import bleep.internal.stderr
import ch.epfl.scala.bsp4j

import java.util.concurrent.CompletableFuture
import java.util
import scala.build.bloop.BuildServer
import scala.build.blooprifle.internal.Constants
import scala.concurrent.{Future, Promise}
import scala.jdk.CollectionConverters._

class BspBleepServer(bloopServer: BuildServer, ensureBloopUpToDate: () => Either[BuildException, Started]) extends BuildServer {
  private val supportedLanguages: util.List[String] = List("scala", "java").asJava

  private def capabilities: bsp4j.BuildServerCapabilities = {
    val capabilities = new bsp4j.BuildServerCapabilities
    capabilities.setCompileProvider(new bsp4j.CompileProvider(supportedLanguages))
    capabilities.setTestProvider(new bsp4j.TestProvider(supportedLanguages))
    capabilities.setRunProvider(new bsp4j.RunProvider(supportedLanguages))
    capabilities.setDebugProvider(new bsp4j.DebugProvider(supportedLanguages))
    capabilities.setInverseSourcesProvider(true)
    capabilities.setDependencySourcesProvider(true)
    capabilities.setResourcesProvider(true)
    capabilities.setBuildTargetChangedProvider(true)
    capabilities.setJvmRunEnvironmentProvider(true)
    capabilities.setJvmTestEnvironmentProvider(true)
    capabilities.setCanReload(true)
    capabilities.setDependencyModulesProvider(true)
    capabilities
  }

  override def buildInitialize(params: bsp4j.InitializeBuildParams): CompletableFuture[bsp4j.InitializeBuildResult] = {
    stderr.log(("onBuildInitialized", params))
    CompletableFuture.completedFuture(new bsp4j.InitializeBuildResult("bleep", Defaults.version, Constants.bspVersion, capabilities))
  }

  override def workspaceBuildTargets(): CompletableFuture[bsp4j.WorkspaceBuildTargetsResult] = {
    stderr.log("workspaceBuildTargets");
    ensureBloopUpToDate() match {
      case Left(th) => CompletableFuture.failedFuture(th)
      case Right(_) => bloopServer.workspaceBuildTargets()
    }
  }

  override def onBuildInitialized(): Unit = {
    stderr.log("onBuildInitialized")
    ensureBloopUpToDate() match {
      case Left(th) => CompletableFuture.failedFuture(th)
      case Right(_) => bloopServer.onBuildInitialized()
    }
  }
  
  //format: off
  override def buildTargetCleanCache(params: bsp4j.CleanCacheParams): CompletableFuture[bsp4j.CleanCacheResult] = {stderr.log(("buildTargetCleanCache", params)); bloopServer.buildTargetCleanCache(params)}
  override def buildTargetCompile(params: bsp4j.CompileParams): CompletableFuture[bsp4j.CompileResult] = {stderr.log(("buildTargetCompile", params)); bloopServer.buildTargetCompile(params)}
  override def buildTargetDependencySources(params: bsp4j.DependencySourcesParams): CompletableFuture[bsp4j.DependencySourcesResult] = {stderr.log(("buildTargetDependencySources", params)); bloopServer.buildTargetDependencySources(params)}
  override def buildTargetInverseSources(params: bsp4j.InverseSourcesParams): CompletableFuture[bsp4j.InverseSourcesResult] = {stderr.log(("buildTargetInverseSources", params)); bloopServer.buildTargetInverseSources(params)}
  override def buildTargetResources(params: bsp4j.ResourcesParams): CompletableFuture[bsp4j.ResourcesResult] = {stderr.log(("buildTargetResources", params)); bloopServer.buildTargetResources(params)}
  override def buildTargetRun(params: bsp4j.RunParams): CompletableFuture[bsp4j.RunResult] = {stderr.log(("buildTargetRun", params)); bloopServer.buildTargetRun(params)}
  override def buildTargetSources(params: bsp4j.SourcesParams): CompletableFuture[bsp4j.SourcesResult] = {stderr.log(("buildTargetSources", params)); bloopServer.buildTargetSources(params)}
  override def buildTargetTest(params: bsp4j.TestParams): CompletableFuture[bsp4j.TestResult] = {stderr.log(("buildTargetTest", params)); bloopServer.buildTargetTest(params)}
  // Bloop does not support workspaceReload and Intellij calls it at the start
  override def workspaceReload(): CompletableFuture[Object] = {stderr.log("workspaceReload"); CompletableFuture.completedFuture(new Object)}
  override def buildTargetDependencyModules(params: bsp4j.DependencyModulesParams): CompletableFuture[bsp4j.DependencyModulesResult] = {stderr.log(("buildTargetDependencyModules", params)); bloopServer.buildTargetDependencyModules(params)}
  override def buildTargetJavacOptions(params: bsp4j.JavacOptionsParams): CompletableFuture[bsp4j.JavacOptionsResult] = {stderr.log(("buildTargetJavacOptions", params)); bloopServer.buildTargetJavacOptions(params)}
  override def buildTargetScalaMainClasses(params: bsp4j.ScalaMainClassesParams): CompletableFuture[bsp4j.ScalaMainClassesResult] = {stderr.log(("buildTargetScalaMainClasses", params)); bloopServer.buildTargetScalaMainClasses(params)}
  override def buildTargetScalaTestClasses(params: bsp4j.ScalaTestClassesParams): CompletableFuture[bsp4j.ScalaTestClassesResult] = {stderr.log(("buildTargetScalaTestClasses", params)); bloopServer.buildTargetScalaTestClasses(params)}
  override def buildTargetScalacOptions(params: bsp4j.ScalacOptionsParams): CompletableFuture[bsp4j.ScalacOptionsResult] = {stderr.log(("buildTargetScalacOptions", params)); bloopServer.buildTargetScalacOptions(params)}
  override def buildTargetDebugSession(params: bsp4j.DebugSessionParams): CompletableFuture[bsp4j.DebugSessionAddress] = {stderr.log(("buildTargetDebugSession", params)); bloopServer.buildTargetDebugSession(params)}
  //format: on

  private val shutdownPromise = Promise[Unit]()

  override def buildShutdown(): CompletableFuture[Object] = {
    stderr.log("buildShutdown")
    if (!shutdownPromise.isCompleted)
      shutdownPromise.success(())
    bloopServer.buildShutdown()
  }

  override def onBuildExit(): Unit = {
    stderr.log("onBuildExit")
    ()
  }

  def initiateShutdown: Future[Unit] =
    shutdownPromise.future
}
