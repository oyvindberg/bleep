package bleep.bsp

import bleep.logging.Logger
import bleep.{constants, BuildException}
import ch.epfl.scala.bsp4j

import java.util
import java.util.concurrent.CompletableFuture
import scala.build.bloop.BuildServer
import scala.build.blooprifle.internal.Constants
import scala.concurrent.{Future, Promise}
import scala.jdk.CollectionConverters._

class BleepBspServer(val logger: Logger, var bloopServer: BuildServer, var ensureBloopUpToDate: () => Either[BuildException, Unit]) extends BuildServer {
  private def capabilities: bsp4j.BuildServerCapabilities = {
    val supportedLanguages: util.List[String] = List("scala", "java").asJava
    val ret = new bsp4j.BuildServerCapabilities
    ret.setCompileProvider(new bsp4j.CompileProvider(supportedLanguages))
    ret.setTestProvider(new bsp4j.TestProvider(supportedLanguages))
    ret.setRunProvider(new bsp4j.RunProvider(supportedLanguages))
    ret.setDebugProvider(new bsp4j.DebugProvider(supportedLanguages))
    ret.setInverseSourcesProvider(true)
    ret.setDependencySourcesProvider(true)
    ret.setResourcesProvider(true)
    ret.setBuildTargetChangedProvider(true)
    ret.setJvmRunEnvironmentProvider(true)
    ret.setJvmTestEnvironmentProvider(true)
    ret.setCanReload(true)
    ret.setDependencyModulesProvider(true)
    ret
  }

  override def buildInitialize(params: bsp4j.InitializeBuildParams): CompletableFuture[bsp4j.InitializeBuildResult] = {
    logger.debug(("onBuildInitialized", params.toString))
    CompletableFuture.completedFuture(new bsp4j.InitializeBuildResult("bleep", constants.version, Constants.bspVersion, capabilities))
  }

  override def workspaceBuildTargets(): CompletableFuture[bsp4j.WorkspaceBuildTargetsResult] = {
    logger.debug("workspaceBuildTargets")

    ensureBloopUpToDate() match {
      case Left(th) =>
        logger.error("couldn't refresh build", th)
        CompletableFuture.failedFuture(th)
      case Right(()) => bloopServer.workspaceBuildTargets()
    }
  }

  override def onBuildInitialized(): Unit = {
    logger.debug("onBuildInitialized")
    ()
  }

  override def workspaceReload(): CompletableFuture[Object] = {
    logger.debug("workspaceReload")
    // Bloop does not support workspaceReload and Intellij calls it at the start
    CompletableFuture.completedFuture(new Object)
  }

  override def buildTargetCleanCache(params: bsp4j.CleanCacheParams): CompletableFuture[bsp4j.CleanCacheResult] = {
    logger.debug(("buildTargetCleanCache", params.toString))
    bloopServer.buildTargetCleanCache(params)
  }
  override def buildTargetCompile(params: bsp4j.CompileParams): CompletableFuture[bsp4j.CompileResult] = {
    logger.debug(("buildTargetCompile", params.toString))
    bloopServer.buildTargetCompile(params)
  }
  override def buildTargetDependencySources(params: bsp4j.DependencySourcesParams): CompletableFuture[bsp4j.DependencySourcesResult] = {
    logger.debug(("buildTargetDependencySources", params.toString))
    bloopServer.buildTargetDependencySources(params)
  }
  override def buildTargetInverseSources(params: bsp4j.InverseSourcesParams): CompletableFuture[bsp4j.InverseSourcesResult] = {
    logger.debug(("buildTargetInverseSources", params.toString))
    bloopServer.buildTargetInverseSources(params)
  }
  override def buildTargetResources(params: bsp4j.ResourcesParams): CompletableFuture[bsp4j.ResourcesResult] = {
    logger.debug(("buildTargetResources", params.toString))
    bloopServer.buildTargetResources(params)
  }
  override def buildTargetRun(params: bsp4j.RunParams): CompletableFuture[bsp4j.RunResult] = {
    logger.debug(("buildTargetRun", params.toString))
    bloopServer.buildTargetRun(params)
  }
  override def buildTargetSources(params: bsp4j.SourcesParams): CompletableFuture[bsp4j.SourcesResult] = {
    logger.debug(("buildTargetSources", params.toString))
    bloopServer.buildTargetSources(params)
  }
  override def buildTargetTest(params: bsp4j.TestParams): CompletableFuture[bsp4j.TestResult] = {
    logger.debug(("buildTargetTest", params.toString))
    bloopServer.buildTargetTest(params)
  }
  override def buildTargetDependencyModules(params: bsp4j.DependencyModulesParams): CompletableFuture[bsp4j.DependencyModulesResult] = {
    logger.debug(("buildTargetDependencyModules", params.toString))
    bloopServer.buildTargetDependencyModules(params)
  }
  override def buildTargetJavacOptions(params: bsp4j.JavacOptionsParams): CompletableFuture[bsp4j.JavacOptionsResult] = {
    logger.debug(("buildTargetJavacOptions", params.toString))
    bloopServer.buildTargetJavacOptions(params)
  }
  override def buildTargetScalaMainClasses(params: bsp4j.ScalaMainClassesParams): CompletableFuture[bsp4j.ScalaMainClassesResult] = {
    logger.debug(("buildTargetScalaMainClasses", params.toString))
    bloopServer.buildTargetScalaMainClasses(params)
  }
  override def buildTargetScalaTestClasses(params: bsp4j.ScalaTestClassesParams): CompletableFuture[bsp4j.ScalaTestClassesResult] = {
    logger.debug(("buildTargetScalaTestClasses", params.toString))
    bloopServer.buildTargetScalaTestClasses(params)
  }
  override def buildTargetScalacOptions(params: bsp4j.ScalacOptionsParams): CompletableFuture[bsp4j.ScalacOptionsResult] = {
    logger.debug(("buildTargetScalacOptions", params.toString))
    bloopServer.buildTargetScalacOptions(params)
  }
  override def buildTargetDebugSession(params: bsp4j.DebugSessionParams): CompletableFuture[bsp4j.DebugSessionAddress] = {
    logger.debug(("buildTargetDebugSession", params.toString))
    bloopServer.buildTargetDebugSession(params)
  }

  private val shutdownPromise = Promise[Unit]()

  override def buildShutdown(): CompletableFuture[Object] = {
    logger.debug("buildShutdown")
    if (!shutdownPromise.isCompleted)
      shutdownPromise.success(())
    bloopServer.buildShutdown()
  }

  override def onBuildExit(): Unit = {
    logger.debug("onBuildExit")
    ()
  }

  def initiateShutdown: Future[Unit] =
    shutdownPromise.future
}
