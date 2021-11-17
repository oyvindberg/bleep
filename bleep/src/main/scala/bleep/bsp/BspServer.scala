package bleep.bsp

import bleep.Defaults
import ch.epfl.scala.bsp4j

import java.util.concurrent.CompletableFuture
import scala.build.bloop.{ScalaDebugServer, ScalaDebugServerForwardStubs}
import scala.concurrent.{Future, Promise}

class BspServer(
    bloopServer: bsp4j.BuildServer with bsp4j.ScalaBuildServer with bsp4j.JavaBuildServer with ScalaDebugServer,
) extends bsp4j.BuildServer
    with bsp4j.ScalaBuildServer
    with bsp4j.JavaBuildServer
    with BuildServerForwardStubs
    with ScalaDebugServerForwardStubs
    with ScalaBuildServerForwardStubs
    with JavaBuildServerForwardStubs {

  protected def forwardTo: bsp4j.BuildServer with bsp4j.ScalaBuildServer with bsp4j.JavaBuildServer with ScalaDebugServer =
    bloopServer

  private def capabilities: bsp4j.BuildServerCapabilities =
    new bsp4j.BuildServerCapabilities

  override def buildInitialize(params: bsp4j.InitializeBuildParams): CompletableFuture[bsp4j.InitializeBuildResult] = {
    val res = new bsp4j.InitializeBuildResult(
      "bleep",
      Defaults.version,
      scala.build.blooprifle.internal.Constants.bspVersion,
      capabilities
    )
    CompletableFuture.completedFuture(res)
  }

  override def onBuildInitialized(): Unit = ()

  override def buildTargetCleanCache(params: bsp4j.CleanCacheParams): CompletableFuture[bsp4j.CleanCacheResult] =
    super.buildTargetCleanCache(params)

  override def buildTargetCompile(params: bsp4j.CompileParams): CompletableFuture[bsp4j.CompileResult] =
    super.buildTargetCompile(params)

  override def buildTargetDependencySources(params: bsp4j.DependencySourcesParams): CompletableFuture[bsp4j.DependencySourcesResult] =
    super.buildTargetDependencySources(params)

  override def buildTargetResources(params: bsp4j.ResourcesParams): CompletableFuture[bsp4j.ResourcesResult] =
    super.buildTargetResources(params)

  override def buildTargetRun(params: bsp4j.RunParams): CompletableFuture[bsp4j.RunResult] =
    super.buildTargetRun(params)

  override def buildTargetSources(params: bsp4j.SourcesParams): CompletableFuture[bsp4j.SourcesResult] =
    super.buildTargetSources(params)

  override def buildTargetTest(params: bsp4j.TestParams): CompletableFuture[bsp4j.TestResult] =
    super.buildTargetTest(params)

  override def workspaceBuildTargets(): CompletableFuture[bsp4j.WorkspaceBuildTargetsResult] =
    super.workspaceBuildTargets()

  private val shutdownPromise = Promise[Unit]()
  override def buildShutdown(): CompletableFuture[Object] = {
    if (!shutdownPromise.isCompleted)
      shutdownPromise.success(())
    CompletableFuture.completedFuture(null)
  }

  override def onBuildExit(): Unit = ()

  def initiateShutdown: Future[Unit] =
    shutdownPromise.future
}
