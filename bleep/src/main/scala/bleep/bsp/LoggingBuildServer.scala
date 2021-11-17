package bleep.bsp

import bleep.internal.stderr
import ch.epfl.scala.bsp4j.{DependencyModulesParams, DependencyModulesResult}
import ch.epfl.scala.bsp4j

import java.util.concurrent.CompletableFuture

trait LoggingBuildServer extends bsp4j.BuildServer {
  protected def underlying: bsp4j.BuildServer
  override def buildInitialize(
      params: bsp4j.InitializeBuildParams
  ): CompletableFuture[bsp4j.InitializeBuildResult] =
    underlying.buildInitialize(stderr.log(params)).logF
  override def onBuildExit(): Unit =
    underlying.onBuildExit()
  override def onBuildInitialized(): Unit =
    underlying.onBuildInitialized()
  override def buildShutdown(): CompletableFuture[Object] =
    underlying.buildShutdown().logF
  override def buildTargetCleanCache(
      params: bsp4j.CleanCacheParams
  ): CompletableFuture[bsp4j.CleanCacheResult] =
    underlying.buildTargetCleanCache(stderr.log(params)).logF
  override def buildTargetCompile(params: bsp4j.CompileParams): CompletableFuture[bsp4j.CompileResult] =
    underlying.buildTargetCompile(stderr.log(params)).logF
  override def buildTargetDependencySources(
      params: bsp4j.DependencySourcesParams
  ): CompletableFuture[bsp4j.DependencySourcesResult] =
    underlying.buildTargetDependencySources(stderr.log(params)).logF
  override def buildTargetInverseSources(
      params: bsp4j.InverseSourcesParams
  ): CompletableFuture[bsp4j.InverseSourcesResult] =
    underlying.buildTargetInverseSources(stderr.log(params)).logF
  override def buildTargetResources(
      params: bsp4j.ResourcesParams
  ): CompletableFuture[bsp4j.ResourcesResult] =
    underlying.buildTargetResources(stderr.log(params)).logF
  override def buildTargetRun(params: bsp4j.RunParams): CompletableFuture[bsp4j.RunResult] =
    underlying.buildTargetRun(stderr.log(params)).logF
  override def buildTargetSources(params: bsp4j.SourcesParams): CompletableFuture[bsp4j.SourcesResult] =
    underlying.buildTargetSources(stderr.log(params)).logF
  override def buildTargetTest(params: bsp4j.TestParams): CompletableFuture[bsp4j.TestResult] =
    underlying.buildTargetTest(stderr.log(params)).logF
  override def workspaceBuildTargets(): CompletableFuture[bsp4j.WorkspaceBuildTargetsResult] =
    underlying.workspaceBuildTargets().logF
  override def workspaceReload(): CompletableFuture[Object] =
    underlying.workspaceReload().logF
  override def buildTargetDependencyModules(params: DependencyModulesParams): CompletableFuture[DependencyModulesResult] =
    underlying.buildTargetDependencyModules(stderr.log(params)).logF
}
