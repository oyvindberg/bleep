package bleep.bsp

import ch.epfl.scala.bsp4j.{DependencyModulesParams, DependencyModulesResult}
import ch.epfl.scala.bsp4j

import java.util.concurrent.CompletableFuture

trait BuildServerForwardStubs extends bsp4j.BuildServer {
  protected def forwardTo: bsp4j.BuildServer
  override def buildShutdown(): CompletableFuture[Object] =
    forwardTo.buildShutdown()
  override def buildTargetCleanCache(
      params: bsp4j.CleanCacheParams
  ): CompletableFuture[bsp4j.CleanCacheResult] =
    forwardTo.buildTargetCleanCache(params)
  override def buildTargetCompile(params: bsp4j.CompileParams): CompletableFuture[bsp4j.CompileResult] =
    forwardTo.buildTargetCompile(params)
  override def buildTargetDependencySources(
      params: bsp4j.DependencySourcesParams
  ): CompletableFuture[bsp4j.DependencySourcesResult] =
    forwardTo.buildTargetDependencySources(params)
  override def buildTargetInverseSources(
      params: bsp4j.InverseSourcesParams
  ): CompletableFuture[bsp4j.InverseSourcesResult] =
    forwardTo.buildTargetInverseSources(params)
  override def buildTargetResources(
      params: bsp4j.ResourcesParams
  ): CompletableFuture[bsp4j.ResourcesResult] =
    forwardTo.buildTargetResources(params)
  override def buildTargetRun(params: bsp4j.RunParams): CompletableFuture[bsp4j.RunResult] =
    forwardTo.buildTargetRun(params)
  override def buildTargetSources(params: bsp4j.SourcesParams): CompletableFuture[bsp4j.SourcesResult] =
    forwardTo.buildTargetSources(params)
  override def buildTargetTest(params: bsp4j.TestParams): CompletableFuture[bsp4j.TestResult] =
    forwardTo.buildTargetTest(params)
  override def workspaceBuildTargets(): CompletableFuture[bsp4j.WorkspaceBuildTargetsResult] =
    forwardTo.workspaceBuildTargets()
  override def workspaceReload(): CompletableFuture[Object] =
    forwardTo.workspaceReload()
  override def buildTargetDependencyModules(params: DependencyModulesParams): CompletableFuture[DependencyModulesResult] =
    forwardTo.buildTargetDependencyModules(params)
}
