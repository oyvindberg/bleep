package bleep.bsp

import ch.epfl.scala.bsp4j

import java.util.concurrent.CompletableFuture

trait ScalaBuildServerForwardStubs extends bsp4j.ScalaBuildServer {
  protected def forwardTo: bsp4j.ScalaBuildServer
  override def buildTargetScalaMainClasses(
    params: bsp4j.ScalaMainClassesParams
  ): CompletableFuture[bsp4j.ScalaMainClassesResult] =
    forwardTo.buildTargetScalaMainClasses(params)
  override def buildTargetScalaTestClasses(
    params: bsp4j.ScalaTestClassesParams
  ): CompletableFuture[bsp4j.ScalaTestClassesResult] =
    forwardTo.buildTargetScalaTestClasses(params)
  override def buildTargetScalacOptions(
    params: bsp4j.ScalacOptionsParams
  ): CompletableFuture[bsp4j.ScalacOptionsResult] =
    forwardTo.buildTargetScalacOptions(params)
}
