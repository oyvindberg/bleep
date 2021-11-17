package bleep.bsp

import bleep.internal.stderr
import ch.epfl.scala.bsp4j

import java.util.concurrent.CompletableFuture

trait LoggingScalaBuildServer extends bsp4j.ScalaBuildServer {
  protected def underlying: bsp4j.ScalaBuildServer
  override def buildTargetScalaMainClasses(
      params: bsp4j.ScalaMainClassesParams
  ): CompletableFuture[bsp4j.ScalaMainClassesResult] =
    underlying.buildTargetScalaMainClasses(stderr.log(params)).logF
  override def buildTargetScalaTestClasses(
      params: bsp4j.ScalaTestClassesParams
  ): CompletableFuture[bsp4j.ScalaTestClassesResult] =
    underlying.buildTargetScalaTestClasses(stderr.log(params)).logF
  override def buildTargetScalacOptions(
      params: bsp4j.ScalacOptionsParams
  ): CompletableFuture[bsp4j.ScalacOptionsResult] =
    underlying.buildTargetScalacOptions(stderr.log(params)).logF
}
