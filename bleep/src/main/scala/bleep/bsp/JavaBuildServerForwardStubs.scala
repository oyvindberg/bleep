package bleep.bsp

import ch.epfl.scala.bsp4j

import java.util.concurrent.CompletableFuture

trait JavaBuildServerForwardStubs extends bsp4j.JavaBuildServer {
  protected def forwardTo: bsp4j.JavaBuildServer

  override def buildTargetJavacOptions(
      params: bsp4j.JavacOptionsParams
  ): CompletableFuture[bsp4j.JavacOptionsResult] =
    forwardTo.buildTargetJavacOptions(params)
}
