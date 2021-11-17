package bleep.bsp

import bleep.internal.stderr
import ch.epfl.scala.bsp4j

import java.util.concurrent.CompletableFuture

trait LoggingJavaBuildServer extends bsp4j.JavaBuildServer {
  protected def underlying: bsp4j.JavaBuildServer
  override def buildTargetJavacOptions(
    params: bsp4j.JavacOptionsParams
  ): CompletableFuture[bsp4j.JavacOptionsResult] =
    underlying.buildTargetJavacOptions(stderr.log(params)).logF
}
