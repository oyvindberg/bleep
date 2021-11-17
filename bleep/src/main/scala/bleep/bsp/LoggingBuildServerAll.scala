package bleep.bsp

import ch.epfl.scala.bsp4j

class LoggingBuildServerAll(
  val underlying: bsp4j.BuildServer with bsp4j.ScalaBuildServer with bsp4j.JavaBuildServer
) extends LoggingBuildServer with LoggingScalaBuildServer with LoggingJavaBuildServer
