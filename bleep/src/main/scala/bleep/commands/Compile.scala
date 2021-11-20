package bleep
package commands

import ch.epfl.scala.bsp4j

import scala.build.bloop.BloopServer

case object Compile extends BleepCommandRemote {
  override def runWithServer(started: Started, bloop: BloopServer): Unit = {
    val chosenProjects = chosenTargets(started)
    val result = bloop.server.buildTargetCompile(new bsp4j.CompileParams(chosenProjects)).get()

    result.getStatusCode match {
      case bsp4j.StatusCode.OK        => started.logger.info("Compilation succeeded")
      case bsp4j.StatusCode.ERROR     => started.logger.warn("Compilation failed")
      case bsp4j.StatusCode.CANCELLED => started.logger.warn("Compilation cancelled")
    }
  }
}
