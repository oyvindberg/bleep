package bleep
package commands

import ch.epfl.scala.bsp4j
import ch.epfl.scala.bsp4j.TestParams

import scala.build.bloop.BloopServer

case object Test extends BleepCommandRemote {
  override def runWithServer(started: Started, bloop: BloopServer): Unit = {
    val chosenProjects = chosenTargets(started)
    val result = bloop.server.buildTargetTest(new TestParams(chosenProjects)).get()
    result.getStatusCode match {
      case bsp4j.StatusCode.OK => started.logger.info("Tests succeeded")
      case bsp4j.StatusCode.ERROR => started.logger.warn("Tests failed")
      case bsp4j.StatusCode.CANCELLED => started.logger.info("Tests cancelled")
    }
  }
}
