package bleep
package commands

import ch.epfl.scala.bsp4j
import ch.epfl.scala.bsp4j.TestParams

import scala.build.bloop.BloopServer

case class Test(started: Started, fromCommandLine: Option[List[model.CrossProjectName]]) extends BleepCommandRemote {
  override def runWithServer(bloop: BloopServer): Unit = {
    val chosenProjects = started.chosenTestProjects(fromCommandLine)
    val targets = buildTargets(started.buildPaths, chosenProjects)
    val result = bloop.server.buildTargetTest(new TestParams(targets)).get()
    result.getStatusCode match {
      case bsp4j.StatusCode.OK        => started.logger.info("Tests succeeded")
      case bsp4j.StatusCode.ERROR     => started.logger.warn("Tests failed")
      case bsp4j.StatusCode.CANCELLED => started.logger.info("Tests cancelled")
    }
  }
}
