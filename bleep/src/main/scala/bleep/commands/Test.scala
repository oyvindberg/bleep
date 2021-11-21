package bleep
package commands

import cats.data.NonEmptyList
import ch.epfl.scala.bsp4j
import ch.epfl.scala.bsp4j.TestParams

import scala.build.bloop.BloopServer

case class Test(started: Started, opts: CommonOpts, projects: Option[NonEmptyList[model.ProjectName]]) extends BleepCommandRemote {
  override def runWithServer(bloop: BloopServer): Unit = {
    val targets = chosenTargets(started, projects)
    val result = bloop.server.buildTargetTest(new TestParams(targets)).get()
    result.getStatusCode match {
      case bsp4j.StatusCode.OK        => started.logger.info("Tests succeeded")
      case bsp4j.StatusCode.ERROR     => started.logger.warn("Tests failed")
      case bsp4j.StatusCode.CANCELLED => started.logger.info("Tests cancelled")
    }
  }
}
