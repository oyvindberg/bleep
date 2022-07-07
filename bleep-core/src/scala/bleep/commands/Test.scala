package bleep
package commands

import bleep.bsp.BspCommandFailed
import ch.epfl.scala.bsp4j
import ch.epfl.scala.bsp4j.TestParams

import scala.build.bloop.BloopServer

case class Test(started: Started, projects: List[model.CrossProjectName]) extends BleepCommandRemote(started) {
  override def runWithServer(bloop: BloopServer): Either[BuildException, Unit] = {
    val targets = buildTargets(started.buildPaths, projects)
    val result = bloop.server.buildTargetTest(new TestParams(targets)).get()

    // todo: failing tests causes statusCode.OK

    result.getStatusCode match {
      case bsp4j.StatusCode.OK =>
        started.logger.info("Tests succeeded")
        Right(())
      case errorCode =>
        Left(new BspCommandFailed("tests", projects, BspCommandFailed.StatusCode(errorCode)))
    }
  }
}
