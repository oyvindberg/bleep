package bleep
package commands

import ch.epfl.scala.bsp4j
import ch.epfl.scala.bsp4j.TestParams

import scala.build.bloop.BloopServer

case class Test(started: Started, fromCommandLine: Option[List[model.CrossProjectName]]) extends BleepCommandRemote {
  override def runWithServer(bloop: BloopServer): Either[BuildException, Unit] = {
    val chosenProjects = started.chosenTestProjects(fromCommandLine)
    val targets = buildTargets(started.buildPaths, chosenProjects)
    val result = bloop.server.buildTargetTest(new TestParams(targets)).get()

    // todo: failing tests causes statusCode.OK

    result.getStatusCode match {
      case bsp4j.StatusCode.OK =>
        started.logger.info("Tests succeeded")
        Right(())
      case other =>
        Left(new BspCommandFailed("tests", chosenProjects, other))
    }
  }
}
