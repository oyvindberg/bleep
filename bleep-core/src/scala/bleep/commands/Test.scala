package bleep
package commands

import bleep.BleepException
import bleep.bsp.BspCommandFailed
import ch.epfl.scala.bsp4j
import ch.epfl.scala.bsp4j.CompileParams

import scala.build.bloop.BloopServer

case class Test(started: Started, projects: Array[model.CrossProjectName]) extends BleepCommandRemote(started) {
  override def runWithServer(bloop: BloopServer): Either[BleepException, Unit] = {
    val targets = buildTargets(started.buildPaths, projects)
    // workaround for https://github.com/scalacenter/bloop/pull/1839
    bloop.server.buildTargetCompile(new CompileParams(targets)).get()
    val result = bloop.server.buildTargetTest(new bsp4j.TestParams(targets)).get()

    result.getStatusCode match {
      case bsp4j.StatusCode.OK =>
        started.logger.info("Tests succeeded")
        Right(())
      case errorCode =>
        Left(new BspCommandFailed("tests", projects, BspCommandFailed.StatusCode(errorCode)))
    }
  }
}
