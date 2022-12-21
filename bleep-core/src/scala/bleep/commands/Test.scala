package bleep
package commands

import bleep.BleepException
import bleep.bsp.BspCommandFailed
import ch.epfl.scala.bsp4j

import scala.build.bloop.BloopServer

case class Test(started: Started, watch: Boolean, projects: Array[model.CrossProjectName])
    extends BleepCommandRemote(started, watch, projects)
    with BleepCommandRemote.OnlyChanged {

  override def onlyChangedProjects(isChanged: model.CrossProjectName => Boolean): BleepCommandRemote = {
    val ps = projects.filter(p => isChanged(p) || started.build.transitiveDependenciesFor(p).keys.exists(isChanged))
    copy(projects = ps)
  }

  override def runWithServer(bloop: BloopServer): Either[BleepException, Unit] = {
    val targets = buildTargets(started.buildPaths, projects)
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
