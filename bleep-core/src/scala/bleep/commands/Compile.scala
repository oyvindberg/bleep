package bleep
package commands

import bleep.BleepException
import bleep.bsp.BspCommandFailed
import ch.epfl.scala.bsp4j

import scala.build.bloop.BloopServer

case class Compile(started: Started, watch: Boolean, projects: Array[model.CrossProjectName])
    extends BleepCommandRemote(started, watch, projects)
    with BleepCommandRemote.OnlyChanged {

  override def onlyChangedProjects(isChanged: model.CrossProjectName => Boolean): Compile = {
    val ps = projects.filter(p => isChanged(p) || started.build.transitiveDependenciesFor(p).keys.exists(isChanged))
    copy(projects = ps)
  }

  override def runWithServer(bloop: BloopServer): Either[BleepException, Unit] = {
    val targets = buildTargets(started.buildPaths, projects)

    val result = bloop.server.buildTargetCompile(new bsp4j.CompileParams(targets)).get()

    result.getStatusCode match {
      case bsp4j.StatusCode.OK => Right(started.logger.info("Compilation succeeded"))
      case other               => Left(new BspCommandFailed(s"compile", projects, BspCommandFailed.StatusCode(other)))
    }
  }
}
