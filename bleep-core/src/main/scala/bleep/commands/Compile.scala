package bleep
package commands

import bleep.bsp.BspCommandFailed
import ch.epfl.scala.bsp4j

import scala.build.bloop.BloopServer

case class Compile(started: Started, projects: List[model.CrossProjectName]) extends BleepCommandRemote(started) {
  override def runWithServer(bloop: BloopServer): Either[BuildException, Unit] = {
    val targets = buildTargets(started.buildPaths, projects)
    val result = bloop.server.buildTargetCompile(new bsp4j.CompileParams(targets)).get()

    result.getStatusCode match {
      case bsp4j.StatusCode.OK => Right(started.logger.info("Compilation succeeded"))
      case other               => Left(new BspCommandFailed(s"compile", projects, BspCommandFailed.StatusCode(other)))
    }
  }
}
