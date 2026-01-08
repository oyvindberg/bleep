package bleep
package commands

import bleep.bsp.BspCommandFailed
import bleep.internal.{DoSourceGen, TransitiveProjects}
import bloop.rifle.BuildServer
import ch.epfl.scala.bsp4j

import scala.jdk.CollectionConverters.*

case class Compile(watch: Boolean, projects: Array[model.CrossProjectName]) extends BleepCommandRemote(watch) with BleepCommandRemote.OnlyChanged {

  override def watchableProjects(started: Started): TransitiveProjects =
    TransitiveProjects(started.build, projects)

  override def onlyChangedProjects(started: Started, isChanged: model.CrossProjectName => Boolean): Compile =
    copy(projects = watchableProjects(started).transitiveFilter(isChanged).direct)

  override def runWithServer(started: Started, bloop: BuildServer): Either[BleepException, Unit] =
    DoSourceGen(started, bloop, watchableProjects(started)).flatMap { _ =>
      val targets = BleepCommandRemote.buildTargets(started.buildPaths, projects)

      val params = new bsp4j.CompileParams(targets)
      params.setArguments(List("--show-rendered-message").asJava)
      val result = bloop.buildTargetCompile(params).get()

      result.getStatusCode match {
        case bsp4j.StatusCode.OK => Right(started.logger.info("Compilation succeeded"))
        case other               => Left(new BspCommandFailed(s"compile", projects, BspCommandFailed.StatusCode(other)))
      }
    }
}
