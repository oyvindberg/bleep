package bleep
package commands

import bleep.bsp.BspCommandFailed
import bleep.internal.{DoSourceGen, TransitiveProjects}
import ch.epfl.scala.bsp4j

import bloop.rifle.BuildServer

case class Test(watch: Boolean, projects: Array[model.CrossProjectName]) extends BleepCommandRemote(watch) with BleepCommandRemote.OnlyChanged {

  override def watchableProjects(started: Started): TransitiveProjects =
    TransitiveProjects(started.build, projects)

  override def onlyChangedProjects(started: Started, isChanged: model.CrossProjectName => Boolean): BleepCommandRemote =
    copy(projects = watchableProjects(started).transitiveFilter(isChanged).direct)

  override def runWithServer(started: Started, bloop: BuildServer): Either[BleepException, Unit] =
    DoSourceGen(started, bloop, watchableProjects(started)).flatMap { _ =>
      val targets = BleepCommandRemote.buildTargets(started.buildPaths, projects)
      val result = bloop.buildTargetTest(new bsp4j.TestParams(targets)).get()

      result.getStatusCode match {
        case bsp4j.StatusCode.OK =>
          started.logger.info("Tests succeeded")
          Right(())
        case errorCode =>
          Left(new BspCommandFailed("tests", projects, BspCommandFailed.StatusCode(errorCode)))
      }
    }
}
