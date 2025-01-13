package bleep
package commands

import bleep.bsp.BspCommandFailed
import bleep.internal.{DoSourceGen, TransitiveProjects}
import bloop.rifle.BuildServer
import ch.epfl.scala.bsp4j
import scala.jdk.CollectionConverters.*

case class Link(watch: Boolean, projects: Array[model.CrossProjectName], isRelaseMode: Boolean)
    extends BleepCommandRemote(watch)
    with BleepCommandRemote.OnlyChanged {

  override def watchableProjects(started: Started): TransitiveProjects =
    TransitiveProjects(started.build, projects)

  override def onlyChangedProjects(started: Started, isChanged: model.CrossProjectName => Boolean): Link =
    copy(projects = watchableProjects(started).transitiveFilter(isChanged).direct)

  override def runWithServer(started: Started, bloop: BuildServer): Either[BleepException, Unit] =
    DoSourceGen(started, bloop, watchableProjects(started)).flatMap { _ =>
      val targets = BleepCommandRemote.buildTargets(started.buildPaths, projects)

      val compileParams = new bsp4j.CompileParams(targets)
      compileParams.setArguments(List("--link").asJava)

      if (isRelaseMode) compileParams.setArguments(List("--link", "--release").asJava)

      val result = bloop.buildTargetCompile(compileParams).get()

      result.getStatusCode match {
        case bsp4j.StatusCode.OK => Right(started.logger.info("Linking succeeded"))
        case other               => Left(new BspCommandFailed(s"link", projects, BspCommandFailed.StatusCode(other)))
      }
    }
}
