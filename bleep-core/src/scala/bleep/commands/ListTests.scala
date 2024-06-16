package bleep
package commands

import bleep.internal.TransitiveProjects
import bloop.rifle.BuildServer
import ch.epfl.scala.bsp4j

import scala.jdk.CollectionConverters.*

case class ListTests(projects: Array[model.CrossProjectName]) extends BleepCommandRemote(watch = false) {
  override def watchableProjects(started: Started): TransitiveProjects =
    TransitiveProjects(started.build, projects)

  override def runWithServer(started: Started, bloop: BuildServer): Either[BleepException, Unit] = {
    val all: List[ListTests.Test] = ListTests.findTests(started, bloop, projects)

    all.groupBy { case ListTests.Test(pn, cls) => pn.name }.foreach { case (pn, tuples) =>
      started.logger.info(s"${pn.value}:")
      tuples.foreach(test => started.logger.info(s"  ${test.cls}"))
    }

    Right(())
  }
}

object ListTests {
  case class Test(project: model.CrossProjectName, cls: String)

  def findTests(started: Started, bloop: BuildServer, projects: Array[model.CrossProjectName]): List[ListTests.Test] = {
    val targets = BleepCommandRemote.buildTargets(started.buildPaths, projects)
    val result = bloop.buildTargetScalaTestClasses(new bsp4j.ScalaTestClassesParams(targets)).get()

    val it = for {
      item <- result.getItems.iterator().asScala
      projectName <- BleepCommandRemote.projectFromBuildTarget(started)(item.getTarget).iterator
      cls <- item.getClasses.iterator.asScala
    } yield ListTests.Test(projectName, cls)

    it.toList
  }
}
