package bleep
package commands

import bleep.internal.TransitiveProjects
import bloop.rifle.BuildServer
import ch.epfl.scala.bsp4j
import ch.epfl.scala.bsp4j.ScalaTestClassesParams

import scala.jdk.CollectionConverters.*

case class ListTests(projects: Array[model.CrossProjectName]) extends BleepCommandRemote(watch = false) {

  override def watchableProjects(started: Started): TransitiveProjects =
    TransitiveProjects(started.build, projects)

  override def runWithServer(started: Started, bloop: BuildServer): Either[BleepException, Unit] = {
    val all: Iterator[(model.CrossProjectName, String)] = testsByCrossProject(started, bloop)

    all.toList.groupBy { case (pn, _) => pn.name }.foreach { case (pn, tuples) =>
      started.logger.info(s"${pn.value}:")
      tuples.foreach { case (_, cls) => started.logger.info(s"  $cls") }
    }

    Right(())
  }

  private def testsByCrossProject(started: Started, bloop: BuildServer): Iterator[(model.CrossProjectName, String)] = {
    val targets = BleepCommandRemote.buildTargets(started.buildPaths, projects)
    val result: bsp4j.ScalaTestClassesResult = bloop.buildTargetScalaTestClasses(new ScalaTestClassesParams(targets)).get()

    for {
      item <- result.getItems.iterator().asScala
      projectName <- BleepCommandRemote.projectFromBuildTarget(started)(item.getTarget).iterator
      cls <- item.getClasses.iterator.asScala
    } yield (projectName, cls)
  }
}
