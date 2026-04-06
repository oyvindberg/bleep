package bleep
package commands

import bleep.bsp.BuildServer
import ch.epfl.scala.bsp4j
import ch.epfl.scala.bsp4j.ScalaTestClassesParams

import scala.jdk.CollectionConverters.*

case class ListTests(projects: Array[model.CrossProjectName], outputMode: OutputMode) extends BleepBuildCommand {

  override def run(started: Started): Either[BleepException, Unit] =
    BspQuery.withServer(started) { server =>
      val all: Iterator[(model.CrossProjectName, String)] = testsByCrossProject(started, server)

      outputMode match {
        case OutputMode.Text =>
          all.toList.groupBy { case (pn, _) => pn.name }.foreach { case (pn, tuples) =>
            started.logger.info(s"${pn.value}:")
            tuples.foreach { case (_, cls) => started.logger.info(s"  $cls") }
          }
        case OutputMode.Json =>
          val grouped = all.toList.groupBy(_._1).toList.sortBy(_._1.value).map { case (pn, tuples) =>
            ProjectTests(pn.value, tuples.map(_._2))
          }
          CommandResult.print(CommandResult.success(TestList(grouped)))
      }

      Right(())
    }

  private def testsByCrossProject(started: Started, server: BuildServer): Iterator[(model.CrossProjectName, String)] = {
    val targets = BspQuery.buildTargets(started.buildPaths, projects)
    val result: bsp4j.ScalaTestClassesResult = server.buildTargetScalaTestClasses(new ScalaTestClassesParams(targets)).get()

    for {
      item <- result.getItems.iterator().asScala
      projectName <- BspQuery.projectFromBuildTarget(started)(item.getTarget).iterator
      cls <- item.getClasses.iterator.asScala
    } yield (projectName, cls)
  }
}
