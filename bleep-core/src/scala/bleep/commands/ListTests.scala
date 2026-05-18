package bleep
package commands

import bleep.bsp.BuildServer
import bleep.testing.TestTagFilter
import ch.epfl.scala.bsp4j
import ch.epfl.scala.bsp4j.ScalaTestClassesParams

import scala.annotation.nowarn
import scala.jdk.CollectionConverters.*

case class ListTests(projects: Array[model.CrossProjectName], outputMode: OutputMode) extends BleepBuildCommand {

  override def run(started: Started): Either[BleepException, Unit] =
    BspQuery.withServer(started) { server =>
      val all: List[(model.CrossProjectName, String)] = testsByCrossProject(started, server).toList

      def manifestFor(p: model.CrossProjectName): Map[String, Set[String]] =
        started.build.explodedProjects(p).testTags.value.view.mapValues(_.values.toSet).toMap

      def tagSuffix(p: model.CrossProjectName, cls: String): String = {
        val tags = TestTagFilter.tagsFor(cls, manifestFor(p))
        if (tags.isEmpty) "" else s" (${tags.toList.sorted.mkString(", ")})"
      }

      outputMode match {
        case OutputMode.Text =>
          all.groupBy { case (pn, _) => pn.name }.toList.sortBy(_._1.value).foreach { case (pn, tuples) =>
            started.logger.info(s"${pn.value}:")
            tuples.sortBy { case (_, cls) => cls }.foreach { case (cn, cls) => started.logger.info(s"  $cls${tagSuffix(cn, cls)}") }
          }
          warnAboutStaleManifests(started, all)
        case OutputMode.Json =>
          val grouped = all.groupBy(_._1).toList.sortBy(_._1.value).map { case (pn, tuples) =>
            ProjectTests(pn.value, tuples.map(_._2).sorted)
          }
          CommandResult.print(CommandResult.success(TestList(grouped)))
        case OutputMode.Raw =>
          all.groupBy { case (pn, _) => pn.name }.toList.sortBy(_._1.value).foreach { case (pn, tuples) =>
            println(s"${pn.value}:")
            tuples.sortBy { case (_, cls) => cls }.foreach { case (cn, cls) => println(s"  $cls${tagSuffix(cn, cls)}") }
          }
          warnAboutStaleManifests(started, all)
      }

      Right(())
    }

  /** Emit a warning for every `testTags` pattern in the build that matched no discovered suite. Drift warnings only — not fatal. */
  private def warnAboutStaleManifests(started: Started, discovered: List[(model.CrossProjectName, String)]): Unit = {
    val discoveredByProject: Map[model.CrossProjectName, Set[String]] =
      discovered.groupBy(_._1).view.mapValues(_.map(_._2).toSet).toMap
    projects.foreach { p =>
      val manifest = started.build.explodedProjects(p).testTags.value.view.mapValues(_.values.toSet).toMap
      if (manifest.nonEmpty) {
        val suites = discoveredByProject.getOrElse(p, Set.empty)
        val warnings = TestTagFilter.staleManifestEntries(manifest, suites)
        warnings.foreach(w => started.logger.warn(s"[${p.value}] $w"))
      }
    }
  }

  @nowarn("msg=buildTargetScalaTestClasses")
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
