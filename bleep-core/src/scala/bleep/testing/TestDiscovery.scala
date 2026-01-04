package bleep.testing

import bleep.model.CrossProjectName
import ch.epfl.scala.bsp4j
import bloop.rifle.BuildServer

import scala.jdk.CollectionConverters._

/** A discovered test suite ready to be executed */
case class DiscoveredSuite(
    project: CrossProjectName,
    buildTarget: bsp4j.BuildTargetIdentifier,
    className: String,
    framework: String
)

/** Discovers test suites in compiled projects using BSP ScalaTestClasses.
  *
  * This queries bloop for available test classes after compilation completes.
  */
object TestDiscovery {

  /** Discover all test suites in the given projects.
    *
    * @param bloop
    *   The BSP build server
    * @param projects
    *   Projects to discover tests in (should be test projects)
    * @param buildTargetFor
    *   Function to get BSP build target for a project
    * @return
    *   List of discovered test suites
    */
  def discoverAll(
      bloop: BuildServer,
      projects: Set[CrossProjectName],
      buildTargetFor: CrossProjectName => bsp4j.BuildTargetIdentifier
  ): List[DiscoveredSuite] = {
    if (projects.isEmpty) return Nil

    val targets = projects.map(buildTargetFor).toList.asJava
    val params = new bsp4j.ScalaTestClassesParams(targets)

    val result: bsp4j.ScalaTestClassesResult =
      bloop.buildTargetScalaTestClasses(params).get()

    result.getItems.asScala.toList.flatMap { item =>
      val targetId = item.getTarget
      // Find which project this target belongs to
      val maybeProject = projects.find(p => buildTargetFor(p).getUri == targetId.getUri)

      maybeProject match {
        case Some(project) =>
          // Get framework info if available
          val framework = Option(item.getFramework).getOrElse("unknown")

          item.getClasses.asScala.toList.map { className =>
            DiscoveredSuite(
              project = project,
              buildTarget = targetId,
              className = className,
              framework = framework
            )
          }
        case None =>
          // Target doesn't match any of our projects - skip
          Nil
      }
    }
  }

  /** Discover test suites for a single project.
    *
    * @param bloop
    *   The BSP build server
    * @param project
    *   Project to discover tests in
    * @param buildTarget
    *   The BSP build target for the project
    * @return
    *   List of discovered test suites
    */
  def discoverForProject(
      bloop: BuildServer,
      project: CrossProjectName,
      buildTarget: bsp4j.BuildTargetIdentifier
  ): List[DiscoveredSuite] = {
    val params = new bsp4j.ScalaTestClassesParams(List(buildTarget).asJava)

    val result: bsp4j.ScalaTestClassesResult =
      bloop.buildTargetScalaTestClasses(params).get()

    result.getItems.asScala.toList.flatMap { item =>
      val framework = Option(item.getFramework).getOrElse("unknown")

      item.getClasses.asScala.toList.map { className =>
        DiscoveredSuite(
          project = project,
          buildTarget = buildTarget,
          className = className,
          framework = framework
        )
      }
    }
  }

  /** Group discovered suites by project */
  def groupByProject(suites: List[DiscoveredSuite]): Map[CrossProjectName, List[DiscoveredSuite]] =
    suites.groupBy(_.project)

  /** Group discovered suites by framework */
  def groupByFramework(suites: List[DiscoveredSuite]): Map[String, List[DiscoveredSuite]] =
    suites.groupBy(_.framework)
}
