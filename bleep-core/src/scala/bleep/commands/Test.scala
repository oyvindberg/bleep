package bleep
package commands

import bleep.bsp.BspCommandFailed
import bleep.internal.{DoSourceGen, TransitiveProjects}
import ch.epfl.scala.bsp4j
import scala.jdk.CollectionConverters.*

import bloop.rifle.BuildServer
import cats.data.NonEmptyList
import bleep.BleepException.TestSuitesNotFound

case class Test(
    watch: Boolean,
    projects: Array[model.CrossProjectName],
    testSuitesOnly: Option[NonEmptyList[String]],
    testSuitesExclude: Option[NonEmptyList[String]]
) extends BleepCommandRemote(watch)
    with BleepCommandRemote.OnlyChanged {

  private def isSameSuite(fromBuild: String, fromCli: String) = {
    val fromBuildFqn = fromBuild.split('.')
    val fromCliFqn = fromCli.split('.')
    fromBuildFqn.sameElements(fromCliFqn) || fromBuildFqn.last == fromCliFqn.last
  }

  private def intersectTestSuites(fromBuild: List[String], fromCli: List[String]): List[String] =
    fromBuild.filter { fullyQualifiedCls =>
      fromCli.exists { fromCliCls =>
        isSameSuite(fullyQualifiedCls, fromCliCls)
      }
    }

  private def runTests(started: Started, bloop: BuildServer, testParams: bsp4j.TestParams) = {
    val result = bloop.buildTargetTest(testParams).get()

    result.getStatusCode match {
      case bsp4j.StatusCode.OK =>
        started.logger.info("Tests succeeded")
        Right(())
      case errorCode =>
        Left(new BspCommandFailed("tests", projects, BspCommandFailed.StatusCode(errorCode)))
    }
  }

  private def runWithFilters(started: Started, bloop: BuildServer, targets: java.util.List[bsp4j.BuildTargetIdentifier]) = {
    val tests = bloop.buildTargetScalaTestClasses(new bsp4j.ScalaTestClassesParams(targets)).get()
    val testClassesUnfiltered: List[String] = tests.getItems().asScala.flatMap(_.getClasses.asScala).toList

    val testClasses = testSuitesExclude.map { exclude =>
      val filtered = testClassesUnfiltered
        .filter(fromBuildCls => exclude.toList.find(fromCliCls => isSameSuite(fromBuildCls, fromCliCls)).isDefined)
      if (filtered.nonEmpty) testClassesUnfiltered.diff(filtered) else List.empty
    }

    val testParams = new bsp4j.TestParams(targets)

    val hasSuites: Boolean =
      testClasses match {
        case None =>
          testSuitesOnly
            .map { classes =>
              val testClassesData = new bsp4j.ScalaTestSuites(
                intersectTestSuites(testClassesUnfiltered, classes.toList)
                  .map(cls => new bsp4j.ScalaTestSuiteSelection(cls, List.empty[String].asJava))
                  .asJava,
                List.empty[String].asJava,
                List.empty[String].asJava
              )
              testParams.setData(testClassesData)
              testParams.setDataKind(bsp4j.TestParamsDataKind.SCALA_TEST_SUITES_SELECTION)

              !testClassesData.getSuites().isEmpty
            }
            .getOrElse(true)
        case Some(value) =>
          if (value.isEmpty) false
          else {
            val testClassesData = new bsp4j.ScalaTestSuites(
              value.map(suite => new bsp4j.ScalaTestSuiteSelection(suite, List.empty[String].asJava)).asJava,
              List.empty[String].asJava,
              List.empty[String].asJava
            )
            testParams.setData(testClassesData)
            testParams.setDataKind(bsp4j.TestParamsDataKind.SCALA_TEST_SUITES_SELECTION)
            true
          }
      }

    if (hasSuites) {
      runTests(started, bloop, testParams)
    } else {
      Left(new TestSuitesNotFound(testSuitesOnly.map(_.toList).getOrElse(List.empty[String])))
    }
  }

  override def watchableProjects(started: Started): TransitiveProjects =
    TransitiveProjects(started.build, projects)

  override def onlyChangedProjects(started: Started, isChanged: model.CrossProjectName => Boolean): BleepCommandRemote =
    copy(projects = watchableProjects(started).transitiveFilter(isChanged).direct)

  override def runWithServer(started: Started, bloop: BuildServer): Either[BleepException, Unit] =
    DoSourceGen(started, bloop, watchableProjects(started)).flatMap { _ =>
      val targets = BleepCommandRemote.buildTargets(started.buildPaths, projects)
      (testSuitesExclude, testSuitesOnly) match {
        case (None, None) =>
          runTests(started, bloop, new bsp4j.TestParams(targets))
        case _ =>
          runWithFilters(started, bloop, targets)
      }
    }
}
