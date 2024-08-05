package bleep
package commands

import bleep.bsp.BspCommandFailed
import bleep.internal.{DoSourceGen, TransitiveProjects}
import ch.epfl.scala.bsp4j
import scala.jdk.CollectionConverters.*

import bloop.rifle.BuildServer
import cats.data.NonEmptyList

case class Test(watch: Boolean, projects: Array[model.CrossProjectName], testSuites: Option[NonEmptyList[String]])
    extends BleepCommandRemote(watch)
    with BleepCommandRemote.OnlyChanged {

  private def intersectTestSuites(fromBuild: List[String], fromCli: List[String]): List[String] =
    fromBuild.filter { fullyQualifiedCls =>
      fromCli.exists { fromCliCls =>
        val fromBuildFqn = fullyQualifiedCls.split('.')
        val fromCliFqn = fromCliCls.split('.')
        fromBuildFqn.sameElements(fromCliFqn) || fromBuildFqn.last == fromCliFqn.last
      }
    }

  override def watchableProjects(started: Started): TransitiveProjects =
    TransitiveProjects(started.build, projects)

  override def onlyChangedProjects(started: Started, isChanged: model.CrossProjectName => Boolean): BleepCommandRemote =
    copy(projects = watchableProjects(started).transitiveFilter(isChanged).direct)

  override def runWithServer(started: Started, bloop: BuildServer): Either[BleepException, Unit] =
    DoSourceGen(started, bloop, watchableProjects(started)).flatMap { _ =>
      val targets = BleepCommandRemote.buildTargets(started.buildPaths, projects)
      val tests = bloop.buildTargetScalaTestClasses(new bsp4j.ScalaTestClassesParams(targets)).get()
      val testClasses: List[String] = tests.getItems().asScala.flatMap(_.getClasses.asScala).toList

      val testParams = new bsp4j.TestParams(targets)

      testSuites.foreach { classes =>
        val testClassesData = new bsp4j.ScalaTestSuites(
          intersectTestSuites(testClasses, classes.toList)
            .map(cls => new bsp4j.ScalaTestSuiteSelection(cls, List.empty[String].asJava))
            .asJava,
          List.empty[String].asJava,
          List.empty[String].asJava
        )
        testParams.setData(testClassesData)
        testParams.setDataKind(bsp4j.TestParamsDataKind.SCALA_TEST_SUITES_SELECTION)
      }

      val result = bloop.buildTargetTest(testParams).get()

      result.getStatusCode match {
        case bsp4j.StatusCode.OK =>
          started.logger.info("Tests succeeded")
          Right(())
        case errorCode =>
          Left(new BspCommandFailed("tests", projects, BspCommandFailed.StatusCode(errorCode)))
      }
    }
}
