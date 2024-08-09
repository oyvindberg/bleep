package bleep
package commands

import bleep.bsp.BspCommandFailed
import bleep.internal.{DoSourceGen, TransitiveProjects}
import bloop.rifle.BuildServer
import cats.data.NonEmptyList
import ch.epfl.scala.bsp4j

import java.util
import scala.jdk.CollectionConverters.*

case class Test(
    watch: Boolean,
    projects: Array[model.CrossProjectName],
    testSuitesOnly: Option[NonEmptyList[String]],
    testSuitesExclude: Option[NonEmptyList[String]]
) extends BleepCommandRemote(watch)
    with BleepCommandRemote.OnlyChanged {

  override def watchableProjects(started: Started): TransitiveProjects =
    TransitiveProjects(started.build, projects)

  override def onlyChangedProjects(started: Started, isChanged: model.CrossProjectName => Boolean): BleepCommandRemote =
    copy(projects = watchableProjects(started).transitiveFilter(isChanged).direct)

  override def runWithServer(started: Started, bloop: BuildServer): Either[BleepException, Unit] =
    DoSourceGen(started, bloop, watchableProjects(started)).flatMap { _ =>
      val allTargets = BleepCommandRemote.buildTargets(started.buildPaths, projects)

      val maybeTestParams: Either[BleepException, bsp4j.TestParams] =
        (testSuitesExclude, testSuitesOnly) match {
          case (None, None) =>
            Right(new bsp4j.TestParams(allTargets))
          case _ =>
            val allTestSuitesResult: bsp4j.ScalaTestClassesResult =
              bloop.buildTargetScalaTestClasses(new bsp4j.ScalaTestClassesParams(allTargets)).get()

            Test
              .testParamsWithFilter(allTestSuitesResult, testSuitesOnly, testSuitesExclude)
              .toRight(Test.NoTestSuitesFound)
        }

      maybeTestParams.flatMap { testParams =>
        bloop.buildTargetTest(testParams).get().getStatusCode match {
          case bsp4j.StatusCode.OK =>
            started.logger.info("Tests succeeded")
            Right(())
          case errorCode =>
            Left(new BspCommandFailed("tests", projects, BspCommandFailed.StatusCode(errorCode)))
        }
      }
    }
}

object Test {
  object NoTestSuitesFound extends BleepException(s"No tests found in projects")

  private implicit class MapSyntax[K, V](m: Map[K, List[V]]) {
    // use this to avoid asking bloop to run tests for projects we know we won't run tests from
    def keepNonEmpty: Map[K, NonEmptyList[V]] = m.flatMap { case (k, v) => NonEmptyList.fromList(v).map(k -> _) }
  }

  def testParamsWithFilter(
      allTestSuitesResult: bsp4j.ScalaTestClassesResult,
      testSuitesOnly: Option[NonEmptyList[String]],
      testSuitesExclude: Option[NonEmptyList[String]]
  ): Option[bsp4j.TestParams] = {

    val allTests: Map[bsp4j.BuildTargetIdentifier, NonEmptyList[String]] =
      allTestSuitesResult.getItems.asScala
        .map(x => (x.getTarget, x.getClasses.asScala.toList))
        .toMap
        .keepNonEmpty

    val afterTestOnly: Map[bsp4j.BuildTargetIdentifier, NonEmptyList[String]] =
      testSuitesOnly match {
        case Some(includes) =>
          allTests.map { case (target, testSuites) => (target, testSuites.filter(amongSuite(includes))) }.keepNonEmpty
        case None =>
          allTests
      }

    val afterExcludes: Map[bsp4j.BuildTargetIdentifier, NonEmptyList[String]] =
      testSuitesExclude match {
        case Some(excludes) =>
          afterTestOnly.map { case (target, testSuites) => (target, testSuites.filterNot(amongSuite(excludes))) }.keepNonEmpty
        case None =>
          afterTestOnly
      }

    if (afterExcludes.isEmpty) None
    else {
      val asBspTestSuites: util.List[bsp4j.ScalaTestSuiteSelection] =
        afterExcludes.valuesIterator
          .flatMap(_.iterator)
          .map(cls => new bsp4j.ScalaTestSuiteSelection(cls, List.empty[String].asJava))
          .toList
          .asJava
      val testParams = new bsp4j.TestParams(afterExcludes.keys.toList.asJava)
      testParams.setData(new bsp4j.ScalaTestSuites(asBspTestSuites, List.empty[String].asJava, List.empty[String].asJava))
      testParams.setDataKind(bsp4j.TestParamsDataKind.SCALA_TEST_SUITES_SELECTION)
      Some(testParams)
    }
  }

  def amongSuite(testSuites: NonEmptyList[String])(testSuite: String): Boolean =
    testSuites.exists(isSameSuite(testSuite))

  def isSameSuite(one: String)(two: String): Boolean =
    one.equalsIgnoreCase(two) || one.split('.').last.equalsIgnoreCase(two.split('.').last)
}
