package bleep
package commands

import bleep.internal.{DoSourceGen, TransitiveProjects}
import bleep.logging.Logger
import bloop.rifle.BuildServer
import cats.data.NonEmptyList
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import ch.epfl.scala.bsp4j
import ch.epfl.scala.bsp4j.ScalaTestClassesParams

import java.util
import scala.jdk.CollectionConverters.*

case class Test(
    watch: Boolean,
    projects: Array[model.CrossProjectName],
    testSuitesOnly: Option[NonEmptyList[String]],
    testSuitesExclude: Option[NonEmptyList[String]],
    parallelism: Int
) extends BleepCommandRemote(watch)
    with BleepCommandRemote.OnlyChanged {

  override def watchableProjects(started: Started): TransitiveProjects =
    TransitiveProjects(started.build, projects)

  override def onlyChangedProjects(started: Started, isChanged: model.CrossProjectName => Boolean): BleepCommandRemote =
    copy(projects = watchableProjects(started).transitiveFilter(isChanged).direct)

  override def runWithServer(started: Started, bloop: BuildServer): Either[BleepException, Unit] = {
    val ioSuccess = for {
      _ <- IO.fromEither(DoSourceGen(started, bloop, watchableProjects(started)))
      // rough estimate to make it a bit more probable that we start to run tests before everything is compiled
      sortedProjects = projects.sortBy(name => started.build.resolvedDependsOn.get(name).fold(0)(_.size))
      testResults <-
        fs2.Stream
          .emits(sortedProjects)
          .covary[IO]
          .parEvalMap(parallelism) { projectName =>
            val target = BleepCommandRemote.buildTarget(started.buildPaths, projectName)

            val maybeTestParams = (testSuitesExclude, testSuitesOnly) match {
              case (None, None) =>
                IO.pure(Some(new bsp4j.TestParams(util.List.of(target))))
              case _ =>
                val params = new ScalaTestClassesParams(util.List.of(target))
                IO.fromCompletableFuture(IO.delay(bloop.buildTargetScalaTestClasses(params)))
                  .map(testSuiteResult => Test.testParamsWithFilter(testSuiteResult, testSuitesOnly, testSuitesExclude))
            }

            maybeTestParams
              .flatMap {
                case None             => IO.pure(None)
                case Some(testParams) => IO.fromCompletableFuture(IO(bloop.buildTargetTest(testParams))).map(Some.apply)
              }
              .attempt
              .map(res => (projectName, res))
          }
          .compile
          .toList
      res <- Test.printAndCheck(started.logger, testResults)
    } yield res

    if (ioSuccess.unsafeRunSync()) Right(()) else Left(Test.TestsFailed)
  }
}

object Test {
  def printAndCheck(logger: Logger, results: List[(model.CrossProjectName, Either[Throwable, Option[bsp4j.TestResult]])]): IO[Boolean] = IO {
    var success = true
    logger.info("---------------------------")
    logger.info("Bleep test project summary:")
    results.foreach {
      case (name, Right(None)) =>
        logger.warn(s"$name (no tests picked)")
      case (name, Right(Some(res))) =>
        res.getStatusCode match {
          case bsp4j.StatusCode.OK =>
            logger.info(s"✅ $name")
          case bsp4j.StatusCode.ERROR =>
            success = false
            logger.error(s"❌ $name")
          case bsp4j.StatusCode.CANCELLED =>
            success = false
            logger.warn(s"⚠️$name")
        }
      case (name, Left(th)) =>
        success = false
        logger.error(s"💣 $name", th)
    }
    success
  }

  object TestsFailed extends BleepException(s"Tests failed")

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
