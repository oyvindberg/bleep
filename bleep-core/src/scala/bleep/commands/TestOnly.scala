package bleep
package commands

import bleep.bsp.BspCommandFailed
import bleep.internal.{DoSourceGen, TransitiveProjects}
import bloop.rifle.BuildServer
import ch.epfl.scala.bsp4j
import ch.epfl.scala.bsp4j.{ScalaTestParams, TestParams}

import scala.jdk.CollectionConverters.*

case class TestOnly(watch: Boolean, tests: Array[ListTests.Test]) extends BleepCommandRemote(watch) with BleepCommandRemote.OnlyChanged {

  override def watchableProjects(started: Started): TransitiveProjects =
    TransitiveProjects(started.build, tests.map(_.project))

  override def onlyChangedProjects(started: Started, isChanged: model.CrossProjectName => Boolean): BleepCommandRemote = {
    val changedProjects = watchableProjects(started).transitiveFilter(isChanged).direct.toSet
    copy(tests = tests.filter(test => changedProjects(test.project)))
  }

  override def runWithServer(started: Started, bloop: BuildServer): Either[BleepException, Unit] =
    DoSourceGen(started, bloop, watchableProjects(started)).flatMap { _ =>
      val distinctProjects = tests.map(_.project).distinct
      val targets = BleepCommandRemote.buildTargets(started.buildPaths, distinctProjects)
      val params = new TestParams(targets)
      params.setDataKind("scala-test")
      params.setData {
        val items = tests.groupBy(_.project).map { case (project, tests) =>
          val testClasses = tests.map(_.cls).toList.asJava
          new bsp4j.ScalaTestClassesItem(BleepCommandRemote.buildTarget(started.buildPaths, project), testClasses)
        }
        val testParams = new ScalaTestParams()
        testParams.setTestClasses(items.toList.asJava)
        testParams
      }
      val result = bloop.buildTargetTest(params).get()

      result.getStatusCode match {
        case bsp4j.StatusCode.OK =>
          started.logger.info("Tests succeeded")
          Right(())
        case errorCode =>
          Left(new BspCommandFailed("tests", distinctProjects, BspCommandFailed.StatusCode(errorCode)))
      }
    }
}
