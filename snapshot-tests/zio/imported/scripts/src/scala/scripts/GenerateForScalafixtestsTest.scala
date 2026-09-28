
package scripts

import bleep.{BleepCodegenScript, Commands, Started}

import java.nio.file.Files

object GenerateForScalafixtestsTest extends BleepCodegenScript("GenerateForScalafixtestsTest") {
  override def run(started: Started, commands: Commands, targets: List[Target], args: List[String]): Unit = {
    started.logger.error("This script is a placeholder! You'll need to replace the contents with code which actually generates the files you want")

    targets.foreach { target =>
      if (Set(s"""|scalafixtests-test""".stripMargin).contains(target.project.value)) {
        val to = target.resources.resolve(s"""|scalafix-testkit.properties""".stripMargin)
        started.logger.withContext("project", target.project.value).warn(s"Writing $to")
        val content = s"""|#Input data for scalafix testkit
      |#Sun Sep 27 16:45:14 CEST 2026
      |inputClasspath=<BLEEP_GIT>/snapshot-tests/zio/sbt-build/scalafix/input/target/scala-2.13/classes\\:<COURSIER>/https/repo1.maven.org/maven2/org/scala-lang/scala-library/2.13.18/scala-library-2.13.18.jar\\:<COURSIER>/https/repo1.maven.org/maven2/dev/zio/zio_2.13/1.0.18/zio_2.13-1.0.18.jar\\:<COURSIER>/https/repo1.maven.org/maven2/dev/zio/zio-streams_2.13/1.0.18/zio-streams_2.13-1.0.18.jar\\:<COURSIER>/https/repo1.maven.org/maven2/dev/zio/zio-test_2.13/1.0.18/zio-test_2.13-1.0.18.jar\\:<COURSIER>/https/repo1.maven.org/maven2/dev/zio/zio-stacktracer_2.13/1.0.18/zio-stacktracer_2.13-1.0.18.jar\\:<COURSIER>/https/repo1.maven.org/maven2/dev/zio/izumi-reflect_2.13/2.2.5/izumi-reflect_2.13-2.2.5.jar\\:<COURSIER>/https/repo1.maven.org/maven2/org/portable-scala/portable-scala-reflect_2.13/1.1.2/portable-scala-reflect_2.13-1.1.2.jar\\:<COURSIER>/https/repo1.maven.org/maven2/dev/zio/izumi-reflect-thirdparty-boopickle-shaded_2.13/2.2.5/izumi-reflect-thirdparty-boopickle-shaded_2.13-2.2.5.jar
      |inputSourceDirectories=<BLEEP_GIT>/snapshot-tests/zio/sbt-build/scalafix/input/src/main/scala
      |outputSourceDirectories=<BLEEP_GIT>/snapshot-tests/zio/sbt-build/scalafix/output/src/main/scala
      |scalaVersion=2.13.18
      |scalacOptions=-Yrangepos|-P\\:semanticdb\\:synthetics\\:on
      |sourceroot=<BLEEP_GIT>/snapshot-tests/zio/sbt-build
      |""".stripMargin
        Files.createDirectories(to.getParent)
        Files.writeString(to, content)
      }
    }

  }
}