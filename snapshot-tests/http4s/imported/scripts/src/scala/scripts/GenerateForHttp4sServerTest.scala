
package scripts

import bleep.{BleepCodegenScript, Commands, Started}

import java.nio.file.Files

object GenerateForHttp4sServerTest extends BleepCodegenScript("GenerateForHttp4sServerTest") {
  override def run(started: Started, commands: Commands, targets: List[Target], args: List[String]): Unit = {
    started.logger.error("This script is a placeholder! You'll need to replace the contents with code which actually generates the files you want")

    targets.foreach { target =>
      if (Set("http4s-server-test@js213", "http4s-server-test@js3").contains(target.project.value)) {
        val to = target.sources.resolve("sbt-buildinfo/BuildInfo.scala")
        started.logger.withContext(target.project).warn(s"Writing $to")
        val content = s"""|// $$COVERAGE-OFF$$
      |package org.http4s.server.test
      |
      |/** This object was generated by sbt-buildinfo. */
      |case object BuildInfo {
      |  /** The value is "<BLEEP_GIT>/snapshot-tests/http4s/sbt-build/server/js/src/test/resources". */
      |  val test_resourceDirectory = "<BLEEP_GIT>/snapshot-tests/http4s/sbt-build/server/js/src/test/resources"
      |  override val toString: String = {
      |    "test_resourceDirectory: %s".format(
      |      test_resourceDirectory
      |    )
      |  }
      |}
      |// $$COVERAGE-ON$$""".stripMargin
        Files.createDirectories(to.getParent)
        Files.writeString(to, content)
      }
    }



    targets.foreach { target =>
      if (Set("http4s-server-test@jvm213", "http4s-server-test@jvm3").contains(target.project.value)) {
        val to = target.sources.resolve("sbt-buildinfo/BuildInfo.scala")
        started.logger.withContext(target.project).warn(s"Writing $to")
        val content = s"""|// $$COVERAGE-OFF$$
      |package org.http4s.server.test
      |
      |/** This object was generated by sbt-buildinfo. */
      |case object BuildInfo {
      |  /** The value is "<BLEEP_GIT>/snapshot-tests/http4s/sbt-build/server/jvm/src/test/resources". */
      |  val test_resourceDirectory = "<BLEEP_GIT>/snapshot-tests/http4s/sbt-build/server/jvm/src/test/resources"
      |  override val toString: String = {
      |    "test_resourceDirectory: %s".format(
      |      test_resourceDirectory
      |    )
      |  }
      |}
      |// $$COVERAGE-ON$$""".stripMargin
        Files.createDirectories(to.getParent)
        Files.writeString(to, content)
      }
    }

  }
}