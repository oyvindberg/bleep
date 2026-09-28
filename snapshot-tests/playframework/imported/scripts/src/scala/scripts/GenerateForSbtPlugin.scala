
package scripts

import bleep.{BleepCodegenScript, Commands, Started}

import java.nio.file.Files

object GenerateForSbtPlugin extends BleepCodegenScript("GenerateForSbtPlugin") {
  override def run(started: Started, commands: Commands, targets: List[Target], args: List[String]): Unit = {
    started.logger.error("This script is a placeholder! You'll need to replace the contents with code which actually generates the files you want")

    targets.foreach { target =>
      if (Set(s"""|sbt-plugin""".stripMargin).contains(target.project.value)) {
        val to = target.sources.resolve(s"""|PlayVersion.scala""".stripMargin)
        started.logger.withContext("project", target.project.value).warn(s"Writing $to")
        val content = s"""|package play.core
      |
      |object PlayVersion {
      |  val current = "3.1.0-M3-SNAPSHOT"
      |  val scalaVersion = "2.13.16"
      |  val sbtVersion = "1.11.4"
      |  val pekkoVersion = "1.0.3"
      |  val pekkoHttpVersion = "1.0.1"
      |}
      |""".stripMargin
        Files.createDirectories(to.getParent)
        Files.writeString(to, content)
      }
    }



    targets.foreach { target =>
      if (Set(s"""|sbt-plugin""".stripMargin).contains(target.project.value)) {
        val to = target.resources.resolve(s"""|sbt/sbt.autoplugins""".stripMargin)
        started.logger.withContext("project", target.project.value).warn(s"Writing $to")
        val content = s"""|play.sbt.Play
      |play.sbt.PlayFilters
      |play.sbt.PlayJava
      |play.sbt.PlayLayoutPlugin
      |play.sbt.PlayLogback
      |play.sbt.PlayMinimalJava
      |play.sbt.PlayNettyServer
      |play.sbt.PlayPekkoHttp2Support
      |play.sbt.PlayPekkoHttpServer
      |play.sbt.PlayScala
      |play.sbt.PlayService
      |play.sbt.PlayWeb
      |play.sbt.routes.RoutesCompiler
      |""".stripMargin
        Files.createDirectories(to.getParent)
        Files.writeString(to, content)
      }
    }

  }
}