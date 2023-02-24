
package scripts

import bleep.{BleepCodegenScript, Commands, Started}

import java.nio.file.Files

object GenerateForBloopBackend extends BleepCodegenScript("GenerateForBloopBackend") {
  override def run(started: Started, commands: Commands, targets: List[Target], args: List[String]): Unit = {
    started.logger.error("This script is a placeholder! You'll need to replace the contents with code which actually generates the files you want")

    targets.foreach { target =>
      if (Set("bloop-backend").contains(target.project.value)) {
        val to = target.sources.resolve("sbt-buildinfo/BloopScalaInfo.scala")
        started.logger.withContext(target.project).warn(s"Writing $to")
        val content = s"""|package bloop.internal.build
      |
      |import scala.Predef._
      |
      |/** This object was generated by sbt-buildinfo. */
      |case object BloopScalaInfo {
      |  /** The value is "2.12.15". */
      |  val scalaVersion: String = "2.12.15"
      |  /** The value is "org.scala-lang". */
      |  val scalaOrganization: String = "org.scala-lang"
      |  /** The value is scala.collection.Seq(new java.io.File("<HOME>/.sbt/boot/scala-2.12.15/lib/scala-library.jar"), new java.io.File("<HOME>/.sbt/boot/scala-2.12.15/lib/scala-compiler.jar"), new java.io.File("<HOME>/.sbt/boot/scala-2.12.15/lib/scala-reflect.jar"), new java.io.File("<HOME>/.sbt/boot/scala-2.12.15/lib/scala-xml_2.12-1.0.6.jar")). */
      |  val scalaJars = scala.collection.Seq(new java.io.File("<HOME>/.sbt/boot/scala-2.12.15/lib/scala-library.jar"), new java.io.File("<HOME>/.sbt/boot/scala-2.12.15/lib/scala-compiler.jar"), new java.io.File("<HOME>/.sbt/boot/scala-2.12.15/lib/scala-reflect.jar"), new java.io.File("<HOME>/.sbt/boot/scala-2.12.15/lib/scala-xml_2.12-1.0.6.jar"))
      |  override val toString: String = {
      |    "scalaVersion: %s, scalaOrganization: %s, scalaJars: %s" format (
      |      scalaVersion, scalaOrganization, scalaJars
      |    )
      |  }
      |}""".stripMargin
        Files.createDirectories(to.getParent)
        Files.writeString(to, content)
      }
    }

  }
}