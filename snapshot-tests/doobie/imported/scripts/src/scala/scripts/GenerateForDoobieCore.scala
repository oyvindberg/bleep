
package scripts

import bleep.{BleepCodegenScript, Commands, Started}

import java.nio.file.Files

object GenerateForDoobieCore extends BleepCodegenScript("GenerateForDoobieCore") {
  override def run(started: Started, commands: Commands, targets: List[Target], args: List[String]): Unit = {
    started.logger.error("This script is a placeholder! You'll need to replace the contents with code which actually generates the files you want")

    targets.foreach { target =>
      if (Set("doobie-core@jvm3").contains(target.project.value)) {
        val to = target.sources.resolve("scala/doobie/buildinfo.scala")
        started.logger.withContext(target.project).warn(s"Writing $to")
        val content = s"""|package doobie
      |
      |/** Auto-generated build information. */
      |object buildinfo {
      |  /** Current version of doobie (1.0-e3adc5d-20250621T223125Z-SNAPSHOT). */
      |  val version = "1.0-e3adc5d-20250621T223125Z-SNAPSHOT"
      |  /** Build date (Sun Jun 22 00:31:29 CEST 2025). */
      |  val date    = new java.util.Date(1750545089647L)
      |}""".stripMargin
        Files.createDirectories(to.getParent)
        Files.writeString(to, content)
      }
    }



    targets.foreach { target =>
      if (Set("doobie-core@jvm213").contains(target.project.value)) {
        val to = target.sources.resolve("scala/doobie/buildinfo.scala")
        started.logger.withContext(target.project).warn(s"Writing $to")
        val content = s"""|package doobie
      |
      |/** Auto-generated build information. */
      |object buildinfo {
      |  /** Current version of doobie (1.0-e3adc5d-20250621T223051Z-SNAPSHOT). */
      |  val version = "1.0-e3adc5d-20250621T223051Z-SNAPSHOT"
      |  /** Build date (Sun Jun 22 00:30:56 CEST 2025). */
      |  val date    = new java.util.Date(1750545056040L)
      |}""".stripMargin
        Files.createDirectories(to.getParent)
        Files.writeString(to, content)
      }
    }



    targets.foreach { target =>
      if (Set("doobie-core@jvm212").contains(target.project.value)) {
        val to = target.sources.resolve("scala/doobie/buildinfo.scala")
        started.logger.withContext(target.project).warn(s"Writing $to")
        val content = s"""|package doobie
      |
      |/** Auto-generated build information. */
      |object buildinfo {
      |  /** Current version of doobie (1.0-e3adc5d-20250621T223107Z-SNAPSHOT). */
      |  val version = "1.0-e3adc5d-20250621T223107Z-SNAPSHOT"
      |  /** Build date (Sun Jun 22 00:31:11 CEST 2025). */
      |  val date    = new java.util.Date(1750545071456L)
      |}""".stripMargin
        Files.createDirectories(to.getParent)
        Files.writeString(to, content)
      }
    }

  }
}