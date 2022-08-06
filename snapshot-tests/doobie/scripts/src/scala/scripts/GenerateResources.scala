
package scripts

import bleep.{BleepScript, Commands, Started}

import java.nio.file.Files

object GenerateResources extends BleepScript("GenerateResources") {
  def run(started: Started, commands: Commands, args: List[String]): Unit = {
    started.logger.error("This script is a placeholder! You'll need to replace the contents with code which actually generates the files you want")

    
    Vector(
      bleep.model.CrossProjectName(bleep.model.ProjectName("doobie-core"), Some(bleep.model.CrossId("jvm212")))).foreach { crossName =>
      val to = started.buildPaths.generatedSourcesDir(crossName).resolve("scala/doobie/buildinfo.scala")
      started.logger.withContext(crossName).warn(s"Writing $to")
      val content = s"""|package doobie
      |
      |/** Auto-generated build information. */
      |object buildinfo {
      |  /** Current version of doobie (1.0-5d0957d-SNAPSHOT). */
      |  val version = "1.0-5d0957d-SNAPSHOT"
      |  /** Build date (Wed Jul 27 01:43:11 CEST 2022). */
      |  val date    = new java.util.Date(1658878991026L)
      |}""".stripMargin
      Files.createDirectories(to.getParent)
      Files.writeString(to, content)
    }



    Vector(
      bleep.model.CrossProjectName(bleep.model.ProjectName("doobie-core"), Some(bleep.model.CrossId("jvm213")))).foreach { crossName =>
      val to = started.buildPaths.generatedSourcesDir(crossName).resolve("scala/doobie/buildinfo.scala")
      started.logger.withContext(crossName).warn(s"Writing $to")
      val content = s"""|package doobie
      |
      |/** Auto-generated build information. */
      |object buildinfo {
      |  /** Current version of doobie (1.0-5d0957d-SNAPSHOT). */
      |  val version = "1.0-5d0957d-SNAPSHOT"
      |  /** Build date (Wed Jul 27 01:42:56 CEST 2022). */
      |  val date    = new java.util.Date(1658878976475L)
      |}""".stripMargin
      Files.createDirectories(to.getParent)
      Files.writeString(to, content)
    }



    Vector(
      bleep.model.CrossProjectName(bleep.model.ProjectName("doobie-core"), Some(bleep.model.CrossId("jvm3")))).foreach { crossName =>
      val to = started.buildPaths.generatedSourcesDir(crossName).resolve("scala/doobie/buildinfo.scala")
      started.logger.withContext(crossName).warn(s"Writing $to")
      val content = s"""|package doobie
      |
      |/** Auto-generated build information. */
      |object buildinfo {
      |  /** Current version of doobie (1.0-5d0957d-SNAPSHOT). */
      |  val version = "1.0-5d0957d-SNAPSHOT"
      |  /** Build date (Mon Aug 01 00:08:15 CEST 2022). */
      |  val date    = new java.util.Date(1659305295056L)
      |}""".stripMargin
      Files.createDirectories(to.getParent)
      Files.writeString(to, content)
    }

  }
}