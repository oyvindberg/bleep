
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
      |  /** Current version of doobie (1.0-5d0957d-20221105T001220Z-SNAPSHOT). */
      |  val version = "1.0-5d0957d-20221105T001220Z-SNAPSHOT"
      |  /** Build date (Sat Nov 05 01:12:23 CET 2022). */
      |  val date    = new java.util.Date(1667607143241L)
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
      |  /** Current version of doobie (1.0-5d0957d-20221105T001152Z-SNAPSHOT). */
      |  val version = "1.0-5d0957d-20221105T001152Z-SNAPSHOT"
      |  /** Build date (Sat Nov 05 01:11:57 CET 2022). */
      |  val date    = new java.util.Date(1667607117359L)
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
      |  /** Current version of doobie (1.0-5d0957d-20221105T001246Z-SNAPSHOT). */
      |  val version = "1.0-5d0957d-20221105T001246Z-SNAPSHOT"
      |  /** Build date (Sat Nov 05 01:12:48 CET 2022). */
      |  val date    = new java.util.Date(1667607168992L)
      |}""".stripMargin
      Files.createDirectories(to.getParent)
      Files.writeString(to, content)
    }

  }
}