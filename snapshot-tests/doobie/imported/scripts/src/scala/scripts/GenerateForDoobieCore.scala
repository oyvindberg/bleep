
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
      |  /** Current version of doobie (1.0-5d0957d-20221105T001246Z-SNAPSHOT). */
      |  val version = "1.0-5d0957d-20221105T001246Z-SNAPSHOT"
      |  /** Build date (Sat Nov 05 01:12:48 CET 2022). */
      |  val date    = new java.util.Date(1667607168992L)
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
      |  /** Current version of doobie (1.0-5d0957d-20221105T001152Z-SNAPSHOT). */
      |  val version = "1.0-5d0957d-20221105T001152Z-SNAPSHOT"
      |  /** Build date (Sat Nov 05 01:11:57 CET 2022). */
      |  val date    = new java.util.Date(1667607117359L)
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
      |  /** Current version of doobie (1.0-5d0957d-20221105T001220Z-SNAPSHOT). */
      |  val version = "1.0-5d0957d-20221105T001220Z-SNAPSHOT"
      |  /** Build date (Sat Nov 05 01:12:23 CET 2022). */
      |  val date    = new java.util.Date(1667607143241L)
      |}""".stripMargin
        Files.createDirectories(to.getParent)
        Files.writeString(to, content)
      }
    }

  }
}