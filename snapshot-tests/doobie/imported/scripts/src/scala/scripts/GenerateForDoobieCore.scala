
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
      |  /** Current version of doobie (1.0-5d0957d-20230529T103009Z-SNAPSHOT). */
      |  val version = "1.0-5d0957d-20230529T103009Z-SNAPSHOT"
      |  /** Build date (Mon May 29 12:30:12 CEST 2023). */
      |  val date    = new java.util.Date(1685356212429L)
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
      |  /** Current version of doobie (1.0-5d0957d-20230529T102848Z-SNAPSHOT). */
      |  val version = "1.0-5d0957d-20230529T102848Z-SNAPSHOT"
      |  /** Build date (Mon May 29 12:28:52 CEST 2023). */
      |  val date    = new java.util.Date(1685356132602L)
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
      |  /** Current version of doobie (1.0-5d0957d-20230529T102924Z-SNAPSHOT). */
      |  val version = "1.0-5d0957d-20230529T102924Z-SNAPSHOT"
      |  /** Build date (Mon May 29 12:29:26 CEST 2023). */
      |  val date    = new java.util.Date(1685356166948L)
      |}""".stripMargin
        Files.createDirectories(to.getParent)
        Files.writeString(to, content)
      }
    }

  }
}