package scripts

import java.nio.file.Files

object GenerateResources extends App {
  bleep.bootstrap.forScript("GenerateResources") { started =>
    started.logger.error("This script is a placeholder! You'll need to replace the contents with code which actually generates the files you want")

    Vector(bleep.model.CrossProjectName(bleep.model.ProjectName("core"), Some(bleep.model.CrossId("jvm213")))).foreach { crossName =>
      val projectPaths = started.projectPaths(crossName)
      val to = projectPaths.generatedSourcesDir.resolve("scala/doobie/buildinfo.scala")
      started.logger.withContext(crossName).warn(s"Writing $to")
      val content = s"""|package doobie
      |
      |/** Auto-generated build information. */
      |object buildinfo {
      |  /** Current version of doobie (1.0-5d0957d-20220320T220343Z-SNAPSHOT). */
      |  val version = "1.0-5d0957d-20220320T220343Z-SNAPSHOT"
      |  /** Build date (Sun Mar 20 23:03:43 CET 2022). */
      |  val date    = new java.util.Date(1647813823805L)
      |}""".stripMargin
      Files.createDirectories(to.getParent)
      Files.writeString(to, content)
    }

    Vector(bleep.model.CrossProjectName(bleep.model.ProjectName("core"), Some(bleep.model.CrossId("jvm212")))).foreach { crossName =>
      val projectPaths = started.projectPaths(crossName)
      val to = projectPaths.generatedSourcesDir.resolve("scala/doobie/buildinfo.scala")
      started.logger.withContext(crossName).warn(s"Writing $to")
      val content = s"""|package doobie
      |
      |/** Auto-generated build information. */
      |object buildinfo {
      |  /** Current version of doobie (1.0-5d0957d-20220320T220332Z-SNAPSHOT). */
      |  val version = "1.0-5d0957d-20220320T220332Z-SNAPSHOT"
      |  /** Build date (Sun Mar 20 23:03:32 CET 2022). */
      |  val date    = new java.util.Date(1647813812836L)
      |}""".stripMargin
      Files.createDirectories(to.getParent)
      Files.writeString(to, content)
    }

    Vector(bleep.model.CrossProjectName(bleep.model.ProjectName("core"), Some(bleep.model.CrossId("jvm3")))).foreach { crossName =>
      val projectPaths = started.projectPaths(crossName)
      val to = projectPaths.generatedSourcesDir.resolve("scala/doobie/buildinfo.scala")
      started.logger.withContext(crossName).warn(s"Writing $to")
      val content = s"""|package doobie
      |
      |/** Auto-generated build information. */
      |object buildinfo {
      |  /** Current version of doobie (1.0-5d0957d-20220320T220350Z-SNAPSHOT). */
      |  val version = "1.0-5d0957d-20220320T220350Z-SNAPSHOT"
      |  /** Build date (Sun Mar 20 23:03:50 CET 2022). */
      |  val date    = new java.util.Date(1647813830601L)
      |}""".stripMargin
      Files.createDirectories(to.getParent)
      Files.writeString(to, content)
    }

  }
}
