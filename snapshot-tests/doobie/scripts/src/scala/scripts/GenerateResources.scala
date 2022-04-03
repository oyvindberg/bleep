
package scripts

import java.nio.file.Files

object GenerateResources extends App {
  bleep.bootstrap.forScript("GenerateResources") { started =>
    started.logger.error("This script is a placeholder! You'll need to replace the contents with code which actually generates the files you want")

    
    Vector(
      bleep.model.CrossProjectName(bleep.model.ProjectName("core"), Some(bleep.model.CrossId("jvm212")))).foreach { crossName =>
      val projectPaths = started.projectPaths(crossName) 
      val to = projectPaths.generatedSourcesDir.resolve("scala/doobie/buildinfo.scala")
      started.logger.withContext(crossName).warn(s"Writing $to")
      val content = s"""|package doobie
      |
      |/** Auto-generated build information. */
      |object buildinfo {
      |  /** Current version of doobie (1.0-5d0957d-SNAPSHOT). */
      |  val version = "1.0-5d0957d-SNAPSHOT"
      |  /** Build date (Mon Apr 04 23:15:42 CEST 2022). */
      |  val date    = new java.util.Date(1649106942729L)
      |}""".stripMargin
      Files.createDirectories(to.getParent)
      Files.writeString(to, content)
    }



    Vector(
      bleep.model.CrossProjectName(bleep.model.ProjectName("core"), Some(bleep.model.CrossId("jvm213")))).foreach { crossName =>
      val projectPaths = started.projectPaths(crossName) 
      val to = projectPaths.generatedSourcesDir.resolve("scala/doobie/buildinfo.scala")
      started.logger.withContext(crossName).warn(s"Writing $to")
      val content = s"""|package doobie
      |
      |/** Auto-generated build information. */
      |object buildinfo {
      |  /** Current version of doobie (1.0-5d0957d-SNAPSHOT). */
      |  val version = "1.0-5d0957d-SNAPSHOT"
      |  /** Build date (Mon Apr 04 23:15:49 CEST 2022). */
      |  val date    = new java.util.Date(1649106949161L)
      |}""".stripMargin
      Files.createDirectories(to.getParent)
      Files.writeString(to, content)
    }



    Vector(
      bleep.model.CrossProjectName(bleep.model.ProjectName("core"), Some(bleep.model.CrossId("jvm3")))).foreach { crossName =>
      val projectPaths = started.projectPaths(crossName) 
      val to = projectPaths.generatedSourcesDir.resolve("scala/doobie/buildinfo.scala")
      started.logger.withContext(crossName).warn(s"Writing $to")
      val content = s"""|package doobie
      |
      |/** Auto-generated build information. */
      |object buildinfo {
      |  /** Current version of doobie (1.0-5d0957d-SNAPSHOT). */
      |  val version = "1.0-5d0957d-SNAPSHOT"
      |  /** Build date (Mon Apr 04 23:15:54 CEST 2022). */
      |  val date    = new java.util.Date(1649106954019L)
      |}""".stripMargin
      Files.createDirectories(to.getParent)
      Files.writeString(to, content)
    }

  }
}