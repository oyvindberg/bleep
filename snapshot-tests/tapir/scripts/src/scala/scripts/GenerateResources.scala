
package scripts

import bleep.{BleepScript, Commands, Started}

import java.nio.file.Files

object GenerateResources extends BleepScript("GenerateResources") {
  def run(started: Started, commands: Commands, args: List[String]): Unit = {
    started.logger.error("This script is a placeholder! You'll need to replace the contents with code which actually generates the files you want")

    
    Vector(
      bleep.model.CrossProjectName(bleep.model.ProjectName("sbt-openapi-codegen"), None)).foreach { crossName =>
      val to = started.buildPaths.generatedResourcesDir(crossName).resolve("sbt/sbt.autoplugins")
      started.logger.withContext(crossName).warn(s"Writing $to")
      val content = s"""|OpenapiCodegenPlugin""".stripMargin
      Files.createDirectories(to.getParent)
      Files.writeString(to, content)
    }

  }
}