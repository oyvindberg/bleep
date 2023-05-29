
package scripts

import bleep.{BleepCodegenScript, Commands, Started}

import java.nio.file.Files

object GenerateForSbtBloop extends BleepCodegenScript("GenerateForSbtBloop") {
  override def run(started: Started, commands: Commands, targets: List[Target], args: List[String]): Unit = {
    started.logger.error("This script is a placeholder! You'll need to replace the contents with code which actually generates the files you want")

    targets.foreach { target =>
      if (Set("sbt-bloop").contains(target.project.value)) {
        val to = target.resources.resolve("sbt/sbt.autoplugins")
        started.logger.withContext(target.project).warn(s"Writing $to")
        val content = s"""|bloop.integrations.sbt.BloopPlugin""".stripMargin
        Files.createDirectories(to.getParent)
        Files.writeString(to, content)
      }
    }

  }
}