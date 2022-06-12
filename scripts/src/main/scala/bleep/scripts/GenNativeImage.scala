package bleep
package scripts

import bleep.tasks._

object GenNativeImage extends App {
  bootstrap.forScript("GenNativeImage") { (started, commands) =>
    val projectName = model.CrossProjectName(model.ProjectName("bleep"), crossId = None)
    val project = started.bloopProjects(projectName)

    commands.compile(List(projectName))

    val index = "https://raw.githubusercontent.com/coursier/jvm-index/master/index.json"
    val plugin = new NativeImagePlugin(
      project,
      started.logger,
      nativeImageOptions = List("--no-fallback", "-H:+ReportExceptionStackTraces"),
      nativeImageJvm = "graalvm-java17",
      nativeImageVersion = "22.1.0",
      nativeImageJvmIndex = index
    )
    val path = plugin.nativeImage()
    started.logger.info(s"Created native-image at $path")
  }
}
