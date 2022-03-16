package bleep
package infra

import bleep.tasks._
import net.hamnaberg.blooppackager.PackagePlugin

object GenNativeImage extends App {
  bootstrap.forScript("GenNativeImage") { started =>
    val projectName = model.CrossProjectName(model.ProjectName("bleep"), crossId = None)
    val project = started.bloopFiles(projectName).forceGet

    val index = "https://raw.githubusercontent.com/coursier/jvm-index/master/index.json"

    val plugin = new NativeImagePlugin(
      project.project,
      started.logger,
      nativeImageOptions = List("--no-fallback", "-H:+ReportExceptionStackTraces"),
      nativeImageJvm = "graalvm-java17",
      nativeImageVersion = "22.0.0.2",
      nativeImageJvmIndex = index
    )
    val path = plugin.nativeImage()
    started.logger.info(s"Created native-image at $path")
  }
}

object PackageAll extends App {
  bootstrap.forScript("PackageAll") { started =>
    val all: List[String] = started.bloopProjects.map(_.name)

    PackagePlugin.run(started.logger, started.bloopProjects, PackageCommand.Jars(all))

    val gitVersioningPlugin = new GitVersioningPlugin(started.buildPaths.buildDir, started.logger)()
    started.logger.info(gitVersioningPlugin.version)
  }
}
