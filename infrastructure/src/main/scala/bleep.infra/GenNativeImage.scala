package bleep
package infra

import bleep.model.{CrossProjectName, ProjectName}
import bleep.tasks._
import net.hamnaberg.blooppackager.PackagePlugin

object GenNativeImage extends App {
  bootstrap.forScript("GenNativeImage") { started =>
    val projectName = CrossProjectName(ProjectName("bleep"), crossId = None)
    val project = started.bloopFiles(projectName).forceGet

    // https://github.com/shyiko/jabba/pull/821/files
    val index = "https://raw.githubusercontent.com/shyiko/jabba/54af2bdc0d895e23495bbee733673c2a36179a34/index.json"

    val plugin = new NativeImagePlugin(
      project.project,
      started.logger,
      nativeImageOptions = List("--no-fallback", "-H:+ReportExceptionStackTraces"),
      nativeImageJvm = "graalvm-ce-java17",
      nativeImageVersion = "21.3.0",
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
