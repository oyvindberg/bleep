package bleep
package infra

import bleep.model.ProjectName
import bleep.tasks._
import net.hamnaberg.blooppackager.PackagePlugin

object GenNativeImage extends App {
  bootstrap.simple { started =>
    val projectName = ProjectName("bleep")
    val project = started.bloopFiles(projectName).forceGet(projectName.value)

    val plugin = new NativeImagePlugin(project.project, started.logger, nativeImageOptions = List("--no-fallback", "-H:+ReportExceptionStackTraces"))
    val path = plugin.nativeImage()
    started.logger.info(s"Created native-image at $path")
  }
}

object PackageAll extends App {
  bootstrap.simple { started =>
    val all: List[String] = started.projects.map(_.name)

    PackagePlugin.run(started.logger, started.projects, PackageCommand.Jars(all))

    val gitVersioningPlugin = new GitVersioningPlugin(started.buildPaths.buildDir, started.logger)()
    started.logger.info(gitVersioningPlugin.version)
  }
}
