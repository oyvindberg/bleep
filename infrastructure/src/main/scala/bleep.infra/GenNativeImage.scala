package bleep
package infra

import bleep.model.ProjectName
import bleep.tasks._
import net.hamnaberg.blooppackager.PackagePlugin

import java.nio.file.Path

object GenNativeImage extends App {
  bootstrap.fromCwd match {
    case Left(value) => throw value
    case Right(started) =>
      implicit val wd: Path = started.buildPaths.buildDir

      val projectName = ProjectName("bleep")
      val project = started.bloopFiles(projectName).forceGet(projectName.value)

      cli(s"bloop compile ${projectName.value}")

      val plugin = new NativeImagePlugin(project.project, nativeImageOptions = List("--no-fallback", "-H:+ReportExceptionStackTraces"))
      val path = plugin.nativeImage()
      println(s"Created native-image at $path")
  }
}

object PackageAll extends App {
  bootstrap.fromCwd match {
    case Left(value) => throw value
    case Right(started) =>
      implicit val wd: Path = started.buildPaths.buildDir

      val projectNames: List[String] =
        started.projects.map(_.name)

      cli(s"bloop compile ${projectNames.mkString(" ")}")

      PackagePlugin.run(started.projects, PackageCommand.Jars(projectNames))

      val gitVersioningPlugin = new GitVersioningPlugin(started.buildPaths.buildDir, Logger.Println)()
      println(gitVersioningPlugin.version)
  }
}
