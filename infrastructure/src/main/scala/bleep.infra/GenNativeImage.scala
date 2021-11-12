package bleep
package infra

import bleep.bootstrap.Bootstrapped
import bleep.model.ProjectName
import bleep.tasks._

import java.nio.file.Path

object GenNativeImage extends App {
  bootstrap.fromCwd match {
    case Bootstrapped.BuildNotFound  => ???
    case Bootstrapped.InvalidJson(e) => ???
    case Bootstrapped.Ok(buildDirPath, _, lazyBloopFiles, _) =>
      implicit val wd: Path = buildDirPath

      val gitVersioningPlugin = new GitVersioningPlugin(buildDirPath, Logger.Println)()
      println(gitVersioningPlugin.version)

      val projectName = ProjectName("bleep")
      val project = lazyBloopFiles(projectName).forceGet(projectName.value)

      cli(s"bloop compile ${projectName.value}")

      val plugin = new NativeImagePlugin(project.project, nativeImageOptions = List("--no-fallback"))
      val path = plugin.nativeImage()
      println(s"Created native-image at $path")
  }
}
