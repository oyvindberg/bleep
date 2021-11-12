package bleep
package infra

import bleep.bootstrap.Bootstrapped
import bleep.model.ProjectName
import bleep.tasks._
import net.hamnaberg.blooppackager.PackagePlugin

import java.nio.file.Path
import scala.collection.immutable

object GenNativeImage extends App {
  bootstrap.fromCwd match {
    case Bootstrapped.BuildNotFound  => ???
    case Bootstrapped.InvalidJson(e) => ???
    case Bootstrapped.Ok(buildDirPath, _, lazyBloopFiles, _) =>
      implicit val wd: Path = buildDirPath

      val projectName = ProjectName("bleep")
      val project = lazyBloopFiles(projectName).forceGet(projectName.value)

      cli(s"bloop compile ${projectName.value}")

      val plugin = new NativeImagePlugin(project.project, nativeImageOptions = List("--no-fallback"))
      val path = plugin.nativeImage()
      println(s"Created native-image at $path")
  }
}

object PackageAll extends App {
  bootstrap.fromCwd match {
    case Bootstrapped.BuildNotFound  => ???
    case Bootstrapped.InvalidJson(e) => ???
    case ok: Bootstrapped.Ok =>
      implicit val wd: Path = ok.buildDirPath

      val projectNames: List[String] =
        ok.projects.map(_.name)

      cli(s"bloop compile ${projectNames.mkString(" ")}")

      PackagePlugin.run(ok.projects, PackageCommand.Jars(projectNames))

      val gitVersioningPlugin = new GitVersioningPlugin(ok.buildDirPath, Logger.Println)()
      println(gitVersioningPlugin.version)
  }
}