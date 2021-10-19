package bleep
package infra

import bleep.model.ProjectName
import bleep.tasks.NativeImageTasks

import java.nio.file.{Path, Paths}

object GenNativeImage {
  implicit val cwd: Path = Paths.get(System.getProperty("user.dir"))

  def main(args: Array[String]): Unit = {
    val projectName = ProjectName("bleep")
    cli(s"bloop compile ${projectName.value}")

    val project = readBloopFile(cwd / Defaults.BloopFolder, projectName)

    val plugin = new NativeImageTasks(
      project.project,
      nativeImageOptions = List(
        "--no-fallback"
      )
    )
    val path = plugin.nativeImage()
    println(s"Created native-image at $path")
  }
}
