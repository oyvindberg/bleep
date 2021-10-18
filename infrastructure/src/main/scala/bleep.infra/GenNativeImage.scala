package bleep
package infra

import bleep.tasks.NativeImageTasks
import bloop.config.ConfigCodecs
import com.github.plokhotnyuk.jsoniter_scala

import java.nio.file.{Files, Path, Paths}

object GenNativeImage {
  implicit val cwd: Path = Paths.get(System.getProperty("user.dir"))

  def main(args: Array[String]): Unit = {
    cli("bloop compile bleep")

    val contents = Files.readString(cwd / ".bloop/bleep.json")
    val project = jsoniter_scala.core.readFromString(contents)(ConfigCodecs.codecFile)
    val plugin = new NativeImageTasks(
      project.project,
      nativeImageOptions = List(
        "--no-fallback"
      )
    )
    plugin.nativeImage()
  }
}
