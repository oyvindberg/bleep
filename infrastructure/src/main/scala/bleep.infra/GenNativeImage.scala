package bleep.infra

import bloop.config.ConfigCodecs
import com.github.plokhotnyuk.jsoniter_scala

import java.nio.file.{Files, Path, Paths}

object GenNativeImage {
  val workspaceDir: Path = Paths.get(System.getProperty("user.dir"))

  def cli(cmd: String): Unit =
    sys.process.Process(cmd).! match {
      case 0 => ()
      case n =>
        System.err.println(s"FAILED: $cmd")
        System.exit(n)
    }

  def main(args: Array[String]): Unit = {
    cli("bloop compile bleep")

    val contents = Files.readString(workspaceDir.resolve(".bloop/bleep.json"))
    val project = jsoniter_scala.core.readFromString(contents)(ConfigCodecs.codecFile)
    val plugin = new NativeImagePlugin(
      project.project,
      nativeImageOptions = List(
        "--no-fallback",
      )
    )
    plugin.nativeImage()
  }
}
