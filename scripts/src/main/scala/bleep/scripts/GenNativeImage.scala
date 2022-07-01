package bleep
package scripts

import bleep.tasks._

object GenNativeImage {
  def main(args: Array[String]): Unit =
    bootstrap.forScript("GenNativeImage") { (started, commands) =>
      val projectName = model.CrossProjectName(model.ProjectName("bleep-cli"), crossId = None)
      val project = started.bloopProjects(projectName)
      commands.compile(List(projectName))

      val index = "https://raw.githubusercontent.com/coursier/jvm-index/master/index.json"
      val plugin = new NativeImagePlugin(
        project,
        started.logger,
        nativeImageOptions = List("--no-fallback", "-H:+ReportExceptionStackTraces"),
        nativeImageJvm = started.rawBuild.jvm.getOrElse(model.Jvm.graalvm),
        nativeImageJvmIndex = index
      ) {
        override val nativeImageOutput = args.headOption match {
          case Some(value) =>
            started.prebootstrapped.buildPaths.cwd / value
          case None => super.nativeImageOutput
        }
      }
      val path = plugin.nativeImage()
      started.logger.info(s"Created native-image at $path")
    }
}
