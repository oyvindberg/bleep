package bleep
package scripts

import bleep.tasks._

object GenNativeImage {
  def main(args: Array[String]): Unit =
    bootstrap.forScript("GenNativeImage") { (started, commands) =>
      val projectName = model.CrossProjectName(model.ProjectName("bleep-cli"), crossId = None)
      val project = started.bloopProjects(projectName)
      commands.compile(List(projectName))

      val plugin = new NativeImagePlugin(
        project = project,
        logger = started.logger,
        nativeImageOptions = List("--no-fallback", "-H:+ReportExceptionStackTraces"),
        nativeImageJvm = started.rawBuild.jvm.getOrElse(model.Jvm.graalvm)
      ) {
        // allow user to pass in name of generated binary as parameter
        override val nativeImageOutput = args.headOption match {
          case Some(relPath) =>
            // smoothen over some irritation from github action scripts
            val relPathNoExe = if (relPath.endsWith(".exe")) relPath.dropRight(".exe".length) else relPath
            started.prebootstrapped.buildPaths.cwd / relPathNoExe
          case None => super.nativeImageOutput
        }
      }
      val path = plugin.nativeImage()
      started.logger.info(s"Created native-image at $path")
    }
}
