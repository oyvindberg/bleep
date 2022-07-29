package bleep
package scripts

import bleep.tasks._

object GenNativeImage extends BleepScriptRunner("GenNativeImage") {
  def runScript(started: Started, commands: Commands, args: List[String]): Unit = {
    val projectName = model.CrossProjectName(model.ProjectName("bleep-cli"), crossId = None)
    val project = started.bloopProjects(projectName)
    commands.compile(List(projectName))

    val plugin = new NativeImagePlugin(
      project,
      started.logger,
      nativeImageOptions = List("--no-fallback", "-H:+ReportExceptionStackTraces"),
      nativeImageJvm = started.rawBuild.jvm.getOrElse(model.Jvm.graalvm)
    ) {
      override val nativeImageOutput = args.headOption match {
        case Some(relPath) =>
          val relPathNoExe = if (relPath.endsWith(".exe")) relPath.dropRight(".exe".length) else relPath
          started.prebootstrapped.buildPaths.cwd / relPathNoExe
        case None => super.nativeImageOutput
      }
    }
    val path = plugin.nativeImage()
    started.logger.info(s"Created native-image at $path")
  }
}
