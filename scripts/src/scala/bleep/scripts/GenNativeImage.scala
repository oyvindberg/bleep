package bleep
package scripts

import bleep.plugin.nativeimage.NativeImagePlugin

object GenNativeImage extends BleepScript("GenNativeImage") {
  def run(started: Started, commands: Commands, args: List[String]): Unit = {
    val projectName = model.CrossProjectName(model.ProjectName("bleep-cli"), crossId = Some(model.CrossId("jvm3")))
    val project = started.bloopProject(projectName)
    commands.compile(List(projectName))

    val plugin = new NativeImagePlugin(
      project = project,
      logger = started.logger,
      jvmCommand = started.jvmCommand,
      nativeImageOptions = List(
        "--no-fallback",
        "--enable-http",
        "--enable-https",
        "-H:+ReportExceptionStackTraces",
        "--initialize-at-build-time=scala.runtime.Statics$VM",
        "--initialize-at-build-time=scala.Symbol",
        "--initialize-at-build-time=scala.Symbol$",
        "--native-image-info"
      ),
      env = sys.env.toList ++ List(("USE_NATIVE_IMAGE_JAVA_PLATFORM_MODULE_SYSTEM", "false"))
    ) {
      // allow user to pass in name of generated binary as parameter
      override val nativeImageOutput = args.headOption match {
        case Some(relPath) =>
          // smoothen over some irritation from github action scripts
          val relPathNoExe = if (relPath.endsWith(".exe")) relPath.dropRight(".exe".length) else relPath
          started.buildPaths.cwd / relPathNoExe
        case None => super.nativeImageOutput
      }
    }
    val path = plugin.nativeImage()
    started.logger.info(s"Created native-image at $path")
  }
}
