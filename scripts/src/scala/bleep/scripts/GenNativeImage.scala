package bleep
package scripts

import bleep.internal.jvmOrSystem
import bleep.logging.Logger
import sbtnativeimage.NativeImagePlugin

object GenNativeImage extends BleepScript("GenNativeImage") {
  def run(started: Started, commands: Commands, args: List[String]): Unit = {
    val projectName = model.CrossProjectName(model.ProjectName("bleep-cli"), crossId = None)
    val project = started.bloopProjects(projectName)
    commands.compile(List(projectName))
    val nativeImageJvm = jvmOrSystem(started.build, logger = Logger.DevNull) match {
      case chosen if chosen.name.startsWith("graalvm-") => chosen
      case _                                            => model.Jvm.graalvm
    }

    val plugin = new NativeImagePlugin(
      project,
      started.logger,
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
      nativeImageJvm = nativeImageJvm,
      env = sys.env.toList ++ List(("USE_NATIVE_IMAGE_JAVA_PLATFORM_MODULE_SYSTEM", "false"))
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
