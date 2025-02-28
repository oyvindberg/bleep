package bleep
package scripts

import bleep.plugin.nativeimage.NativeImagePlugin

import java.nio.file.{Files, Path}

object GenNativeImage extends BleepScript("GenNativeImage") {
  def run(started: Started, commands: Commands, args: List[String]): Unit = {
    val jvm3 = model.CrossId("jvm3")
    val projectName = model.CrossProjectName(model.ProjectName("bleep-cli"), crossId = Some(jvm3))
    val project = started.bloopProject(projectName)
    // commands.compile(List(projectName))

    // this is here to pick up the graalvm installed by `graalvm/setup-graalvm@v1` in github actions *on windows*
    // the one we install ourselves does not work
    val jvm = sys.env.get("JAVA_HOME").map(javahome => Path.of(javahome).resolve("bin/java.exe")).filter(Files.exists(_)) match {
      case Some(jvm) =>
        started.logger
          .withContext("jvm", jvm)
          .warn("hack: picked up external java from JAVA_HOME. this mechanism is meant to fix native image build on windows")
        jvm
      case None => started.jvmCommand
    }

    val plugin = new NativeImagePlugin(
      project = project,
      logger = started.logger,
      jvmCommand = jvm,
      nativeImageOptions = List(
        "-march=compatibility",
        "--no-fallback",
        "--enable-http",
        "--enable-https",
        "-H:+ReportExceptionStackTraces",
        "-H:+UnlockExperimentalVMOptions",
        "--initialize-at-build-time=scala.runtime.Statics$VM",
        "--initialize-at-build-time=scala.Symbol",
        "--initialize-at-build-time=scala.Symbol$",
        "--native-image-info"
      ),
      env = sys.env.toList
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
