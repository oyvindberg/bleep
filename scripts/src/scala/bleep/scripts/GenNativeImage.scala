package bleep
package scripts

import java.nio.file.Path
import java.nio.file.Files
import bleep.plugin.nativeimage.NativeImagePlugin

object GenNativeImage extends BleepScript("GenNativeImage") {
  def run(started: Started, commands: Commands, args: List[String]): Unit = {
    val jvm3 = model.CrossId("jvm3")
    val projectName = model.CrossProjectName(model.ProjectName("bleep-cli"), crossId = Some(jvm3))
    val localLibDaemon = model.CrossProjectName(model.ProjectName("bleep-libdaemon-jvm"), crossId = Some(jvm3))
    val project = started.bloopProject(projectName)
    commands.compile(List(projectName, localLibDaemon))
    val dir = started.bloopProject(localLibDaemon).classesDir
    println(dir)
    // use a forked version of libdaemon-jvm for native-image
    val fixedProject =
      project.copy(classpath = project.classpath.filterNot(_.toString.contains("libdaemon")) :+ dir)
    val jvm = sys.env.get("JAVA_HOME").map(javahome => Path.of(javahome).resolve("bin/java.exe")).filter(Files.exists(_)).getOrElse(started.jvmCommand)
    val plugin = new NativeImagePlugin(
      project = fixedProject,
      logger = started.logger,
      jvmCommand = jvm,
      nativeImageOptions = List(
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
