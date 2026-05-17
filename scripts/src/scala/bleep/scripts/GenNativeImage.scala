package bleep
package scripts

import bleep.plugin.nativeimage.NativeImagePlugin

import java.io.File
import java.nio.file.{Files, Path}
import java.util.jar.{Attributes, JarOutputStream, Manifest}

object GenNativeImage extends BleepScript("GenNativeImage") {

  /** Native-image options shared by both the inline build path and the `--emit-script` path. Centralised here so the stand-alone launcher script and a regular
    * `bleep native-image` invocation produce the same binary, modulo the host JVM picking the binary up off `JAVA_HOME` on Windows CI.
    */
  private val nativeImageOptions: List[String] = List(
    "-march=compatibility",
    "--no-fallback",
    "--enable-http",
    "--enable-https",
    "-H:+ReportExceptionStackTraces",
    "-H:+UnlockExperimentalVMOptions",
    "--initialize-at-build-time=scala.runtime.Statics$VM",
    "--initialize-at-build-time=scala.Symbol",
    "--initialize-at-build-time=scala.Symbol$",
    "--enable-native-access=ALL-UNNAMED",
    "-J--sun-misc-unsafe-memory-access=allow",
    "-H:+ForeignAPISupport",
    "--native-image-info",
    "-J-Xmx5g"
  )

  def run(started: Started, commands: Commands, args: List[String]): Unit = {
    // Env-var signal to switch to script-emit mode (env not flag, because bleep's script subcommand parser uses `Opts.arguments[String]()` which rejects
    // anything starting with `--`). Set `BLEEP_NATIVE_IMAGE_EMIT_SCRIPT=<path>` to make this script write a stand-alone launcher to <path> and exit, instead
    // of running the build inline. CI then stops the compile-server and runs the launcher so `native-image` gets the full RAM budget.
    val emitScript = sys.env.get("BLEEP_NATIVE_IMAGE_EMIT_SCRIPT").map(Path.of(_))
    val rest = args

    val projectName = model.CrossProjectName(model.ProjectName("bleep-cli"), crossId = None)
    val project = started.bloopProject(projectName)

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
      nativeImageOptions = nativeImageOptions,
      env = sys.env.toList
    ) {
      // allow user to pass in name of generated binary as parameter
      override val nativeImageOutput = rest.headOption match {
        case Some(relPath) =>
          // smoothen over some irritation from github action scripts
          val relPathNoExe = if (relPath.endsWith(".exe")) relPath.dropRight(".exe".length) else relPath
          started.buildPaths.cwd / relPathNoExe
        case None => super.nativeImageOutput
      }
    }

    emitScript match {
      case Some(scriptPath) =>
        val (command, cwd) = prepareNativeImageCommand(started, plugin, project)
        writeLauncherScript(scriptPath, command, cwd, sys.env.toList)
        started.logger
          .withContext("scriptPath", scriptPath)
          .info("Wrote native-image launcher script. Stop the compile-server, then run the script directly so native-image gets the full RAM budget.")
      case None =>
        val path = plugin.nativeImage()
        started.logger.info(s"Created native-image at $path")
    }
  }

  /** Replicates the command-building logic of `NativeImagePlugin.nativeImage()` without launching the build. Produces the same manifest jar (classpath
    * indirection so we don't hit "argument list too large" on Windows) and the same argument vector, returning `(command, cwd)`. Kept here instead of in the
    * plugin so we don't have to spin a new release of the liberated sbt-native-image submodule for this feature.
    */
  private def prepareNativeImageCommand(started: Started, plugin: NativeImagePlugin, project: ResolvedProject): (List[String], Path) = {
    val mainClass = project.platform
      .flatMap {
        case jvm: ResolvedProject.Platform.Jvm       => jvm.mainClass
        case js: ResolvedProject.Platform.Js         => js.mainClass
        case native: ResolvedProject.Platform.Native => native.mainClass
      }
      .getOrElse(sys.error("no mainClass on bleep-cli — cannot emit native-image launcher"))

    val classpath = plugin.fixScala3(fixedClasspath(project))
    val manifestJar = plugin.targetNativeImageInternal / "manifest.jar"
    Files.createDirectories(manifestJar.getParent)
    writeManifestJar(manifestJar, classpath)

    val cwd = plugin.targetNativeImage
    Files.createDirectories(cwd)

    val command =
      plugin.nativeImageCommand.toString ::
        "-cp" ::
        manifestJar.toAbsolutePath.toString ::
        nativeImageOptions :::
        mainClass ::
        plugin.nativeImageOutput.toAbsolutePath.toString ::
        Nil

    (command, cwd)
  }

  /** Manifest-jar trampoline: builds a JAR whose only content is a META-INF/MANIFEST.MF with a `Class-Path:` entry listing every classpath path. Used so the
    * `native-image -cp <one.jar>` invocation stays well under any command-line length limits (Windows: ~8KB; Linux: ARG_MAX). On Windows paths are stored as
    * forward-slashed relative paths because absolute paths trip up some JDK manifest parsers.
    */
  private def writeManifestJar(manifestJar: Path, classpath: Seq[Path]): Unit = {
    val classpathStr = classpath.map(p => manifestEntry(manifestJar, p)).mkString(" ")
    val manifest = new Manifest()
    manifest.getMainAttributes.put(Attributes.Name.MANIFEST_VERSION, "1.0")
    manifest.getMainAttributes.put(Attributes.Name.CLASS_PATH, classpathStr)
    val out = Files.newOutputStream(manifestJar)
    val jos: JarOutputStream = new JarOutputStream(out, manifest)
    jos.close()
  }

  private def manifestEntry(manifestJar: Path, path: Path): String = {
    val isWin = scala.util.Properties.isWin
    val syntax = if (isWin) manifestJar.getParent.relativize(path).toString.replace('\\', '/') else path.toUri.getPath
    if (syntax.endsWith(".jar") || syntax.endsWith(File.separator)) syntax else syntax + File.separator
  }

  /** Write a self-contained launcher for `native-image`. Auto-detects shell vs batch from the script's extension (`.cmd`/`.bat` → Windows batch, else POSIX
    * shell). Each argument is quoted defensively so paths with spaces / `$` / `&` stay literal. The script:
    *   - shells out to the resolved `native-image` binary (already fully-pathed by the GraalVM install layout)
    *   - sets the env we passed when constructing the plugin (so JAVA_HOME tweaks etc. propagate when CI runs the script after the bleep CLI is gone)
    *   - cd's to the native-image work dir, matching what `nativeImage()` does inline
    *   - propagates the native-image exit code, so CI failures bubble up.
    */
  private def writeLauncherScript(scriptPath: Path, command: List[String], cwd: Path, env: List[(String, String)]): Unit = {
    val isBatch = {
      val s = scriptPath.getFileName.toString.toLowerCase
      s.endsWith(".cmd") || s.endsWith(".bat")
    }
    val script = if (isBatch) renderBatch(command, cwd, env) else renderShell(command, cwd, env)
    Option(scriptPath.getParent).foreach(p => Files.createDirectories(p))
    Files.writeString(scriptPath, script)
    if (!isBatch)
      try {
        val perms = Files.getPosixFilePermissions(scriptPath)
        perms.add(java.nio.file.attribute.PosixFilePermission.OWNER_EXECUTE)
        perms.add(java.nio.file.attribute.PosixFilePermission.GROUP_EXECUTE)
        perms.add(java.nio.file.attribute.PosixFilePermission.OTHERS_EXECUTE)
        Files.setPosixFilePermissions(scriptPath, perms)
      } catch { case _: UnsupportedOperationException => () }
  }

  private def renderShell(command: List[String], cwd: Path, env: List[(String, String)]): String = {
    def shQuote(s: String): String = "'" + s.replace("'", "'\\''") + "'"
    val sb = new StringBuilder
    sb.append("#!/usr/bin/env bash\n")
    sb.append(
      "# Auto-generated by `bleep native-image --emit-script`. Runs `native-image` standalone — no bleep CLI, no BSP server — so the build inherits all\n"
    )
    sb.append("# available RAM. Shut down the bleep compile-server before invoking this script (e.g. `bleep config compile-server stop-all`).\n")
    sb.append("set -euo pipefail\n")
    env.foreach { case (k, v) => sb.append("export ").append(k).append('=').append(shQuote(v)).append('\n') }
    sb.append("cd ").append(shQuote(cwd.toAbsolutePath.toString)).append('\n')
    sb.append("exec ").append(command.map(shQuote).mkString(" ")).append('\n')
    sb.toString
  }

  private def renderBatch(command: List[String], cwd: Path, env: List[(String, String)]): String = {
    def batQuote(s: String): String = "\"" + s.replace("\"", "\"\"") + "\""
    val sb = new StringBuilder
    sb.append("@echo off\r\n")
    sb.append(
      "REM Auto-generated by `bleep native-image --emit-script`. Runs `native-image` standalone -- no bleep CLI, no BSP server -- so the build inherits\r\n"
    )
    sb.append("REM all available RAM. Shut down the bleep compile-server before invoking (e.g. `bleep config compile-server stop-all`).\r\n")
    sb.append("setlocal\r\n")
    env.foreach { case (k, v) => sb.append("set ").append(k).append('=').append(v.replace("\"", "\"\"")).append("\r\n") }
    sb.append("cd /d ").append(batQuote(cwd.toAbsolutePath.toString)).append("\r\n")
    sb.append(command.map(batQuote).mkString(" ")).append("\r\n")
    sb.append("exit /b %ERRORLEVEL%\r\n")
    sb.toString
  }
}
