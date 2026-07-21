package bleep
package scripts

import bleep.plugin.nativeimage.NativeImagePlugin

import java.io.File
import java.nio.file.{Files, Path}
import java.util.jar.{Attributes, JarOutputStream, Manifest}

object GenNativeImage extends BleepScript("GenNativeImage") {

  /** Match the same condition the `release` job in `.github/workflows/build.yml` uses: `startsWith(github.ref, 'refs/tags/v')`. When CI is building artifacts
    * that will be uploaded to a GitHub Release, we want full `-O2` optimisation; every other invocation gets `-Ob`.
    */
  private def isReleaseBuild: Boolean =
    sys.env.get("GITHUB_REF").exists(_.startsWith("refs/tags/v"))

  /** Native-image options shared by both the inline build path and the `--emit-script` path. Centralised here so the stand-alone launcher script and a regular
    * `bleep native-image` invocation produce the same binary, modulo the host JVM picking the binary up off `JAVA_HOME` on Windows CI.
    *
    * `quickBuild` adds GraalVM's `-Ob` (alias `-O0`): skips advanced inlining / escape-analysis / etc., trading runtime performance for **30–50% faster
    * build**. Selected when not building from a release tag (see [[isReleaseBuild]]): every PR, every master push, every local `bleep native-image` gets the
    * quick path. Release tag builds get full optimisation. The mode is logged at the top of the build so a reviewer can confirm which mode produced a given
    * binary.
    */
  private def nativeImageOptions(quickBuild: Boolean): List[String] = {
    val base = List(
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
    if (quickBuild) base :+ "-Ob" else base
  }

  def run(started: Started, commands: Commands, args: List[String]): Unit = {
    // Env-var signal to switch to script-emit mode (env not flag, because bleep's script subcommand parser uses `Opts.arguments[String]()` which rejects
    // anything starting with `--`). Set `BLEEP_NATIVE_IMAGE_EMIT_SCRIPT=<path>` to make this script write a stand-alone launcher to <path> and exit, instead
    // of running the build inline. CI then stops the compile-server and runs the launcher so `native-image` gets the full RAM budget.
    val emitScript = sys.env.get("BLEEP_NATIVE_IMAGE_EMIT_SCRIPT").map(Path.of(_))
    val rest = args

    // Quick-build mode (`-Ob`) on every non-release build. Signal: the `GITHUB_REF` env var, which the `release` job in build.yml gates itself on
    // (`if: "startsWith(github.ref, 'refs/tags/v')"`). When unset (local dev) we also get the fast path — testing a binary locally doesn't need full
    // optimisation. When set to anything other than a release tag (PR / master) we still want fast: the artifact is throwaway.
    val quickBuild = !isReleaseBuild
    val optMode = if (quickBuild) "-Ob (quick build, snapshot/PR/local)" else "-O2 (full optimisation, release tag)"
    started.logger.withContext("GITHUB_REF", sys.env.getOrElse("GITHUB_REF", "<unset>")).withContext("mode", optMode).info("native-image build mode")

    val options = nativeImageOptions(quickBuild)

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
      nativeImageOptions = options,
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
        val (command, cwd) = prepareNativeImageCommand(plugin, project, options)
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
  private def prepareNativeImageCommand(plugin: NativeImagePlugin, project: ResolvedProject, options: List[String]): (List[String], Path) = {
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
        options :::
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

  /** Format a single classpath entry for a Class-Path: manifest attribute. On Windows we prefer relative paths because some older JDKs reject absolute paths
    * here, but when the entry's drive differs from the manifest jar's drive (typical on GitHub Actions: D:\workspace, C:\Users\…\Coursier cache),
    * `Path.relativize` throws `IllegalArgumentException("'other' has different root")`. In that case fall back to an absolute forward-slashed path —
    * native-image's class-path attribute parser does `Path.of(entry)` on each token, which accepts `C:/foo/bar.jar` but not `file:` URIs.
    */
  private def manifestEntry(manifestJar: Path, path: Path): String = {
    val isWin = scala.util.Properties.isWin
    val syntax: String =
      if (isWin) {
        try manifestJar.getParent.relativize(path).toString.replace('\\', '/')
        catch { case _: IllegalArgumentException => path.toAbsolutePath.toString.replace('\\', '/') }
      } else path.toUri.getPath
    if (syntax.endsWith(".jar") || syntax.endsWith(File.separator) || syntax.endsWith("/")) syntax else syntax + "/"
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
        val _ = Files.setPosixFilePermissions(scriptPath, perms)
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
    env.foreach { case (k, v) => sb.append(s"export $k=${shQuote(v)}\n") }
    sb.append(s"cd ${shQuote(cwd.toAbsolutePath.toString)}\n")
    sb.append(s"exec ${command.map(shQuote).mkString(" ")}\n")
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
    env.foreach { case (k, v) => sb.append(s"set $k=${v.replace("\"", "\"\"")}\r\n") }
    sb.append(s"cd /d ${batQuote(cwd.toAbsolutePath.toString)}\r\n")
    sb.append(s"${command.map(batQuote).mkString(" ")}\r\n")
    sb.append("exit /b %ERRORLEVEL%\r\n")
    sb.toString
  }
}
