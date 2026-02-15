package bleep
package commands

import bleep.bsp.protocol.BleepBspProtocol
import bleep.internal.{jvmRunCommand, TransitiveProjects}

import java.nio.file.Files

/** @param raw
  *   use raw stdin and stdout and avoid the logger
  */
case class Run(
    project: model.CrossProjectName,
    maybeOverriddenMain: Option[String],
    args: List[String],
    raw: Boolean,
    watch: Boolean,
    buildOpts: CommonBuildOpts
) extends BleepBuildCommand {

  override def run(started: Started): Either[BleepException, Unit] =
    if (watch) WatchMode.run(started, s => TransitiveProjects(s.build, Array(project)))(runOnce)
    else runOnce(started)

  private def runOnce(started: Started): Either[BleepException, Unit] = {
    val maybeSpecifiedMain: Option[String] =
      maybeOverriddenMain.orElse(started.build.explodedProjects(project).platform.flatMap(_.mainClass))

    val explodedProject = started.build.explodedProjects(project)
    val platformId = explodedProject.platform.flatMap(_.name)
    val isKotlin = explodedProject.kotlin.flatMap(_.version).isDefined

    (platformId, isKotlin) match {
      case (Some(model.PlatformId.Jvm), _) | (None, _) =>
        for {
          _ <- compileProject(started)
          main <- resolveMain(started, maybeSpecifiedMain)
          _ <- jvmRun(started, main)
        } yield ()
      case (Some(model.PlatformId.Js), _) =>
        for {
          _ <- linkProject(started)
          _ <- runJs(started, isKotlin)
        } yield ()
      case (Some(model.PlatformId.Native), _) =>
        for {
          _ <- linkProject(started)
          _ <- runNative(started)
        } yield ()
    }
  }

  private def compileProject(started: Started): Either[BleepException, Unit] =
    ReactiveBsp
      .compile(
        watch = false,
        projects = Array(project),
        displayMode = buildOpts.displayMode,
        flamegraph = buildOpts.flamegraph,
        cancel = buildOpts.cancel
      )
      .run(started)

  private def linkProject(started: Started): Either[BleepException, Unit] =
    ReactiveBsp
      .link(
        watch = false,
        projects = Array(project),
        displayMode = buildOpts.displayMode,
        options = LinkOptions.Debug,
        flamegraph = buildOpts.flamegraph,
        cancel = buildOpts.cancel
      )
      .run(started)

  private def resolveMain(started: Started, maybeSpecifiedMain: Option[String]): Either[BleepException, String] =
    maybeSpecifiedMain match {
      case Some(mainClass) => Right(mainClass)
      case None =>
        started.logger.info("No main class specified in build or command line. discovering...")
        BspQuery.withServer(started) { server =>
          discoverMain(started.logger, server, BspQuery.buildTarget(started.buildPaths, project))
        }
    }

  private def jvmRun(started: Started, main: String): Either[BleepException, Unit] = {
    val outMode = if (raw) cli.Out.Raw else cli.Out.ViaLogger(started.logger)
    val inMode = if (raw) cli.In.Attach else cli.In.No
    cli(
      "run",
      started.pre.buildPaths.cwd,
      jvmRunCommand(started.resolvedProject(project), started.resolvedJvm, project, Some(main), args).orThrow,
      logger = started.logger,
      out = outMode,
      in = inMode,
      env = sys.env.toList
    ).discard()
    Right(())
  }

  private def runJs(started: Started, isKotlin: Boolean): Either[BleepException, Unit] = {
    val targetDir = started.projectPaths(project).targetDir
    // Link uses debug config (isReleaseMode = false)
    val linkDirSuffix = BleepBspProtocol.linkDirSuffix(isRelease = false, hasDebugInfo = false, hasLto = false)
    val jsOutput = if (isKotlin) {
      // Kotlin/JS uses underscores in module names (converts hyphens)
      val moduleName = project.value.replace("-", "_")
      targetDir.resolve("link-output").resolve(linkDirSuffix).resolve("js").resolve(s"$moduleName.js")
    } else {
      targetDir.resolve("link-output").resolve(linkDirSuffix).resolve("js").resolve("main.js")
    }

    if (!Files.exists(jsOutput)) {
      Left(new BleepException.Text(s"JS output not found at $jsOutput. Compile may not have produced linked output."))
    } else {
      val outMode = if (raw) cli.Out.Raw else cli.Out.ViaLogger(started.logger)
      val inMode = if (raw) cli.In.Attach else cli.In.No
      val command = List("node", jsOutput.toAbsolutePath.toString) ++ args
      cli(
        "run",
        started.pre.buildPaths.cwd,
        command,
        logger = started.logger,
        out = outMode,
        in = inMode,
        env = sys.env.toList
      ).discard()
      Right(())
    }
  }

  private def runNative(started: Started): Either[BleepException, Unit] = {
    val targetDir = started.projectPaths(project).targetDir
    // Link uses debug config (isReleaseMode = false)
    val linkDirSuffix = BleepBspProtocol.linkDirSuffix(isRelease = false, hasDebugInfo = false, hasLto = false)
    val linkOutput = targetDir.resolve("link-output").resolve(linkDirSuffix)
    // Native binaries may have different extensions
    val possiblePaths = Seq(
      linkOutput.resolve(project.value),
      linkOutput.resolve(project.value + ".kexe") // Kotlin/Native on macOS
    )
    val binary = possiblePaths.find(Files.exists(_))

    binary match {
      case None =>
        Left(new BleepException.Text(s"Native binary not found. Checked: ${possiblePaths.mkString(", ")}"))
      case Some(binaryPath) =>
        val outMode = if (raw) cli.Out.Raw else cli.Out.ViaLogger(started.logger)
        val inMode = if (raw) cli.In.Attach else cli.In.No
        val command = List(binaryPath.toAbsolutePath.toString) ++ args
        cli(
          "run",
          started.pre.buildPaths.cwd,
          command,
          logger = started.logger,
          out = outMode,
          in = inMode,
          env = sys.env.toList
        ).discard()
        Right(())
    }
  }
}
