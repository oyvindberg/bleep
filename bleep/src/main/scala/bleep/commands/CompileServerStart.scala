package bleep
package commands

import bleep.bsp.{BloopLogger, CompileServerConfig, SetupBloopRifle}
import bleep.internal.{BspClientDisplayProgress, Lazy}
import bleep.logging.Logger

import java.nio.file.Files
import scala.build.bloop.{BloopServer, BloopThreads}
import scala.build.blooprifle.BloopRifle

case class CompileServerStart(logger: Logger, userPaths: UserPaths, buildPaths: BuildPaths, lazyResolver: Lazy[CoursierResolver]) extends BleepCommand {
  override def run(): Either[BuildException, Unit] = {
    val config = CompileServerConfig.Shared
    // persist config
    CompileServerConfig.store(logger, userPaths, config)

    val rifleConfig = SetupBloopRifle(JavaCmd.javacommand, buildPaths, lazyResolver, config.asAddress(userPaths))
    val rifleLogger = new BloopLogger(logger)
    if (BloopRifle.check(rifleConfig, rifleLogger)) {
      Right(logger.info("Compile server is already running"))
    } else {
      val tempDir = Files.createTempDirectory("bleep-bloop")
      Right {
        // start new
        BloopServer.buildServer(
          config = rifleConfig,
          clientName = "bleep",
          clientVersion = constants.version,
          workspace = tempDir,
          classesDir = tempDir / "classes",
          buildClient = BspClientDisplayProgress(logger),
          threads = BloopThreads.create(),
          logger = rifleLogger
        )
      }
    }
  }
}
