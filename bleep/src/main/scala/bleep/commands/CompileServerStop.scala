package bleep
package commands

import bleep.bsp.{BloopLogger, CompileServerConfig, SetupBloopRifle}
import bleep.internal.Lazy
import bleep.logging.Logger

import scala.build.blooprifle.BloopRifle

case class CompileServerStop(logger: Logger, userPaths: UserPaths, buildPaths: BuildPaths, lazyResolver: Lazy[CoursierResolver]) extends BleepCommand {
  override def run(): Either[BuildException, Unit] =
    CompileServerConfig.load(userPaths) map {
      case CompileServerConfig.NewEachInvocation =>
        logger.warn("Nothing to stop")
      case config @ CompileServerConfig.Shared =>
        val rifleConfig = SetupBloopRifle(JavaCmd.javacommand, buildPaths, lazyResolver, config.asAddress(userPaths))
        val rifleLogger = new BloopLogger(logger)
        if (BloopRifle.check(rifleConfig, rifleLogger)) {
          BloopRifle.exit(rifleConfig, buildPaths.dotBleepDir, rifleLogger)
        } else
          logger.info("bloop server was not running")

        CompileServerConfig.store(logger, userPaths, CompileServerConfig.NewEachInvocation)
        ()
    }
}
