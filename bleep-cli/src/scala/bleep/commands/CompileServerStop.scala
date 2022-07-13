package bleep
package commands

import bleep.Lazy
import bleep.bsp.{BleepRifleLogger, CompileServerMode, SetupBloopRifle}
import bleep.internal.FileUtils
import bleep.logging.Logger

import scala.build.blooprifle.BloopRifle
import scala.concurrent.ExecutionContext

case class CompileServerStop(logger: Logger, userPaths: UserPaths, lazyResolver: Lazy[CoursierResolver]) extends BleepCommand {
  override def run(): Either[BuildException, Unit] =
    BleepConfig
      .rewritePersisted(logger, userPaths) { bleepConfig =>
        bleepConfig.compileServerMode match {
          case CompileServerMode.NewEachInvocation =>
            logger.warn("Nothing to stop")
            bleepConfig

          case mode @ CompileServerMode.Shared =>
            val bleepRifleLogger = new BleepRifleLogger(logger)
            val rifleConfig = SetupBloopRifle(bleepConfig, logger, userPaths, lazyResolver, bleepRifleLogger, ExecutionContext.global)
            if (BloopRifle.check(rifleConfig, bleepRifleLogger)) {
              BloopRifle.exit(rifleConfig, FileUtils.TempDir, bleepRifleLogger)
            } else
              logger.info("bloop server was not running")

            bleepConfig.copy(compileServerMode = CompileServerMode.NewEachInvocation)
        }
      }
      .map(_ => ())
}
