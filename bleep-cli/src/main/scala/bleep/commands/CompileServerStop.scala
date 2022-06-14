package bleep
package commands

import bleep.bsp.{BloopLogger, CompileServerMode, SetupBloopRifle}
import bleep.internal.Lazy
import bleep.logging.Logger

import java.nio.file.Path
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

          case status @ CompileServerMode.Shared =>
            val rifleConfig = SetupBloopRifle(JvmCmd(logger, bleepConfig.compileServerJvm, ExecutionContext.global), userPaths, lazyResolver, status)
            val rifleLogger = new BloopLogger(logger)
            if (BloopRifle.check(rifleConfig, rifleLogger)) {
              BloopRifle.exit(rifleConfig, Path.of("/tmp"), rifleLogger)
            } else
              logger.info("bloop server was not running")

            bleepConfig.copy(compileServerMode = CompileServerMode.NewEachInvocation)
        }
      }
      .map(_ => ())
}
