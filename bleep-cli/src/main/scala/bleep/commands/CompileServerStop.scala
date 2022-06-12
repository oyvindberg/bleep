package bleep
package commands

import bleep.bsp.{BloopLogger, CompileServerMode, SetupBloopRifle}
import bleep.internal.Lazy

import scala.build.blooprifle.BloopRifle
import scala.concurrent.ExecutionContext

case class CompileServerStop(pre: Prebootstrapped, lazyResolver: Lazy[CoursierResolver]) extends BleepCommand {
  override def run(): Either[BuildException, Unit] =
    BleepConfig
      .rewritePersisted(pre.logger, pre.userPaths) { bleepConfig =>
        bleepConfig.compileServerMode match {
          case CompileServerMode.NewEachInvocation =>
            pre.logger.warn("Nothing to stop")
            bleepConfig

          case status @ CompileServerMode.Shared =>
            val rifleConfig = SetupBloopRifle(JvmCmd(pre.logger, bleepConfig.compileServerJvm, ExecutionContext.global), pre, lazyResolver, status)
            val rifleLogger = new BloopLogger(pre.logger)
            if (BloopRifle.check(rifleConfig, rifleLogger)) {
              BloopRifle.exit(rifleConfig, pre.buildPaths.dotBleepDir, rifleLogger)
            } else
              pre.logger.info("bloop server was not running")

            bleepConfig.copy(compileServerMode = CompileServerMode.NewEachInvocation)
        }
      }
      .map(_ => ())
}
