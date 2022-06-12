package bleep
package commands

import bleep.bsp.{BloopLogger, CompileServerMode, SetupBloopRifle}
import bleep.internal.{BspClientDisplayProgress, Lazy}

import java.nio.file.Files
import scala.build.bloop.{BloopServer, BloopThreads}
import scala.build.blooprifle.BloopRifle
import scala.concurrent.ExecutionContext

case class CompileServerStart(pre: Prebootstrapped, lazyResolver: Lazy[CoursierResolver]) extends BleepCommand {
  override def run(): Either[BuildException, Unit] =
    BleepConfig
      .rewritePersisted(pre.logger, pre.userPaths)(_.copy(compileServerMode = CompileServerMode.Shared))
      .flatMap { bleepConfig =>
        val threads = BloopThreads.create()
        val jvm = JvmCmd(pre.logger, bleepConfig.compileServerJvm, ExecutionContext.fromExecutorService(threads.jsonrpc))
        val rifleConfig = SetupBloopRifle(jvm, pre, lazyResolver, bleepConfig.compileServerMode)
        val rifleLogger = new BloopLogger(pre.logger)
        if (BloopRifle.check(rifleConfig, rifleLogger)) {
          Right(pre.logger.info("Compile server is already running"))
        } else {
          val tempDir = Files.createTempDirectory("bleep-bloop")
          BloopServer.buildServer(
            config = rifleConfig,
            clientName = "bleep",
            clientVersion = BleepVersion.version,
            workspace = tempDir,
            classesDir = tempDir / "classes",
            buildClient = BspClientDisplayProgress(pre.logger),
            threads = threads,
            logger = rifleLogger
          )
          Right(())
        }
      }
}
