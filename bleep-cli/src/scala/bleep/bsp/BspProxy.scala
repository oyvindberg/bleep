package bleep.bsp

import bleep._
import cats.effect.unsafe.implicits.global

object BspProxy {
  def run(pre: Prebootstrapped): ExitCode = {
    val config = BleepConfigOps.loadOrDefault(pre.userPaths).orThrow
    val build = pre.existingBuild.buildFile.forceGet.getOrElse {
      model.BuildFile(model.$schema, model.BleepVersion.current, model.JsonMap.empty, model.JsonMap.empty, model.JsonList.empty, model.JsonMap.empty, None)
    }
    val resolver = CoursierResolver.Factory.default(pre, config, build)
    val bspConfig = SetupBleepBsp(
      compileServerMode = config.compileServerModeOrDefault,
      config = config,
      resolvedJvm = pre.resolvedJvm.forceGet,
      userPaths = pre.userPaths,
      resolver = resolver,
      logger = pre.logger
    ).orThrow

    val program = BspRifle.ensureRunningAndConnect(bspConfig, pre.logger).use { connection =>
      cats.effect.IO.blocking {
        val stdinToServer = new Thread("bsp-stdin-to-server") {
          setDaemon(true)
          override def run(): Unit =
            try System.in.transferTo(connection.output)
            catch { case _: Exception => () }
            finally
              try connection.output.close()
              catch { case _: Exception => () }
        }
        val serverToStdout = new Thread("bsp-server-to-stdout") {
          setDaemon(true)
          override def run(): Unit =
            try connection.input.transferTo(System.out)
            catch { case _: Exception => () }
            finally
              try System.out.close()
              catch { case _: Exception => () }
        }

        stdinToServer.start()
        serverToStdout.start()

        // Wait for server output to finish (server disconnects or dies)
        serverToStdout.join()
        ExitCode.Success
      }
    }

    program.unsafeRunSync()
  }
}
