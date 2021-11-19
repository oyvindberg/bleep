package bleep.commands

import bleep.{BleepCommand, BloopSetup, bootstrap, bsp}
import bleep.bsp.BspImpl
import cats.effect.{ExitCode, IO}

import scala.concurrent.Await
import scala.concurrent.duration.Duration

case object Bsp extends BleepCommand {
  override def run(): IO[ExitCode] =
    runWithEnv { started =>
      IO {
        val bloopRifleConfig = new BloopSetup(
          "/usr/bin/java",
          started,
          bloopBspProtocol = None,
          bloopBspSocket = None
        ).bloopRifleConfig

        bsp.BspThreads.withThreads { threads =>
          val bsp = new BspImpl(
            started.logger,
            bloopRifleConfig,
            buildPath = started.buildPaths.dotBleepDir,
            threads,
            System.in,
            System.out,
            ensureBloopUpToDate = () => {
              // run for side effects
              bootstrap.fromCwd
              ()
            }
          )

          try {
            val doneFuture = bsp.run()
            Await.result(doneFuture, Duration.Inf)
          } finally bsp.shutdown()
        }
        ExitCode.Success
      }
    }
}
