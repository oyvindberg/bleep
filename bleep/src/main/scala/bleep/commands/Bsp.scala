package bleep
package commands

import bleep.bsp.BspImpl
import bleep.internal.MyBloopRifleLogger

import scala.build.blooprifle.internal.Operations
import scala.concurrent.Await
import scala.concurrent.duration.Duration

case class Bsp(opts: CommonOpts, started: Started) extends BleepCommand {
  override def run(): Unit = {
    val bloopRifleConfig = new BloopSetup(
      JavaCmd.javacommand,
      started,
      bloopBspProtocol = Some("local")
    ).bloopRifleConfig

    bsp.BspThreads.withThreads { threads =>
      val bsp = new BspImpl(
        started.logger,
        bloopRifleConfig,
        buildPath = started.buildPaths.dotBleepDir,
        threads,
        System.in,
        System.out,
        ensureBloopUpToDate = () =>
          // run for side effects
          bootstrap.from(started.logger, started.buildPaths.buildDir)
      )

      try {
        val doneFuture = bsp.run()
        Await.result(doneFuture, Duration.Inf)
      } finally {
        bsp.shutdown()
        Operations.exit(
          bloopRifleConfig.address,
          started.buildPaths.dotBleepDir,
          System.in,
          System.out,
          System.err,
          new MyBloopRifleLogger(started.logger, true, true)
        )
        ()
      }
    }
  }
}
