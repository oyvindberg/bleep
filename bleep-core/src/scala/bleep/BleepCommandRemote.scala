package bleep

import bleep.bsp.{BleepRifleLogger, CompileServerMode, SetupBloopRifle}
import bleep.bsp.BspCommandFailed
import bleep.internal.BspClientDisplayProgress
import ch.epfl.scala.bsp4j

import java.util
import scala.build.bloop.{BloopServer, BloopThreads}
import scala.build.blooprifle.BloopRifleConfig
import scala.build.blooprifle.internal.Operations
import scala.jdk.CollectionConverters._

abstract class BleepCommandRemote(started: Started) extends BleepCommand {
  def buildTarget(buildPaths: BuildPaths, name: model.CrossProjectName): bsp4j.BuildTargetIdentifier =
    new bsp4j.BuildTargetIdentifier(buildPaths.dotBleepModeDir.toFile.toURI.toASCIIString.stripSuffix("/") + "/?id=" + name.value)

  def projectFromBuildTarget(name: bsp4j.BuildTargetIdentifier): model.CrossProjectName = {
    val id = name.getUri.split("=").last
    started.build.projects.keys.find(_.value == id).getOrElse(sys.error(s"Couldn't find project for $name"))
  }

  def buildTargets(buildPaths: BuildPaths, projects: Seq[model.CrossProjectName]): util.List[bsp4j.BuildTargetIdentifier] =
    projects.map(p => buildTarget(buildPaths, p)).asJava

  def runWithServer(bloop: BloopServer): Either[BuildException, Unit]

  override final def run(): Either[BuildException, Unit] = {
    val bleepConfig = started.lazyConfig.forceGet

    bleepConfig.compileServerMode match {
      case CompileServerMode.NewEachInvocation =>
        started.logger.warn("TIP: run `bleep compile-server start` so you'll get a warm/fast compile server")
      case CompileServerMode.Shared => ()
    }

    val bleepRifleLogger = new BleepRifleLogger(started.logger)
    val bloopRifleConfig: BloopRifleConfig =
      SetupBloopRifle(bleepConfig, started.logger, started.prebootstrapped.userPaths, started.resolver, bleepRifleLogger, started.executionContext)
    val buildClient: BspClientDisplayProgress =
      BspClientDisplayProgress(started.logger)

    val server = BloopServer.buildServer(
      config = bloopRifleConfig,
      clientName = "bleep",
      clientVersion = BleepVersion.version,
      workspace = started.buildPaths.dotBleepModeDir,
      classesDir = started.buildPaths.dotBleepModeDir / "classes",
      buildClient = buildClient,
      threads = BloopThreads.create(),
      logger = bleepRifleLogger
    )

    try
      runWithServer(server).flatMap { case () =>
        buildClient.failed match {
          case empty if empty.isEmpty => Right(())
          case failed =>
            Left(new BspCommandFailed("Failed", failed.map(projectFromBuildTarget).toList, BspCommandFailed.NoDetails))
        }
      }
    finally
      bleepConfig.compileServerMode match {
        case CompileServerMode.NewEachInvocation =>
          server.shutdown()
          Operations.exit(bloopRifleConfig.address, started.buildPaths.dotBleepDir, System.out, System.err, bleepRifleLogger)
          ()
        case CompileServerMode.Shared =>
          ()
      }
  }
}
