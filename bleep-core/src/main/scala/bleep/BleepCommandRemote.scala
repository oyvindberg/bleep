package bleep

import bleep.bsp.{BloopLogger, CompileServerConfig, SetupBloopRifle}
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
    val maybeConfig = CompileServerConfig.load(started.userPaths)

    maybeConfig.foreach {
      case CompileServerConfig.NewEachInvocation =>
        started.logger.warn("TIP: run `bleep compile-server start` so you'll get a warm/fast compile server")
      case CompileServerConfig.Shared => ()
    }

    maybeConfig.flatMap { config =>
      val bloopRifleConfig: BloopRifleConfig =
        SetupBloopRifle(JavaCmd.javacommand, started.buildPaths, started.resolver, config.asAddress(started.userPaths))
      val buildClient: BspClientDisplayProgress =
        BspClientDisplayProgress(started.logger)
      val rifleLogger =
        new BloopLogger(started.logger)

      val server = BloopServer.buildServer(
        config = bloopRifleConfig,
        clientName = "bleep",
        clientVersion = constants.version,
        workspace = started.buildPaths.dotBleepModeDir,
        classesDir = started.buildPaths.dotBleepModeDir / "classes",
        buildClient = buildClient,
        threads = BloopThreads.create(),
        logger = rifleLogger
      )

      try
        runWithServer(server).flatMap { case () =>
          buildClient.failed match {
            case empty if empty.isEmpty => Right(())
            case failed =>
              Left(new BspCommandFailed("Failed", failed.map(projectFromBuildTarget).toList, bsp4j.StatusCode.ERROR))
          }
        }
      finally
        config match {
          case CompileServerConfig.NewEachInvocation =>
            server.shutdown()
            Operations.exit(bloopRifleConfig.address, started.buildPaths.dotBleepDir, System.out, System.err, rifleLogger)
            ()
          case CompileServerConfig.Shared =>
            ()
        }
    }
  }
}
