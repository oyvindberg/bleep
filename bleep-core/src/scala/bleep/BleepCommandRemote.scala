package bleep

import bleep.bsp.{BleepRifleLogger, BspCommandFailed, SetupBloopRifle}
import bleep.internal.{jvmOrSystem, BspClientDisplayProgress}
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
    started.build.explodedProjects.keys.find(_.value == id).getOrElse(sys.error(s"Couldn't find project for $name"))
  }

  def buildTargets(buildPaths: BuildPaths, projects: Seq[model.CrossProjectName]): util.List[bsp4j.BuildTargetIdentifier] =
    projects.map(p => buildTarget(buildPaths, p)).asJava

  def runWithServer(bloop: BloopServer): Either[BleepException, Unit]

  override final def run(): Either[BleepException, Unit] = {
    val bleepConfig = started.lazyConfig.forceGet

    bleepConfig.compileServerMode match {
      case model.CompileServerMode.NewEachInvocation =>
        started.logger.warn("TIP: run `bleep compile-server auto-shutdown-disable` so you'll get a warm/fast compile server")
      case model.CompileServerMode.Shared => ()
    }

    val bloopJvm = jvmOrSystem(started)

    val bleepRifleLogger = new BleepRifleLogger(started.logger)
    val bloopRifleConfig: BloopRifleConfig =
      SetupBloopRifle(
        bleepConfig.compileServerMode,
        bloopJvm,
        started.logger,
        started.prebootstrapped.userPaths,
        started.resolver,
        bleepRifleLogger,
        started.executionContext
      )
    val buildClient: BspClientDisplayProgress =
      BspClientDisplayProgress(started.logger)

    val server = BloopServer.buildServer(
      config = bloopRifleConfig,
      clientName = "bleep",
      clientVersion = model.BleepVersion.current.value,
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
            Left(new BspCommandFailed("Failed", failed.map(projectFromBuildTarget).toArray, BspCommandFailed.NoDetails))
        }
      }
    finally
      bleepConfig.compileServerMode match {
        case model.CompileServerMode.NewEachInvocation =>
          server.shutdown()
          Operations.exit(bloopRifleConfig.address, started.buildPaths.dotBleepDir, System.out, System.err, bleepRifleLogger)
          ()
        case model.CompileServerMode.Shared =>
          ()
      }
  }

  def discoverMain(bloop: BloopServer, project: model.CrossProjectName): Either[BleepException, String] = {
    val req = new bsp4j.ScalaMainClassesParams(util.List.of[bsp4j.BuildTargetIdentifier](buildTarget(started.buildPaths, project)))
    started.logger.debug(req.toString)

    val res: bsp4j.ScalaMainClassesResult =
      bloop.server.buildTargetScalaMainClasses(req).get()

    started.logger.debug(res.toString)

    res.getItems.asScala.flatMap(_.getClasses.asScala).map(_.getClassName).toList match {
      case Nil       => Left(BleepCommandRemote.NoMain())
      case List(one) => Right(one)
      case many      => Left(BleepCommandRemote.AmbiguousMain(many))
    }
  }
}

object BleepCommandRemote {
  case class AmbiguousMain(mainClasses: Seq[String])
      extends BleepException(
        s"Discovered more than one main class, so you need to specify which one you want with `--class ...`. ${mainClasses.map(fansi.Color.Magenta(_)).mkString("\n", "\n, ", "\n")}"
      )

  case class NoMain() extends BleepException(s"No main class found. Specify which one you want with `--class ...`")

}
