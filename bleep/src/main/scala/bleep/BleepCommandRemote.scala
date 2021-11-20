package bleep

import bleep.internal.{BspClientDisplayProgress, MyBloopRifleLogger}
import bleep.model.ProjectName
import cats.effect.{ExitCode, IO}
import ch.epfl.scala.bsp4j
import ch.epfl.scala.bsp4j.BuildTargetIdentifier

import java.util
import scala.build.bloop.{BloopServer, BloopThreads}
import scala.jdk.CollectionConverters._

trait BleepCommandRemote extends BleepCommand {
  def targetId(buildPaths: BuildPaths, name: ProjectName): bsp4j.BuildTargetIdentifier =
    new bsp4j.BuildTargetIdentifier(buildPaths.buildDir.toFile.toURI.toASCIIString.stripSuffix("/") + "/?id=" + name.value)

  def chosenTargets(started: Started): util.List[BuildTargetIdentifier] =
    started.chosenProjects.map(projectName => targetId(started.buildPaths, projectName)).asJava

  def runWithServer(started: Started, bloop: BloopServer): Unit

  override final def run(): IO[ExitCode] =
    runWithEnv { started: Started =>
      val bloopRifleConfig = new BloopSetup(
        "/usr/bin/java",
        started,
        bloopBspProtocol = None,
        bloopBspSocket = None
      ).bloopRifleConfig

      IO {
        BloopServer.withBuildServer(
          bloopRifleConfig,
          "bleep",
          Defaults.version,
          started.buildPaths.dotBleepDir,
          started.buildPaths.dotBleepDir / "classes",
          BspClientDisplayProgress(started.logger),
          BloopThreads.create(),
          new MyBloopRifleLogger(started.logger, true, true)
        )(bloop => runWithServer(started, bloop))
      }.as(ExitCode.Success)
    }
}
