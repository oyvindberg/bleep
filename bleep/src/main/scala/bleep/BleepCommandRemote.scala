package bleep

import bleep.internal.{BspClientDisplayProgress, MyBloopRifleLogger}
import bleep.model.ProjectName
import cats.data.NonEmptyList
import ch.epfl.scala.bsp4j

import java.util
import scala.build.bloop.{BloopServer, BloopThreads}

trait BleepCommandRemote extends BleepCommand {
  def started: Started

  def chosenTargets(started: Started, fromCommandLine: Option[NonEmptyList[model.ProjectName]]): util.List[bsp4j.BuildTargetIdentifier] =
    buildTargets(started.buildPaths, started.chosenProjects(fromCommandLine))

  def buildTargets(buildPaths: BuildPaths, projects: List[model.ProjectName]): util.List[bsp4j.BuildTargetIdentifier] = {
    def targetId(buildPaths: BuildPaths, name: ProjectName): bsp4j.BuildTargetIdentifier =
      new bsp4j.BuildTargetIdentifier(buildPaths.buildDir.toFile.toURI.toASCIIString.stripSuffix("/") + "/?id=" + name.value)

    import scala.jdk.CollectionConverters._

    projects.map(projectName => targetId(buildPaths, projectName)).asJava
  }

  def runWithServer(bloop: BloopServer): Unit

  override final def run(): Unit = {
    val bloopRifleConfig = new BloopSetup(
      JavaCmd.javacommand,
      started,
      bloopBspProtocol = None,
      bloopBspSocket = None
    ).bloopRifleConfig

    BloopServer.withBuildServer(
      bloopRifleConfig,
      "bleep",
      Defaults.version,
      started.buildPaths.dotBleepDir,
      started.buildPaths.dotBleepDir / "classes",
      BspClientDisplayProgress(started.logger),
      BloopThreads.create(),
      new MyBloopRifleLogger(started.logger, true, true)
    )(runWithServer)
  }
}
