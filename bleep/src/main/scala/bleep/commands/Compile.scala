package bleep
package commands

import bleep.internal.MyBloopRifleLogger
import bleep.model.ProjectName
import cats.effect.{ExitCode, IO}
import ch.epfl.scala.bsp4j

import scala.build.bloop.{BloopServer, BloopThreads}
import scala.jdk.CollectionConverters._

case object Compile extends BleepCommand {
  object bspClient extends bsp4j.BuildClient {
    override def onBuildShowMessage(params: bsp4j.ShowMessageParams): Unit = println(params)
    override def onBuildLogMessage(params: bsp4j.LogMessageParams): Unit = println(params)
    override def onBuildTaskStart(params: bsp4j.TaskStartParams): Unit = println(params)
    override def onBuildTaskProgress(params: bsp4j.TaskProgressParams): Unit = println(params)
    override def onBuildTaskFinish(params: bsp4j.TaskFinishParams): Unit = println(params)
    override def onBuildPublishDiagnostics(params: bsp4j.PublishDiagnosticsParams): Unit = println(params)
    override def onBuildTargetDidChange(params: bsp4j.DidChangeBuildTarget): Unit = println(params)
  }

  override def run(): IO[ExitCode] =
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
          bspClient,
          BloopThreads.create(),
          new MyBloopRifleLogger(started.logger, true, true)
        ) { server =>
          def targetId(name: ProjectName): bsp4j.BuildTargetIdentifier =
            new bsp4j.BuildTargetIdentifier(started.buildPaths.buildDir.toFile.toURI.toASCIIString.stripSuffix("/") + "/?id=" + name.value)

          val targets = started.activeProject match {
            case Some(value) => List(targetId(value)).asJava
            case None        => started.build.projects.keys.map(targetId).toList.asJava
          }

          println(server.server.buildTargetCompile(new bsp4j.CompileParams(targets)).get())
        }
      }.as(ExitCode.Success)
    }
}

