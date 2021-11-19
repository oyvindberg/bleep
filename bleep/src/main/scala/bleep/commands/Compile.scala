package bleep
package commands

import bleep.internal.{BspClientDisplayProgress, MyBloopRifleLogger}
import bleep.model.ProjectName
import cats.effect.{ExitCode, IO}
import ch.epfl.scala.bsp4j

import scala.build.bloop.{BloopServer, BloopThreads}
import scala.jdk.CollectionConverters._

case object Compile extends BleepCommand {
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
          BspClientDisplayProgress(started.logger),
          BloopThreads.create(),
          new MyBloopRifleLogger(started.logger, true, true)
        ) { server =>
          def targetId(name: ProjectName): bsp4j.BuildTargetIdentifier =
            new bsp4j.BuildTargetIdentifier(started.buildPaths.buildDir.toFile.toURI.toASCIIString.stripSuffix("/") + "/?id=" + name.value)

          val targets = started.activeProject match {
            case Some(value) => List(targetId(value)).asJava
            case None        => started.build.projects.keys.map(targetId).toList.asJava
          }

          val result = server.server.buildTargetCompile(new bsp4j.CompileParams(targets)).get()

          result.getStatusCode match {
            case bsp4j.StatusCode.OK        => started.logger.info("Compilation succeeded")
            case bsp4j.StatusCode.ERROR     => started.logger.warn("Compilation failed")
            case bsp4j.StatusCode.CANCELLED => started.logger.warn("Compilation cancelled")
          }
        }
      }.as(ExitCode.Success)
    }
}
