package bleep

import bleep.bsp.{BleepRifleLogger, BspCommandFailed, SetupBloopRifle}
import bleep.internal.{BspClientDisplayProgress, Throwables, TransitiveProjects}
import bloop.rifle.internal.Operations
import bloop.rifle.*
import ch.epfl.scala.bsp4j

import java.nio.file.Files
import java.util
import scala.util.Try

abstract class BleepCommandRemote(watch: Boolean) extends BleepBuildCommand {
  def watchableProjects(started: Started): TransitiveProjects

  def runWithServer(started: Started, bloop: BuildServer): Either[BleepException, Unit]

  override final def run(started: Started): Either[BleepException, Unit] =
    BleepCommandRemote.withServer(started) { (server, buildClient) =>
      if (watch) {
        // run once initially
        runWithServer(started, server)

        var currentStarted = started

        val codeWatcher = BleepFileWatching.projects(currentStarted, watchableProjects(currentStarted)) { changedProjects =>
          val patchedCmd = this match {
            case x: BleepCommandRemote.OnlyChanged => x.onlyChangedProjects(currentStarted, changedProjects)
            case other                             => other
          }

          patchedCmd.runWithServer(currentStarted, server) match {
            case Left(bleepException) => currentStarted.logger.error(Throwables.messagesFrom(bleepException).mkString(": "))
            case Right(())            => ()
          }
          ()
        }

        val buildWatcher = BleepFileWatching.build(started.pre) { _ =>
          started.reloadFromDisk() match {
            case Left(bleepException) =>
              Throwables.log("build changed, but it didn't work :(", started.logger, bleepException)
              codeWatcher.updateMapping(Map.empty)
            case Right(None) =>
              ()
            case Right(Some(newStarted)) =>
              currentStarted = newStarted
              codeWatcher.updateMapping(BleepFileWatching.projectPathsMapping(currentStarted, watchableProjects(currentStarted)))
          }
        }

        started.logger.info("Running in watch mode")

        codeWatcher.combine(buildWatcher).run(FileWatching.StopWhen.OnStdInput)

        Right(())
      } else {
        for {
          _ <- runWithServer(started, server)
          res <- buildClient.failed match {
            case empty if empty.isEmpty => Right(())
            case failed =>
              val projects = failed.flatMap(BleepCommandRemote.projectFromBuildTarget(started)).toArray
              Left(new BspCommandFailed("Failed", projects, BspCommandFailed.NoDetails))
          }
        } yield res
      }

    }
}

object BleepCommandRemote {
  trait OnlyChanged {
    self: BleepCommandRemote =>
    def onlyChangedProjects(started: Started, isChanged: model.CrossProjectName => Boolean): BleepCommandRemote
  }

  def withServer[T](started: Started)(run: (BuildServer, BspClientDisplayProgress) => Either[BleepException, T]): Either[BleepException, T] = {
    started.config.compileServerModeOrDefault match {
      case model.CompileServerMode.NewEachInvocation =>
        started.logger.warn("TIP: run `bleep config compile-server auto-shutdown-disable` so you'll get a warm/fast compile server")
      case model.CompileServerMode.Shared => ()
    }

    val bleepRifleLogger = new BleepRifleLogger(started.logger)
    val bloopRifleConfig: BloopRifleConfig =
      SetupBloopRifle(
        started.config.compileServerModeOrDefault,
        started.resolvedJvm.forceGet,
        started.pre.userPaths,
        started.resolver,
        bleepRifleLogger
      )

    val buildClient: BspClientDisplayProgress =
      BspClientDisplayProgress(started.logger)

    val server =
      try
        BloopServer.buildServer(
          config = bloopRifleConfig,
          clientName = "bleep",
          clientVersion = model.BleepVersion.current.value,
          workspace = started.buildPaths.buildVariantDir,
          classesDir = started.buildPaths.buildVariantDir / "classes",
          buildClient = buildClient,
          threads = BloopThreads.create(),
          logger = bleepRifleLogger
        )
      catch {
        case th: FailedToStartServerException =>
          val readLog: Option[String] =
            bloopRifleConfig.address match {
              case _: BloopRifleConfig.Address.Tcp           => None
              case ds: BloopRifleConfig.Address.DomainSocket => Try(Files.readString(ds.outputPath)).toOption
            }
          throw BleepCommandRemote.FailedToStartBloop(th, readLog)
      }

    try run(server.server, buildClient)
    finally
      started.config.compileServerModeOrDefault match {
        case model.CompileServerMode.NewEachInvocation =>
          server.shutdown()
          if (Operations.exit(bloopRifleConfig.address, started.buildPaths.dotBleepDir, System.out, System.err, bleepRifleLogger) != 0) {
            started.logger.warn("Failed to shutdown the compile server")
          }
          ()
        case model.CompileServerMode.Shared =>
          ()
      }
  }

  case class FailedToStartBloop(cause: FailedToStartServerException, readLog: Option[String])
      extends BleepException(
        readLog.foldLeft(cause.getMessage)((msg, log) => s"$msg\nRead log file:\n$log")
      )

  def buildTarget(buildPaths: BuildPaths, name: model.CrossProjectName): bsp4j.BuildTargetIdentifier = {
    val id = buildPaths.buildVariantDir.toFile.toURI.toASCIIString.stripSuffix("/") + "/?id=" + name.value
    // Applying the same format as bloop. There might be a better way to do this.
    val amended = id.replace("file:///", "file:/")
    new bsp4j.BuildTargetIdentifier(amended)
  }

  def projectFromBuildTarget(started: Started)(name: bsp4j.BuildTargetIdentifier): Option[model.CrossProjectName] =
    started.build.explodedProjects.keys.find(_.value == name.getUri.split("=").last)

  def buildTargets(buildPaths: BuildPaths, projects: Array[model.CrossProjectName]): util.List[bsp4j.BuildTargetIdentifier] =
    util.List.of(projects.map(p => buildTarget(buildPaths, p)): _*)
}
