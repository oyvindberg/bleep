package bleep

import bleep.bsp.{BleepRifleLogger, BspCommandFailed, SetupBloopRifle}
import bleep.internal.{BspClientDisplayProgress, Throwables, TransitiveProjects}
import ch.epfl.scala.bsp4j

import java.nio.file.Files
import java.util
import scala.build.bloop.{BloopServer, BloopThreads, BuildServer}
import scala.build.blooprifle.internal.Operations
import scala.build.blooprifle.{BloopRifleConfig, FailedToStartServerException}
import scala.util.Try

abstract class BleepCommandRemote(watch: Boolean) extends BleepBuildCommand {
  def watchableProjects(started: Started): TransitiveProjects

  def runWithServer(started: Started, bloop: BuildServer): Either[BleepException, Unit]

  override final def run(started: Started): Either[BleepException, Unit] = {
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

    try
      if (watch) {
        // run once initially
        runWithServer(started, server.server)

        var currentStarted = started

        val codeWatcher = BleepFileWatching.projects(currentStarted, watchableProjects(currentStarted)) { changedProjects =>
          val patchedCmd = this match {
            case x: BleepCommandRemote.OnlyChanged => x.onlyChangedProjects(currentStarted, changedProjects)
            case other                             => other
          }

          patchedCmd.runWithServer(currentStarted, server.server) match {
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
          _ <- runWithServer(started, server.server)
          res <- buildClient.failed match {
            case empty if empty.isEmpty => Right(())
            case failed =>
              val projects = failed.map(BleepCommandRemote.projectFromBuildTarget(started)).toArray
              Left(new BspCommandFailed("Failed", projects, BspCommandFailed.NoDetails))
          }
        } yield res
      }
    finally
      started.config.compileServerModeOrDefault match {
        case model.CompileServerMode.NewEachInvocation =>
          server.shutdown()
          Operations.exit(bloopRifleConfig.address, started.buildPaths.dotBleepDir, System.out, System.err, bleepRifleLogger)
          ()
        case model.CompileServerMode.Shared =>
          ()
      }
  }

}

object BleepCommandRemote {
  trait OnlyChanged {
    self: BleepCommandRemote =>
    def onlyChangedProjects(started: Started, isChanged: model.CrossProjectName => Boolean): BleepCommandRemote
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

  def projectFromBuildTarget(started: Started)(name: bsp4j.BuildTargetIdentifier): model.CrossProjectName = {
    val id = name.getUri.split("=").last
    started.build.explodedProjects.keys.find(_.value == id).getOrElse(sys.error(s"Couldn't find project for $name"))
  }

  def buildTargets(buildPaths: BuildPaths, projects: Array[model.CrossProjectName]): util.List[bsp4j.BuildTargetIdentifier] =
    util.List.of(projects.map(p => buildTarget(buildPaths, p)): _*)
}
