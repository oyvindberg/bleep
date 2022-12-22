package bleep

import bleep.bsp.{BleepRifleLogger, BspCommandFailed, SetupBloopRifle}
import bleep.internal.{fatal, jvmOrSystem, BspClientDisplayProgress}
import ch.epfl.scala.bsp4j

import java.util
import scala.build.bloop.{BloopServer, BloopThreads}
import scala.build.blooprifle.BloopRifleConfig
import scala.build.blooprifle.internal.Operations
import scala.jdk.CollectionConverters._

abstract class BleepCommandRemote(watch: Boolean) extends BleepBuildCommand {
  def chosenProjects(started: Started): Array[model.CrossProjectName]

  def buildTarget(buildPaths: BuildPaths, name: model.CrossProjectName): bsp4j.BuildTargetIdentifier =
    new bsp4j.BuildTargetIdentifier(buildPaths.buildVariantDir.toFile.toURI.toASCIIString.stripSuffix("/") + "/?id=" + name.value)

  def projectFromBuildTarget(started: Started)(name: bsp4j.BuildTargetIdentifier): model.CrossProjectName = {
    val id = name.getUri.split("=").last
    started.build.explodedProjects.keys.find(_.value == id).getOrElse(sys.error(s"Couldn't find project for $name"))
  }

  def buildTargets(buildPaths: BuildPaths, projects: Array[model.CrossProjectName]): util.List[bsp4j.BuildTargetIdentifier] =
    util.List.of(projects.map(p => buildTarget(buildPaths, p)): _*)

  def runWithServer(started: Started, bloop: BloopServer): Either[BleepException, Unit]

  override final def run(started: Started): Either[BleepException, Unit] = {
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
      workspace = started.buildPaths.buildVariantDir,
      classesDir = started.buildPaths.buildVariantDir / "classes",
      buildClient = buildClient,
      threads = BloopThreads.create(),
      logger = bleepRifleLogger
    )

    try
      if (watch) {
        // run once initially
        runWithServer(started, server)

        var currentStarted = started

        val codeWatcher = BleepFileWatching.projects(currentStarted, chosenProjects(currentStarted)) { changedProjects =>
          val patchedCmd = this match {
            case x: BleepCommandRemote.OnlyChanged => x.onlyChangedProjects(currentStarted, changedProjects)
            case other                             => other
          }

          patchedCmd.runWithServer(currentStarted, server)
          ()
        }

        val buildWatcher = BleepFileWatching.build(started.logger, started.prebootstrapped.existingBuild) { case () =>
          started.reloaded match {
            case Left(bleepException) =>
              fatal.log("build changed, but it didn't work :(", started.logger, bleepException)
              codeWatcher.updateMapping(Map.empty)
            case Right(newStarted) =>
              currentStarted = newStarted
              codeWatcher.updateMapping(BleepFileWatching.projectPathsMapping(currentStarted, chosenProjects(currentStarted)))
          }
        }

        started.logger.info("Running in watch mode")

        codeWatcher.combine(buildWatcher).run(FileWatching.StopWhen.OnStdInput)

        Right(())

      } else
        runWithServer(started, server).flatMap { case () =>
          buildClient.failed match {
            case empty if empty.isEmpty => Right(())
            case failed =>
              Left(new BspCommandFailed("Failed", failed.map(projectFromBuildTarget(started)).toArray, BspCommandFailed.NoDetails))
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

  def discoverMain(started: Started, bloop: BloopServer, project: model.CrossProjectName): Either[BleepException, String] = {
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
  trait OnlyChanged {
    self: BleepCommandRemote =>
    def onlyChangedProjects(started: Started, isChanged: model.CrossProjectName => Boolean): BleepCommandRemote
  }

  case class AmbiguousMain(mainClasses: Seq[String])
      extends BleepException(
        s"Discovered more than one main class, so you need to specify which one you want with `--class ...`. ${mainClasses.map(fansi.Color.Magenta(_)).mkString("\n", "\n, ", "\n")}"
      )

  case class NoMain() extends BleepException(s"No main class found. Specify which one you want with `--class ...`")

}
