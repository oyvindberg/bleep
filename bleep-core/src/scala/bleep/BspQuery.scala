package bleep

import bleep.bsp.{BspConnection, BspRifle, BspServerBuilder, BspServerClasspathSource, BuildServer, SetupBleepBsp}
import bleep.internal.BspClientDisplayProgress
import cats.effect.{IO, Resource}
import cats.effect.unsafe.implicits.global
import ch.epfl.scala.bsp4j

import java.util
import scala.util.Try

/** Utility for running brief queries against the BSP server.
  *
  * Used by commands that need a BSP connection for something other than compile (e.g., main class discovery). The server should already be running (e.g., after
  * a ReactiveBsp compile step).
  */
object BspQuery {

  /** Connect to the BSP server, run a query, and disconnect. */
  def withServer[A](started: Started)(f: BuildServer => Either[BleepException, A]): Either[BleepException, A] = {
    val (connectionResource, traceFile): (Resource[IO, BspConnection], Option[java.nio.file.Path]) =
      started.bspServerClasspathSource match {
        case BspServerClasspathSource.InProcess(connect) =>
          (connect(started.logger), None)
        case BspServerClasspathSource.FromCoursier(resolver) =>
          SetupBleepBsp(
            compileServerMode = started.config.compileServerModeOrDefault,
            config = started.config,
            resolvedJvm = started.resolvedJvm.forceGet,
            userPaths = started.pre.userPaths,
            resolver = resolver,
            logger = started.logger,
            extraServerClasspath = Seq.empty
          ) match {
            case Left(err)     => return Left(err)
            case Right(config) => (BspRifle.ensureRunningAndConnect(config, started.logger), config.traceFile)
          }
      }

    val program: IO[Either[BleepException, A]] = connectionResource.use { connection =>
      val client = BspClientDisplayProgress(started.logger)
      BspServerBuilder.create(connection, client, traceFile).use { lifecycle =>
        val server = lifecycle.server
        BspServerBuilder
          .initializeSession(
            server = server,
            clientName = "bleep",
            clientVersion = model.BleepVersion.current.value,
            rootUri = started.buildPaths.buildDir.toUri.toString,
            buildData = None,
            listening = lifecycle.listening
          )
          .flatMap { _ =>
            IO.blocking(f(server))
          }
          .flatTap { _ =>
            IO.blocking(Try(server.buildShutdown().get())).attempt.void >>
              IO.blocking {
                val oldErr = System.err
                System.setErr(new java.io.PrintStream(new java.io.OutputStream { def write(b: Int): Unit = () }))
                try
                  Try(server.onBuildExit())
                finally
                  System.setErr(oldErr)
              }.attempt
                .void
          }
      }
    }

    try
      program.unsafeRunSync()
    catch {
      case ex: Exception => Left(new BleepException.Cause(ex, "BSP query failed"))
    }
  }

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
