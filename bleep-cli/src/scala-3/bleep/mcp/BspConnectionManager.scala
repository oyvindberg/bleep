package bleep.mcp

import bleep.Started
import bleep.bsp.{BspBuildData, BspRifle, BspRifleConfig, BspServerBuilder, BuildServer, BuildServerWithLifecycle}
import bleep.model
import cats.effect.std.Mutex
import cats.effect.{IO, Ref, Resource}

/** Owns the MCP session's connection to the bleep-bsp daemon and transparently reconnects when it dies.
  *
  * The daemon can go away underneath a long-lived MCP session: a new bleep snapshot spawns a daemon at a different socket dir, someone stops the server, or it
  * crashes. lsp4j never recovers on its own — once the connection is dead, every request on it fails instantly, forever. So every BSP call goes through
  * [[withServer]], which checks the connection's listening future before use and re-establishes daemon + socket + BSP initialize handshake when it has died.
  */
private[mcp] class BspConnectionManager private (
    bspConfig: BspRifleConfig,
    sharedClient: SharedMcpBspClient,
    currentStarted: () => Started,
    state: Ref[IO, BspConnectionManager.Connected],
    reconnectMutex: Mutex[IO]
) {

  /** Run `f` against a live BSP connection, reconnecting first if the current one is dead. */
  def withServer[T](f: (BuildServer, java.util.concurrent.Future[Void]) => IO[T]): IO[T] =
    connected.flatMap(c => f(c.lifecycle.server, c.lifecycle.listening))

  def shutdown: IO[Unit] =
    state.get.flatMap(_.release)

  private def connected: IO[BspConnectionManager.Connected] =
    state.get.flatMap { c =>
      if (c.lifecycle.listening.isDone) reconnect else IO.pure(c)
    }

  private def reconnect: IO[BspConnectionManager.Connected] =
    reconnectMutex.lock.surround {
      state.get.flatMap { c =>
        // another fiber may have finished reconnecting while we waited for the lock
        if (!c.lifecycle.listening.isDone) IO.pure(c)
        else {
          val started = currentStarted()
          for {
            _ <- IO(started.logger.warn("BSP server connection lost — reconnecting"))
            _ <- c.release.handleErrorWith(e => IO(started.logger.warn(s"Error while closing dead BSP connection: ${e.getMessage}")))
            next <- BspConnectionManager.connect(bspConfig, sharedClient, started)
            _ <- state.set(next)
            _ <- IO(started.logger.info("Reconnected to BSP server"))
          } yield next
        }
      }
    }
}

private[mcp] object BspConnectionManager {

  /** A live connection plus the finalizer that tears it down (closes socket, executor, trace writer). */
  final case class Connected(lifecycle: BuildServerWithLifecycle, release: IO[Unit])

  def resource(
      bspConfig: BspRifleConfig,
      sharedClient: SharedMcpBspClient,
      currentStarted: () => Started
  ): Resource[IO, BspConnectionManager] =
    Resource.make(
      for {
        first <- connect(bspConfig, sharedClient, currentStarted())
        state <- Ref.of[IO, Connected](first)
        mutex <- Mutex[IO]
      } yield new BspConnectionManager(bspConfig, sharedClient, currentStarted, state, mutex)
    )(_.shutdown)

  /** Ensure the daemon is running, connect, and run the BSP initialize handshake with the current build. */
  private def connect(bspConfig: BspRifleConfig, sharedClient: SharedMcpBspClient, started: Started): IO[Connected] =
    for {
      _ <- BspRifle.ensureRunning(bspConfig, started.logger)
      allocated <- BspRifle
        .connectWithRetry(bspConfig, started.logger)
        .flatMap(connection => BspServerBuilder.create(connection, sharedClient))
        .allocated
      (lifecycle, release) = allocated
      _ <- BspServerBuilder
        .initializeSession(
          server = lifecycle.server,
          clientName = "bleep-mcp",
          clientVersion = model.BleepVersion.current.value,
          rootUri = started.buildPaths.buildDir.toUri.toString,
          buildData = Some(BspBuildData.Payload.from(started)),
          listening = lifecycle.listening
        )
        .onError(_ => release.handleErrorWith(e => IO(started.logger.warn(s"Error while closing BSP connection after failed initialize: ${e.getMessage}"))))
    } yield Connected(lifecycle, release)
}
