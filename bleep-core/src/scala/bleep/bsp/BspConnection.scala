package bleep.bsp

import cats.effect.{Deferred, IO, Resource}

import java.io.{InputStream, OutputStream}
import java.net.Socket
import java.nio.channels.{Channels, SocketChannel}
import java.nio.file.Path

/** A connection to a BSP server.
  *
  * Provides access to the socket streams for JSON-RPC communication and a way to monitor server exit.
  */
trait BspConnection {

  /** Input stream for reading server responses */
  def input: InputStream

  /** Output stream for sending requests to server */
  def output: OutputStream

  /** Completes when the server process exits, with exit code */
  def serverExited: IO[Int]

  /** Close the connection */
  def close: IO[Unit]
}

object BspConnection {

  /** Create a connection from an open socket.
    *
    * @param s
    *   the connected socket
    * @param serverProcess
    *   optional process handle for monitoring exit
    * @return
    *   Resource that manages connection lifecycle
    */
  def fromSocket(
      s: Socket,
      serverProcess: Option[Process]
  ): Resource[IO, BspConnection] =
    Resource.make(
      for {
        exitDeferred <- Deferred[IO, Int]
        // Start background fiber to monitor process exit
        _ <- serverProcess match {
          case Some(proc) =>
            IO.blocking(proc.waitFor())
              .flatMap(code => exitDeferred.complete(code))
              .start
              .void
          case None =>
            // No process to monitor - will never complete
            IO.unit
        }
      } yield new BspConnectionFromSocket(s, exitDeferred)
    )(_.close)

  /** Create a connection without process monitoring.
    *
    * Use this when connecting to an existing server started by another process.
    */
  def fromSocketOnly(s: Socket): Resource[IO, BspConnection] =
    Resource.make(
      for {
        exitDeferred <- Deferred[IO, Int]
      } yield new BspConnectionFromSocket(s, exitDeferred)
    )(_.close)

  /** Create a connection from a SocketChannel (used for Unix domain sockets). */
  def fromChannel(channel: SocketChannel): Resource[IO, BspConnection] =
    Resource.make(
      for {
        exitDeferred <- Deferred[IO, Int]
      } yield new BspConnectionFromChannel(channel, exitDeferred)
    )(_.close)

  /** Create a connection from a SocketChannel with PID-based process monitoring. */
  def fromChannelWithPid(channel: SocketChannel, serverPid: Long): Resource[IO, BspConnection] =
    Resource.make(
      for {
        exitDeferred <- Deferred[IO, Int]
        // Start background fiber to monitor process by PID
        _ <- monitorPid(serverPid, exitDeferred).start.void
      } yield new BspConnectionFromChannel(channel, exitDeferred)
    )(_.close)

  /** Create a connection from a Socket with PID-based process monitoring. */
  def fromSocketWithPid(socket: Socket, serverPid: Long): Resource[IO, BspConnection] =
    Resource.make(
      for {
        exitDeferred <- Deferred[IO, Int]
        // Start background fiber to monitor process by PID
        _ <- monitorPid(serverPid, exitDeferred).start.void
      } yield new BspConnectionFromSocket(socket, exitDeferred)
    )(_.close)

  /** Monitor a process by PID and complete the deferred when it exits. */
  private def monitorPid(pid: Long, exitDeferred: Deferred[IO, Int]): IO[Unit] =
    IO.blocking {
      ProcessHandle.of(pid)
    }.flatMap {
      case opt if opt.isPresent =>
        // Use OS-level notification via CompletableFuture
        IO.fromCompletableFuture(IO.delay(opt.get().onExit())).flatMap { _ =>
          exitDeferred.complete(-1).void
        }
      case _ =>
        // Process already gone
        exitDeferred.complete(-1).void
    }

  private class BspConnectionFromSocket(
      socket: Socket,
      exitDeferred: Deferred[IO, Int]
  ) extends BspConnection {

    val input: InputStream = socket.getInputStream
    val output: OutputStream = socket.getOutputStream

    def serverExited: IO[Int] = exitDeferred.get

    def close: IO[Unit] = IO.blocking {
      try
        if (!socket.isClosed) {
          try socket.shutdownInput()
          catch { case _: Exception => () }
          try socket.shutdownOutput()
          catch { case _: Exception => () }
          socket.close()
        }
      catch {
        case _: Exception => ()
      }
    }
  }

  private class BspConnectionFromChannel(
      channel: SocketChannel,
      exitDeferred: Deferred[IO, Int]
  ) extends BspConnection {

    val input: InputStream = Channels.newInputStream(channel)
    val output: OutputStream = Channels.newOutputStream(channel)

    def serverExited: IO[Int] = exitDeferred.get

    def close: IO[Unit] = IO.blocking {
      try
        if (channel.isOpen) {
          channel.close()
        }
      catch {
        case _: Exception => ()
      }
    }
  }
}

/** Information about a running BSP server */
case class ServerInfo(
    pid: Long,
    socketPath: Path,
    version: String
)
