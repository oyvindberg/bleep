package bleep.mcp

import bleep._
import cats.effect.{Deferred, IO, Resource}
import cats.effect.unsafe.implicits.global
import ch.linkyard.mcp.jsonrpc2.JsonRpcConnection
import ch.linkyard.mcp.jsonrpc2.transport.StdioJsonRpcConnection
import ryddig.Logger

import scala.concurrent.ExecutionContext

/** Entry point for the MCP server. Runs on stdio.
  *
  * Deliberately workspace-free: no build is loaded at boot, so the server starts from any directory — every tool call names its workspace and bootstraps it
  * fresh. This is what lets one user-scoped MCP registration serve every checkout and git worktree.
  *
  * Runs the server exactly once and exits when the connection ends — cleanly on client shutdown, nonzero on a crash. There is deliberately no in-process
  * restart: on stdio the client holds the session state and will not re-send `initialize` to a restarted server, so a respawned instance on the same pipes
  * would reject every subsequent request while the process looks healthy — and if the pipes themselves are dead (client gone, binary replaced underneath us), a
  * restart loop just spins forever as an orphan. Exiting is the honest signal: the client sees the disconnect and relaunches a fresh process.
  *
  * "When the connection ends" is detected here, not by linkyard: its `JsonRpcServer.start` merges the input with the server's outgoing stream, which never
  * completes, and `McpServer.start` then runs that forever. So a closed stdin, which is how a client that is done with us says so, went unnoticed, and every
  * ended session left a server behind. Sixty of them had piled up on one machine, the oldest weeks old, each still spawning daemons on its old binary.
  */
object McpServerRunner {

  def run(logger: Logger, userPaths: UserPaths, ec: ExecutionContext): bleep.ExitCode = {
    val server = new BleepMcpServer(logger, userPaths, ec)
    val program = for {
      inputEnded <- Deferred[IO, Either[Throwable, Unit]]
      _ <- server
        .start(
          endingWith(StdioJsonRpcConnection.create[IO], inputEnded),
          e => IO(logger.error(s"MCP server error: $e", e))
        )
        .use(_ => inputEnded.get.rethrow)
      _ <- IO(logger.info("MCP client closed stdin, exiting"))
    } yield bleep.ExitCode.Success

    try {
      program.unsafeRunSync(): Unit
      bleep.ExitCode.Success
    } catch {
      case _: InterruptedException =>
        bleep.ExitCode.Success
      case ex: Exception =>
        logger.error(s"MCP server crashed, exiting so the client can relaunch a fresh process: ${ex.getMessage}", ex)
        bleep.ExitCode.Failure
    }
  }

  /** `connection`, whose input completes `ended` once it stops: `Right` at end of input, `Left` when reading failed. Either way no request will arrive again.
    */
  private[mcp] def endingWith(connection: JsonRpcConnection[IO], ended: Deferred[IO, Either[Throwable, Unit]]): JsonRpcConnection[IO] =
    new JsonRpcConnection[IO] {
      override def info: JsonRpcConnection.Info = connection.info
      override def out: fs2.Pipe[IO, ch.linkyard.mcp.jsonrpc2.JsonRpc.Message, Unit] = connection.out
      override def in: fs2.Stream[IO, ch.linkyard.mcp.jsonrpc2.JsonRpc.MessageEnvelope] =
        connection.in.onFinalizeCase {
          case Resource.ExitCase.Succeeded  => ended.complete(Right(())).void
          case Resource.ExitCase.Errored(e) => ended.complete(Left(e)).void
          // Cancelled means we are already shutting down, from the other side.
          case Resource.ExitCase.Canceled => IO.unit
        }
    }
}
