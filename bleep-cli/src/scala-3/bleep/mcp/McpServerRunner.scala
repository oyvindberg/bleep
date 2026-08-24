package bleep.mcp

import bleep._
import cats.effect.IO
import cats.effect.unsafe.implicits.global
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
  */
object McpServerRunner {

  def run(logger: Logger, userPaths: UserPaths, ec: ExecutionContext): bleep.ExitCode = {
    val server = new BleepMcpServer(logger, userPaths, ec)
    val program = server
      .start(
        StdioJsonRpcConnection.create[IO],
        e => IO(logger.error(s"MCP server error: $e", e))
      )
      .useForever
      .as(bleep.ExitCode.Success)

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
}
