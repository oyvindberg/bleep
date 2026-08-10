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
  * Automatically restarts the server if it crashes, with exponential backoff up to 30 seconds. Interrupted exceptions (clean shutdown) are not retried.
  */
object McpServerRunner {

  private val InitialBackoffMs: Long = 1000
  private val MaxBackoffMs: Long = 30000

  def run(logger: Logger, userPaths: UserPaths, ec: ExecutionContext): bleep.ExitCode = {
    var backoffMs = InitialBackoffMs

    while (true) {
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
        return bleep.ExitCode.Success
      } catch {
        case _: InterruptedException =>
          return bleep.ExitCode.Success
        case ex: Exception =>
          logger.error(s"MCP server crashed, restarting in ${backoffMs}ms: ${ex.getMessage}")
          Thread.sleep(backoffMs)
          backoffMs = math.min(backoffMs * 2, MaxBackoffMs)
      }
    }

    bleep.ExitCode.Success // unreachable, satisfies compiler
  }
}
