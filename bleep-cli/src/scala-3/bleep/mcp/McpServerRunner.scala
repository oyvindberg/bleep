package bleep.mcp

import bleep._
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import ch.linkyard.mcp.jsonrpc2.transport.StdioJsonRpcConnection

/** Entry point for the MCP server. Bootstraps the build, creates the MCP server, and runs on stdio.
  *
  * Automatically restarts the server if it crashes, with exponential backoff up to 30 seconds. Interrupted exceptions
  * (clean shutdown) are not retried.
  */
object McpServerRunner {

  private val InitialBackoffMs: Long = 1000
  private val MaxBackoffMs: Long = 30000

  def run(pre: Prebootstrapped): bleep.ExitCode = {
    val config = BleepConfigOps.loadOrDefault(pre.userPaths).orThrow

    bootstrap.from(pre, ResolveProjects.InMemory, rewrites = Nil, config, CoursierResolver.Factory.default) match {
      case Left(err) =>
        pre.logger.error(s"Failed to load build: ${err.getMessage}")
        bleep.ExitCode.Failure
      case Right(started) =>
        runWithRestart(pre, started)
    }
  }

  private def runWithRestart(pre: Prebootstrapped, started: Started): bleep.ExitCode = {
    var backoffMs = InitialBackoffMs

    while (true) {
      val server = new BleepMcpServer(started)
      val program = server
        .start(
          StdioJsonRpcConnection.create[IO],
          e => IO(started.logger.error(s"MCP server error: $e", e))
        )
        .useForever
        .as(bleep.ExitCode.Success)

      try {
        program.unsafeRunSync()
        return bleep.ExitCode.Success
      } catch {
        case _: InterruptedException =>
          return bleep.ExitCode.Success
        case ex: Exception =>
          pre.logger.error(s"MCP server crashed, restarting in ${backoffMs}ms: ${ex.getMessage}")
          Thread.sleep(backoffMs)
          backoffMs = math.min(backoffMs * 2, MaxBackoffMs)
      }
    }

    bleep.ExitCode.Success // unreachable, satisfies compiler
  }
}
