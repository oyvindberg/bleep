package bleep.mcp

import bleep._
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import ch.linkyard.mcp.jsonrpc2.transport.StdioJsonRpcConnection

/** Entry point for the MCP server. Bootstraps the build, creates the MCP server, and runs on stdio. */
object McpServerRunner {

  def run(pre: Prebootstrapped): bleep.ExitCode = {
    val config = BleepConfigOps.loadOrDefault(pre.userPaths).orThrow

    bootstrap.from(pre, ResolveProjects.InMemory, rewrites = Nil, config, CoursierResolver.Factory.default) match {
      case Left(err) =>
        pre.logger.error(s"Failed to load build: ${err.getMessage}")
        bleep.ExitCode.Failure
      case Right(started) =>
        val server = new BleepMcpServer(started)
        val program = server
          .start(
            StdioJsonRpcConnection.create[IO],
            e => IO(started.logger.error(s"MCP server error: $e", e))
          )
          .useForever
          .as(bleep.ExitCode.Success)

        try
          program.unsafeRunSync()
        catch {
          case _: InterruptedException =>
            bleep.ExitCode.Success
          case ex: Exception =>
            pre.logger.error(s"MCP server failed: ${ex.getMessage}")
            bleep.ExitCode.Failure
        }
    }
  }
}
