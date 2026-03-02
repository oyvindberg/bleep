package bleep.mcp

import bleep._
import cats.effect.{Deferred, IO}
import cats.effect.unsafe.implicits.global
import ch.linkyard.mcp.jsonrpc2.transport.StdioJsonRpcConnection

/** Entry point for the MCP server. Bootstraps the build, creates the MCP server, and runs on stdio.
  *
  * Automatically restarts the server if it crashes, with exponential backoff up to 30 seconds. Interrupted exceptions
  * (clean shutdown) are not retried. The restart tool triggers a graceful re-bootstrap without killing the process.
  */
object McpServerRunner {

  private val InitialBackoffMs: Long = 1000
  private val MaxBackoffMs: Long = 30000

  def run(pre: Prebootstrapped): bleep.ExitCode =
    runWithRestart(pre)

  private def bootstrapStarted(pre: Prebootstrapped): Either[BleepException, Started] = {
    val config = BleepConfigOps.loadOrDefault(pre.userPaths).orThrow
    bootstrap.from(pre, ResolveProjects.InMemory, rewrites = Nil, config, CoursierResolver.Factory.default)
  }

  private def runWithRestart(pre: Prebootstrapped): bleep.ExitCode = {
    var backoffMs = InitialBackoffMs

    while (true) {
      bootstrapStarted(pre) match {
        case Left(err) =>
          pre.logger.error(s"Failed to load build: ${err.getMessage}")
          return bleep.ExitCode.Failure
        case Right(started) =>
          val restartSignal = Deferred.unsafe[IO, Unit]
          val server = new BleepMcpServer(started, restartSignal)
          val program = IO
            .race(
              server
                .start(
                  StdioJsonRpcConnection.create[IO],
                  e => IO(started.logger.error(s"MCP server error: $e", e))
                )
                .useForever,
              restartSignal.get
            )

          try {
            program.unsafeRunSync() match {
              case Left(_) =>
                // useForever completed (shouldn't happen) — treat as clean exit
                return bleep.ExitCode.Success
              case Right(_) =>
                // Restart signal — re-bootstrap on next iteration
                pre.logger.info("MCP server restarting (re-bootstrapping build)...")
                backoffMs = InitialBackoffMs
            }
          } catch {
            case _: InterruptedException =>
              return bleep.ExitCode.Success
            case ex: Exception =>
              pre.logger.error(s"MCP server crashed, restarting in ${backoffMs}ms: ${ex.getMessage}")
              Thread.sleep(backoffMs)
              backoffMs = math.min(backoffMs * 2, MaxBackoffMs)
          }
      }
    }

    bleep.ExitCode.Success // unreachable, satisfies compiler
  }
}
