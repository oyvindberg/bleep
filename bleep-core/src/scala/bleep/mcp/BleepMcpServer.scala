package bleep.mcp

import bleep.Started
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import mcp.protocol.Implementation
import mcp.server.{McpServer, StdioTransport}

case class BleepMcpServer(started: Started) {

  def run(): Unit = {
    val server = for {
      mcp <- McpServer[IO](
        Implementation(name = "bleep-mcp", version = "0.0.1", title = Some("frontend for bloop")),
        tools = List(
          McpToolCompile(started)
        )
      )
      stdin <- StdioTransport[IO]()
    } yield mcp.serve(stdin)
    server.use(_ => IO.never).unsafeRunSync()
  }

}
