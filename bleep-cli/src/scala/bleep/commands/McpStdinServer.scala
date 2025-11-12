package bleep.commands

import bleep.mcp.BleepMcpServer
import bleep.{BleepCommand, BleepException, Started}
import ryddig.Logger

case class McpStdinServer(started: Started) extends BleepCommand {
  override def run(): Either[BleepException, Unit] = {
    started.logger.info("TODO run the mcp server!")
    Right(BleepMcpServer(started).run())
  }
}
