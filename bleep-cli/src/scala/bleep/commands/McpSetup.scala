package bleep.commands

import bleep.{BleepCommand, BleepException}
import ryddig.Logger

case class McpSetup(logger: Logger) extends BleepCommand {
  override def run(): Either[BleepException, Unit] = {
    logger.warn("Setting up MCP server! (TODO)")
    Right(())
  }
}
