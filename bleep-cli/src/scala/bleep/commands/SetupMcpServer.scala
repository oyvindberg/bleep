package bleep
package commands

import bleep.internal.FileUtils
import io.circe.Json

case class SetupMcpServer(forceJvm: Boolean) extends BleepBuildCommand {

  override def run(started: Started): Either[BleepException, Unit] = {
    val argv = started.bleepExecutable.forceGet.whole ++ List("mcp-server")

    val mcpConfig = Json.obj(
      "mcpServers" -> Json.obj(
        "bleep" -> Json.obj(
          "type" -> Json.fromString("stdio"),
          "command" -> Json.fromString(argv.head),
          "args" -> Json.arr(argv.tail.map(Json.fromString)*)
        )
      )
    )

    val mcpJsonFile = started.buildPaths.buildDir.resolve(".mcp.json")

    Right(FileUtils.writeString(started.logger, Some("writing MCP server configuration"), mcpJsonFile, mcpConfig.spaces2))
  }
}
