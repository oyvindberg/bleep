package bleep.mcp

import bleep.{BleepException, Started}
import cats.effect.{ IO}
import io.circe.Codec
import mcp.protocol.Content
import mcp.schema.{McpSchema, description}
import mcp.server.ToolDef

object McpToolCompile {
  @description("Compile arguments")
  case class Input(
                    @description("The module that should me compiled")
                    module: Option[String]
                  ) derives Codec.AsObject

  object Input {
    given McpSchema[Input] = McpSchema.derived
  }
  def apply(started: Started): ToolDef[IO, Input, Nothing] =
    ToolDef.unstructured[IO, Input](
      name = "compile",
      description = Some("compile the project/module")
    ) { input =>

      IO {
        //todo:
        // 1. parse args input and pass it inn to the compile command
        // 2. read the logger and pass the result?
        bleep.commands.Compile(false, Array.empty).run(started) match {
          case Left(value) => List(Content.Text(s"Error:\n${value.message}"))
          case Right(value) => List(Content.Text(s"Build success"))
        }
      }
    }

}
