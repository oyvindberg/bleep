package bleep.model

import bleep.RelPath
import io.circe._
import io.circe.generic.semiauto.deriveCodec

sealed trait ScriptDef {
  lazy val asJson: Json = ScriptDef.encodes(this)
}

object ScriptDef {
  // inefficient, but let's roll with it for now
  implicit val ordering: Ordering[ScriptDef] =
    Ordering.by(_.asJson.noSpaces)

  case class Main(project: CrossProjectName, main: String, sourceGlobs: JsonSet[RelPath]) extends ScriptDef
  object Main {
    implicit val codec: Codec.AsObject[Main] = deriveCodec
  }

  case class Shell(command: Option[String], `override-os`: Option[Map[Os, String]], sourceGlobs: JsonSet[RelPath]) extends ScriptDef
  object Shell {
    implicit val codec: Codec.AsObject[Shell] = deriveCodec
  }

  val fromString: Decoder[ScriptDef] =
    Decoder.instance { c =>
      c.as[String].flatMap { str =>
        str.split("/") match {
          case Array(projectName, main) =>
            CrossProjectName.decodes.decodeJson(Json.fromString(projectName)).map { crossProjectName =>
              ScriptDef.Main(crossProjectName, main, JsonSet.empty)
            }

          case _ =>
            Left(DecodingFailure(s"$str needs to be on the form `projectName(@crossId)/fully.qualified.Main`", c.history))
        }
      }
    }

  implicit val decodes: Decoder[ScriptDef] =
    fromString.or(Main.codec.map(x => x: ScriptDef)).or(Shell.codec.map(x => x: ScriptDef))

  implicit val encodes: Encoder[ScriptDef] =
    Encoder.instance {
      case x: Main  => Json.fromJsonObject(Main.codec.encodeObject(x))
      case x: Shell => Json.fromJsonObject(Shell.codec.encodeObject(x))
    }
}
