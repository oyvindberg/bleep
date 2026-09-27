package bleep.model

import bleep.RelPath
import io.circe.*
import io.circe.generic.semiauto.deriveCodec

sealed trait ScriptDef {
  lazy val asJson: Json = ScriptDef.encodes(this)
  def folderName: String
}

object ScriptDef {
  // inefficient, but let's roll with it for now
  implicit val ordering: Ordering[ScriptDef] =
    Ordering.by(_.asJson.noSpaces)

  /** @param inputs
    *   projects whose output the script reads (their compiled classes, by path from the build model): built before it runs, a change to their classes re-runs
    *   it, and never on anyone's classpath because of this. The `sourcegen:` counterpart of `postCompile.inputs`.
    */
  case class Main(project: CrossProjectName, main: String, sourceGlobs: JsonSet[RelPath], inputs: JsonSet[CrossProjectName]) extends ScriptDef {
    def folderName: String = main
  }

  object Main {
    implicit val codec: Codec.AsObject[Main] = deriveCodec
  }

  val fromString: Decoder[ScriptDef] =
    Decoder.instance { c =>
      c.as[String].flatMap { str =>
        str.split("/") match {
          case Array(projectName, main) =>
            CrossProjectName.decodes.decodeJson(Json.fromString(projectName)).map { crossProjectName =>
              ScriptDef.Main(crossProjectName, main, JsonSet.empty, JsonSet.empty)
            }

          case _ =>
            Left(DecodingFailure(s"$str needs to be on the form `projectName(@crossId)/fully.qualified.Main`", c.history))
        }
      }
    }

  implicit val decodes: Decoder[ScriptDef] =
    fromString.or(Main.codec.map(x => x: ScriptDef))

  implicit val encodes: Encoder[ScriptDef] =
    Encoder.instance { case x: Main =>
      Json.fromJsonObject(Main.codec.encodeObject(x))
    }
}
