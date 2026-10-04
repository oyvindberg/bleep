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
    * @param description
    *   one line saying what the script does, shown by `bleep script` and in `bleep --help`
    */
  case class Main(
      project: CrossProjectName,
      main: String,
      sourceGlobs: JsonSet[RelPath],
      inputs: JsonSet[CrossProjectName],
      description: Option[String]
  ) extends ScriptDef {
    def folderName: String = main
  }

  object Main {
    implicit val codec: Codec.AsObject[Main] = deriveCodec
  }

  /** What `bleep script` and `bleep --help` say about a script: the descriptions of its entries, in order. `None` when no entry has one. */
  def description(scriptDefs: Seq[ScriptDef]): Option[String] =
    scriptDefs.flatMap { case x: Main => x.description }.distinct match {
      case Seq()     => None
      case described => Some(described.mkString("; "))
    }

  val fromString: Decoder[ScriptDef] =
    Decoder.instance { c =>
      c.as[String].flatMap { str =>
        str.split("/") match {
          case Array(projectName, main) =>
            CrossProjectName.decodes.decodeJson(Json.fromString(projectName)).map { crossProjectName =>
              ScriptDef.Main(crossProjectName, main, JsonSet.empty, JsonSet.empty, None)
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
