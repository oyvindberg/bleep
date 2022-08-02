package bleep.model

import io.circe.{Decoder, DecodingFailure, Encoder, Json}

case class ScriptDef(project: CrossProjectName, main: String)

object ScriptDef {
  implicit val decodes: Decoder[ScriptDef] = Decoder.instance(c =>
    c.as[String].flatMap { str =>
      str.split("/") match {
        case Array(projectName, main) =>
          CrossProjectName.decodes.decodeJson(Json.fromString(projectName)).map(crossProjectName => ScriptDef(crossProjectName, main))

        case _ =>
          Left(DecodingFailure(s"$str needs to be on the form `projectName(@crossId)/fully.qualified.Main`", c.history))
      }
    }
  )
  implicit val encodes: Encoder[ScriptDef] =
    Encoder.instance(sd => Json.fromString(s"${sd.project.value}/${sd.main}"))
}
