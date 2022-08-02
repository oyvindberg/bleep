package bleep.model

import io.circe.generic.semiauto.deriveEncoder
import io.circe.{Decoder, DecodingFailure, Encoder, JsonObject}

case class Build(
    $schema: String,
    $version: BleepVersion,
    templates: JsonMap[TemplateId, Project],
    scripts: JsonMap[ScriptName, JsonList[ScriptDef]],
    resolvers: JsonList[Repository],
    projects: JsonMap[ProjectName, Project],
    jvm: Option[Jvm]
)

object Build {
  def empty(version: BleepVersion) = Build($schema, version, JsonMap.empty, JsonMap.empty, JsonList.empty, JsonMap.empty, None)

  implicit val decodes: Decoder[Build] =
    Decoder.instance(c =>
      for {
        schema <- c.downField("$schema").as[String].flatMap {
          case ok @ $schema => Right(ok)
          case notOk        => Left(DecodingFailure(s"$notOk must be ${$schema}", c.history))
        }
        version <- c.downField("$version").as[BleepVersion]
        /* construct a custom decoder for `Project` to give better error messages */
        templateIds <- c.downField("templates").as[Option[JsonObject]].map(_.fold(Iterable.empty[TemplateId])(_.keys.map(TemplateId.apply)))
        templateIdDecoder = TemplateId.decoder(templateIds)
        projectNames <- c.downField("projects").as[Option[JsonObject]].map(_.fold(Iterable.empty[ProjectName])(_.keys.map(ProjectName.apply)))
        projectNameDecoder = ProjectName.decoder(projectNames)
        projectDecoder = Project.decodes(templateIdDecoder, projectNameDecoder)

        templates <- c.downField("templates").as[JsonMap[TemplateId, Project]](JsonMap.decodes(TemplateId.keyDecodes, projectDecoder))
        projects <- c.downField("projects").as[JsonMap[ProjectName, Project]](JsonMap.decodes(ProjectName.keyDecodes, projectDecoder))
        scripts <- c.downField("scripts").as[JsonMap[ScriptName, JsonList[ScriptDef]]]
        resolvers <- c.downField("resolvers").as[JsonList[Repository]]
        jvm <- c.downField("jvm").as[Option[Jvm]]
      } yield Build(schema, version, templates, scripts, resolvers, projects, jvm)
    )
  implicit val encodes: Encoder[Build] = deriveEncoder
}
