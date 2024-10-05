package bleep
package internal

import bleep.templates.ProjectNameLike.Syntax
import bleep.templates.{ProjectNameLike, TemplateLogger}
import ryddig.Logger

class BleepTemplateLogger(logger: Logger) extends TemplateLogger {
  override def appliedTemplateTo[Name: ProjectNameLike](templateId: model.TemplateId, to: Seq[Name]): Unit =
    logger.withContext("templateId", templateId.value).withContext("to", to.map(_.asString)).debug(s"applied to")

  override def couldntApplyTemplate(templateId: model.TemplateId, reason: String): Unit =
    logger.withContext("templateId", templateId.value).debug(reason)
}
