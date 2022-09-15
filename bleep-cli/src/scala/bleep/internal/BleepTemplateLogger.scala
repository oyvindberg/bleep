package bleep
package internal

import bleep.logging.Logger
import bleep.templates.ProjectNameLike.Syntax
import bleep.templates.{ProjectNameLike, TemplateLogger}

class BleepTemplateLogger(logger: Logger) extends TemplateLogger {
  override def appliedTemplateTo[Name: ProjectNameLike](templateId: model.TemplateId, to: Seq[Name]): Unit =
    logger.withContext(templateId).withContext(to.map(_.asString)).debug(s"applied to")

  override def couldntApplyTemplate(templateId: model.TemplateId, reason: String): Unit =
    logger.withContext(templateId).debug(reason)
}
