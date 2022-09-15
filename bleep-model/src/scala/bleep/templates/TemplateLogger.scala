package bleep.templates

import bleep.model
import bleep.model.TemplateId

// decouple model/templating from internal logging library.
trait TemplateLogger {
  def appliedTemplateTo[Name: ProjectNameLike](templateId: model.TemplateId, to: Seq[Name]): Unit
  def couldntApplyTemplate(templateId: model.TemplateId, reason: String): Unit
}
object TemplateLogger {
  object Noop extends TemplateLogger {
    override def appliedTemplateTo[Name: ProjectNameLike](templateId: TemplateId, to: Seq[Name]): Unit = ()

    override def couldntApplyTemplate(templateId: TemplateId, reason: String): Unit = ()
  }
}
