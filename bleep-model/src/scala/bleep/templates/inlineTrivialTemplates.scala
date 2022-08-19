package bleep.templates

import bleep.model

object inlineTrivialTemplates {
  def apply(b: model.BuildFile): model.BuildFile = {

    val toInline: Map[model.TemplateId, List[model.TemplateId]] =
      b.templates.value.collect {
        case (name, p) if p.`extends`.values.size <= 1 && p.copy(`extends` = model.JsonSet.empty).isEmpty => (name, p.`extends`.values.toList)
      }

    def expand(templateId: model.TemplateId): List[model.TemplateId] =
      toInline.get(templateId) match {
        case Some(replacements) => replacements.flatMap(expand)
        case None               => List(templateId)
      }

    def go(p: model.Project): model.Project =
      p.copy(
        `extends` = model.JsonSet(p.`extends`.values.flatMap(expand)),
        cross = model.JsonMap(p.cross.value.map { case (crossId, p) => (crossId, go(p)) })
      )

    b.copy(
      templates = model.JsonMap(b.templates.value.collect { case (templateId, p) if !toInline.contains(templateId) => (templateId, go(p)) }),
      projects = model.JsonMap(b.projects.value.map { case (name, p) => (name, go(p)) })
    )
  }
}
