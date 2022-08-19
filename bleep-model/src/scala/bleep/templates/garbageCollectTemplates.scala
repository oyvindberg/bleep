package bleep.templates

import bleep.model

object garbageCollectTemplates {
  def apply(b: model.BuildFile): model.BuildFile = {
    val seen = collection.mutable.Set.empty[model.TemplateId]

    def go(p: model.Project): Unit = {
      p.`extends`.values.foreach { templateId =>
        seen += templateId
        go(b.templates.value(templateId))
      }

      p.cross.value.values.foreach(go)
    }

    b.projects.value.values.foreach(go)

    b.copy(templates = model.JsonMap(b.templates.value.filter { case (templateId, _) => seen(templateId) }))
  }

}
