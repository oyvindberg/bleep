package bleep.model

import bleep.model

case class TemplateWithExploded(exploded: model.Project, current: model.Project) {
  def toProject = ProjectWithExploded(exploded, current)
}
