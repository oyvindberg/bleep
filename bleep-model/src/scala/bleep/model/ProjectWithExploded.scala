package bleep.model

import bleep.model

case class ProjectWithExploded(exploded: model.Project, current: model.Project) {
  def mapCurrent(f: model.Project => model.Project) = copy(current = f(current))

  def mapBoth(f: model.Project => model.Project) = copy(current = f(current), exploded = f(exploded))
  def mapBothOpt(f: model.Project => Option[model.Project]) =
    for {
      newCurrent <- f(current)
      newExploded <- f(exploded)
    } yield ProjectWithExploded(exploded = newExploded, current = newCurrent)

  def toTemplate = TemplateWithExploded(exploded, current)
}
