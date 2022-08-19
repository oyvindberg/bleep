package bleep.templates

import bleep.model

import scala.annotation.tailrec

/** Takes an exploded build, reapplies existing templates
  */
object templatesReapply {
  def apply(build: model.Build.FileBacked): model.Build.FileBacked = {

    val initial: Map[model.CrossProjectName, model.ProjectWithExploded] =
      build.explodedProjects.map { case (name, exploded) => (name, model.ProjectWithExploded(exploded, exploded)) }

    val templated: Map[model.CrossProjectName, model.ProjectWithExploded] =
      initial.map { case (name, p) =>
        val pp = p.mapCurrent(current => reapplyTemplates(build.explodedTemplates, current))
        (name, pp)
      }

    val groupedCrossProjects: Map[model.ProjectName, model.ProjectWithExploded] =
      groupCrossProjects(templated)

    val groupedTemplatedCrossProjects: Map[model.ProjectName, model.ProjectWithExploded] =
      groupedCrossProjects.map { case (name, p) =>
        val pp = p.mapCurrent(current => reapplyTemplates(build.explodedTemplates, current))
        (name, pp)
      }

    val newProjects = groupedTemplatedCrossProjects.map { case (n, cp) => (n, cp.current) }
    build.mapBuildFile(_.copy(projects = model.JsonMap(newProjects)))
  }

  def reapplyTemplates(templates: Map[model.TemplateId, model.Project], project: model.Project): model.Project = {
    // applying a template may remove other templateIds in list
    @tailrec
    def go(project: model.Project, `extends`: List[model.TemplateId]): model.Project =
      `extends` match {
        case templateId :: restTemplateIds =>
          val pp =
            if (project.`extends`.values.contains(templateId))
              project.removeAll(templates(templateId))
            else project

          go(pp, restTemplateIds)
        case Nil => project
      }

    go(project, project.`extends`.values.toList)
  }
}
