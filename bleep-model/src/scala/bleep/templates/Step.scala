package bleep.templates

import bleep.internal.IterableOps
import bleep.model
import bleep.templates.ProjectNameLike.Syntax

import scala.collection.mutable

object Step {
  case class Result[ProjectName](
      templatedProjects: Map[ProjectName, model.ProjectWithExploded],
      inferredTemplates: Map[model.TemplateId, Option[model.TemplateWithExploded]]
  )

  @FunctionalInterface
  trait RelevantProjectContent {
    def apply(p: model.Project, templateDef: TemplateDef): model.Project
  }

  object RelevantProjectContent {
    object Noop extends RelevantProjectContent {
      override def apply(p: model.Project, templateDef: TemplateDef): model.Project = p
    }

    object RelevantParents extends RelevantProjectContent {
      override def apply(p: model.Project, templateDef: TemplateDef): model.Project = {
        val keepParent: Set[model.TemplateId] = templateDef.allParents.map(_.templateId).toSet

        p.copy(`extends` = model.JsonSet(p.`extends`.values.filter(keepParent)))
      }
    }
  }

  implicit class ProjectOps(p: model.Project) {
    def withTemplate(t: model.TemplateId): model.Project =
      p.copy(`extends` = p.`extends` + t)

    def dropTemplates: model.Project =
      p.copy(`extends` = model.JsonSet.empty)
  }

  def step[ProjectName: ProjectNameLike](
      logger: TemplateLogger,
      initialProjects: Map[ProjectName, model.ProjectWithExploded],
      ignoreWhenInferringTemplates: ProjectName => Boolean,
      templatingContent: RelevantProjectContent,
      applicableTemplates: TemplateDefs[ProjectName]
  ): Result[ProjectName] = {
    // we'll apply these groups of templates in turn
    val templates: List[TemplateDef] = applicableTemplates(initialProjects, ignoreWhenInferringTemplates)

    // keep state here as we go about applying
    var templatedProjects: Map[ProjectName, model.ProjectWithExploded] =
      initialProjects

    val inferredTemplates: mutable.Map[model.TemplateId, Option[model.TemplateWithExploded]] =
      mutable.Map.empty

    templates.foreach { templateDef =>
      // filter which projects we use to infer the contents of the template
      val projects = templatedProjects.toVector.filter { case (name, pe) =>
        !ignoreWhenInferringTemplates(name) &&
        templateDef.include(pe.exploded)
      }

      // only infer contents for a template if it is used by more than one project (not counting cross)
      val initial: Either[String, Vector[(ProjectName, model.ProjectWithExploded)]] = {
        val projectNames = projects.map { case (pn, _) => pn.extractProjectName }.distinct
        if (projectNames.size <= 1) Left(s"Too few qualifying project names: $projectNames")
        else Right(projects)
      }

      val fullTemplateContents: Either[String, model.ProjectWithExploded] =
        initial.flatMap { projects =>
          projects
            .map { case (_, p) => p.mapBoth(current => templatingContent(current, templateDef)) }
            .optReduce((p1, p2) => p1.mapBothOpt(_.intersectDropEmpty(p2.current)))
            .toRight("No common settings")
        }

      val templateContentsAfterParents: Either[String, model.TemplateWithExploded] =
        fullTemplateContents.map(p => applyInheritance(templateDef, inferredTemplates.apply, p).toTemplate)

      inferredTemplates(templateDef.templateId) = templateContentsAfterParents.toOption

      templateContentsAfterParents match {
        case Right(template) =>
          val appliedTo = List.newBuilder[ProjectName]

          templatedProjects = templatedProjects.flatMap {
            case (name, project0) if templateDef.include(project0.exploded) =>
              tryApplyTemplate(templateDef, template, project0) match {
                case Some(project) =>
                  appliedTo += name
                  Map(name -> project)
                case None => Map(name -> project0)
              }
            case other => Some(other)
          }
          val ps = appliedTo.result()
          logger.appliedTemplateTo(templateDef.templateId, ps)

        case Left(reason) =>
          logger.couldntApplyTemplate(templateDef.templateId, reason)
      }
    }
    Result(templatedProjects, inferredTemplates.toMap)
  }

  def applyInheritance(
      templateDef: TemplateDef,
      getTemplate: model.TemplateId => Option[model.TemplateWithExploded],
      project: model.ProjectWithExploded
  ): model.ProjectWithExploded = {
    def go(acc: model.ProjectWithExploded, parent: TemplateDef): model.ProjectWithExploded =
      if (acc.current.`extends`.values.contains(parent.templateId)) acc
      else {
        val rec = parent.allParents.foldLeft(acc)(go)

        getTemplate(parent.templateId)
          .flatMap(templateProject => tryApplyTemplate(parent, templateProject, rec))
          .getOrElse(rec)
      }

    templateDef.allParents.foldLeft(project)(go)
  }

  def tryApplyTemplate(
      template: TemplateDef,
      templateProject: model.TemplateWithExploded,
      project: model.ProjectWithExploded
  ): Option[model.ProjectWithExploded] = {
    val shortened = project.current.removeAll(templateProject.exploded)
    val isShortened = shortened != project.current

    def doesntAddNew: Boolean =
      templateProject.exploded.removeAll(project.exploded).isEmpty

    // primitive values will be overridden, so including these templates would be sound. however, it is confusing.
    def doesntHaveIncompatiblePrimitives: Boolean =
      project.exploded.union(templateProject.exploded) == templateProject.exploded.union(project.exploded)

    // this is needed since we disregard `extends` within cross projects, and also trim empty projects in `doesntAddNew`
    def sameCrossVersions: Boolean =
      templateProject.exploded.cross.value.keys.forall(project.exploded.cross.value.keySet)

    if (isShortened && doesntAddNew && doesntHaveIncompatiblePrimitives && sameCrossVersions) {
      val newCurrent = shortened.withTemplate(template.templateId)
      val newExploded = project.exploded.withTemplate(template.templateId)
      val newExploded1 = newExploded.copy(cross = newExploded.cross.map { case (crossId, p) => (crossId, p.withTemplate(template.templateId)) })
      Some(project.copy(current = newCurrent, exploded = newExploded1))
    } else None
  }
}
