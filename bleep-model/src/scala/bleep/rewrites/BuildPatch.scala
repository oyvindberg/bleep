package bleep
package rewrites

import bleep.internal.IterableOps

import scala.collection.mutable

sealed trait BuildPatch

object BuildPatch {
  case class AddProject(values: model.Project, crossProjectName: model.CrossProjectName) extends BuildPatch

  case class RemoveProjects(crossProjectNames: Set[model.CrossProjectName]) extends BuildPatch

  case class AddValues(values: model.Project, crossProjectNames: Set[model.CrossProjectName], overwrite: Boolean) extends BuildPatch

  case class RemoveValues(values: model.Project, crossProjectNames: Set[model.CrossProjectName]) extends BuildPatch

  def apply(build: model.Build.FileBacked, patch: BuildPatch): model.Build.FileBacked =
    patch match {
      case BuildPatch.RemoveProjects(crossProjectNames) =>
        val affected = Affected(build, crossProjectNames)

        if (affected.crossProjects.isEmpty) build
        else {
          // when we remove values from a template which is inherited by a project which should keep its values
          // we need to push values down to the project definition
          val pushedValues =
            mutable.HashMap.empty[model.CrossProjectName, model.Project]

          def pushValues(values: model.Project)(crossName: model.CrossProjectName): Unit =
            pushedValues.get(crossName) match {
              case Some(oldValues) => pushedValues.put(crossName, oldValues.union(values)).discard()
              case None            => pushedValues.put(crossName, values).discard()
            }

          val newTemplates = build.file.templates.map {
            case (templateId, templateDef) if affected.templates(templateId) && !templateDef.cross.isEmpty =>
              val affectedCrossIds = affected.templatesForCross(templateId)
              val (affectedCross, unaffectedCross) = templateDef.cross.value.partition { case (crossId, _) => affectedCrossIds(crossId) }

              // inline settings removed in template to non-affected cross projects
              val extendedByNonAffected = build.explodedProjects.collect {
                case (crossName, p) if !affected.crossProjectNames(crossName) && p.`extends`.values.contains(templateId) => crossName
              }.toSet

              affectedCross.foreach { case (crossId, values) =>
                extendedByNonAffected.foreach { crossName =>
                  if (crossName.crossId.contains(crossId)) {
                    pushValues(values)(crossName)
                  }
                }
              }

              (templateId, templateDef.copy(cross = model.JsonMap(unaffectedCross)))

            case unchanged => unchanged
          }

          val newProjects = build.file.projects.value.flatMap { case (name, p0) =>
            val p1 = pushedValues.get(model.CrossProjectName(name, None)).foldLeft(p0)(_.union(_))
            val p2 = {
              val pushed = pushedValues.collect { case (model.CrossProjectName(`name`, Some(crossId)), values) => (crossId, values) }
              val newCross = p0.cross.union(model.JsonMap(pushed.toMap))
              p1.copy(cross = newCross)
            }

            affected.crossProjectsByProjectName.get(name) match {
              case None => Some((name, p2))
              case Some(affectedCrossForProject) =>
                val nonAffected = build.explodedProjectsByName(name).keySet -- affectedCrossForProject.keySet
                // easy case: if we delete all cross projects when drop the whole thing
                if (nonAffected.isEmpty) None
                // the project contains several cross projects, and some of them are kept. some/all of those may be inherited from template
                else {
                  val keep = nonAffected.flatMap(_.crossId).toSet
                  Some((name, p2.copy(cross = p2.cross.filter { case (crossId, _) => keep(crossId) })))
                }
            }
          }

          build.mapBuildFile(bf => bf.copy(projects = model.JsonMap(newProjects), templates = newTemplates))
        }

      case BuildPatch.AddProject(values, crossProjectName) =>
        val oldProjects = build.file.projects.value
        val newProject: model.Project =
          crossProjectName.crossId match {
            case Some(crossId) =>
              oldProjects.get(crossProjectName.name) match {
                // existing project? merge potentially new cross
                case Some(existingProject) =>
                  val maybeExistingCrossProject = existingProject.cross.value.get(crossId)
                  // cross existed? merge
                  val newCrossProject = maybeExistingCrossProject match {
                    case Some(existingCrossProject) => existingCrossProject.union(values.removeAll(existingProject))
                    case None                       => values
                  }
                  val newCross = existingProject.cross.updated(crossId, newCrossProject)
                  val newProject = existingProject.copy(cross = newCross)
                  newProject

                case None =>
                  model.Project.empty.copy(cross = model.JsonMap(Map(crossId -> values)))
              }
            case None =>
              oldProjects.get(crossProjectName.name) match {
                case Some(existingProject) => existingProject.union(values)
                case None                  => values
              }
          }

        val newProjects = oldProjects.updated(crossProjectName.name, newProject)
        build.mapBuildFile(_.copy(projects = model.JsonMap(newProjects)))

      case BuildPatch.AddValues(values, crossProjectNames, overwrite) =>
        val crossProjectNamesList = crossProjectNames.toList

        // drop whatever all projects already have
        val remainingValues =
          crossProjectNamesList
            .map(build.explodedProjects.apply)
            .optReduce(_ intersectDropEmpty _)
            .foldLeft(values)(_ removeAll _)

        if (remainingValues.isEmpty) build
        else
          crossProjectNamesList.groupBy(_.name) match {
            // all chosen projects are cross projects of the same project
            case AllInOneProject((projectName, maybeCrossIds)) =>
              addToCrossProjects(build, remainingValues, projectName, maybeCrossIds, overwrite)

            case more =>
              // detect if there exists a template such that the set of projects extending it
              // is exactly the set of projects we're adding properties to
              val maybeAppendToTemplate = build.explodedProjects.toArray
                .flatMap { case (crossName, p) => p.`extends`.values.iterator.map(e => (e, crossName)) }
                .groupBy { case (tid, _) => tid }
                .collectFirst {
                  case (templateId, tuples)
                      if tuples.length == crossProjectNames.size && // optimization
                        tuples.map(_._2).toSet == crossProjectNames =>
                    templateId
                }

              maybeAppendToTemplate match {
                case Some(templateId) =>
                  val template = build.file.templates.value(templateId)
                  val updatedTemplate = appended(template, values, overwrite)
                  build.mapBuildFile(file => file.copy(templates = file.templates.updated(templateId, updatedTemplate)))
                case None =>
                  more.foldLeft(build) { case (build, (projectName, crossNames)) =>
                    addToCrossProjects(build, remainingValues, projectName, crossNames.map(_.crossId), overwrite)
                  }
              }
          }

      case BuildPatch.RemoveValues(removeValues, targetCrossProjects) =>
        removeFrom(build, removeValues, targetCrossProjects)
    }

  def appended(p: model.Project, newValues: model.Project, overwrite: Boolean): model.Project =
    if (overwrite) newValues.union(p) else p.union(newValues)

  object AllInOneProject {
    def unapply(projectsByName: Map[model.ProjectName, List[model.CrossProjectName]]): Option[(model.ProjectName, List[Option[model.CrossId]])] =
      if (projectsByName.size == 1) {
        val (projectName, tuples) = projectsByName.head
        Some((projectName, tuples.map(_.crossId)))
      } else None
  }

  def addToCrossProjects(
      build: model.Build.FileBacked,
      values: model.Project,
      projectName: model.ProjectName,
      crossIds: List[Option[model.CrossId]],
      overwrite: Boolean
  ): model.Build.FileBacked = {
    // subtract what's already provided by the project common to the cross projects
    val values1 = {
      val sharedBetweenCrossProjects: model.Project =
        build.explodedProjects
          // note: all cross projects are not necessarily chosen. this finds all
          .collect { case (model.CrossProjectName(`projectName`, _), p) => p }
          .reduce(_ intersect _)

      values.removeAll(sharedBetweenCrossProjects)
    }

    if (values1.isEmpty) build
    else {
      val updatedRawProject =
        crossIds.foldLeft(build.file.projects.value(projectName)) {
          case (rawProject, None) =>
            appended(rawProject, values, overwrite)

          case (rawProject, Some(crossId)) =>
            val rawCrossProject = rawProject.cross.value.getOrElse(crossId, model.Project.empty)
            val updatedRawCrossProject = appended(rawCrossProject, values1, overwrite)
            val newCross = rawProject.cross.updated(crossId, updatedRawCrossProject)
            rawProject.copy(cross = newCross)
        }

      build.mapBuildFile(_.copy(projects = build.file.projects.updated(projectName, updatedRawProject)))
    }
  }

  private case class Affected(build: model.Build, crossProjectNames: Set[model.CrossProjectName]) {
    lazy val crossProjects: Map[model.CrossProjectName, model.Project] =
      build.explodedProjects.filter { case (crossName, _) => crossProjectNames.contains(crossName) }

    lazy val crossProjectsByProjectName: Map[model.ProjectName, Map[model.CrossProjectName, model.Project]] =
      crossProjects.groupBy { case (crossName, _) => crossName.name }

    lazy val projectNames: Set[model.ProjectName] =
      crossProjectsByProjectName.keySet

    // not all cross settings inside a template are necessarily affected
    lazy val templatesForCross: Map[model.TemplateId, Set[model.CrossId]] =
      crossProjects.toArray // reminder: must be a sequence since next step produces duplicate keys
        .flatMap { case (crossName, p) => p.`extends`.values.map(templateId => (templateId, crossName)) }
        .groupBy { case (templateId, _) => templateId }
        .map { case (templateId, tuples) => (templateId, tuples.flatMap(_._2.crossId).toSet) }

    lazy val templates: Set[model.TemplateId] =
      templatesForCross.keySet
  }

  def removeFrom(build: model.Build.FileBacked, removeValues: model.Project, targetCrossProjects: Set[model.CrossProjectName]): model.Build.FileBacked = {
    // collect list of things we may have to rewrite. reminder that it's common that things are inherited from templates
    val affected = Affected(build, targetCrossProjects)

    // when we remove values from a template which is inherited by a project which should keep its values
    // we need to push values down to the project definition
    val pushedValues =
      mutable.HashMap.empty[model.CrossProjectName, model.Project]

    def pushValues(values: model.Project)(crossName: model.CrossProjectName): Unit =
      pushedValues.get(crossName) match {
        case Some(oldValues) => pushedValues.put(crossName, oldValues.union(values)).discard()
        case None            => pushedValues.put(crossName, values).discard()
      }

    // check if noop
    if (affected.crossProjectNames.isEmpty) return build

    val newTemplates =
      build.file.templates.map {
        case (templateId, templateDef) if affected.templates(templateId) =>
          val beforeCross = {
            val change = templateDef.intersect(removeValues)
            if (change.isEmpty) templateDef
            else {
              // inline settings removed in template to non-affected cross projects
              val extendedByNonAffected = build.explodedProjects.collect {
                case (crossName, p) if !affected.crossProjectNames(crossName) && p.`extends`.values.contains(templateId) => crossName
              }.toSet
              extendedByNonAffected.foreach(pushValues(change))
              templateDef.removeAll(change)
            }
          }

          val newCross = beforeCross.cross.map { case unchanged @ (crossId, crossTemplateDef) =>
            val change = crossTemplateDef.intersect(removeValues)
            val isAffectedCross = affected.templatesForCross.getOrElse(templateId, Set.empty).contains(crossId)
            if (!isAffectedCross || change.isEmpty) unchanged
            else {
              // inline settings removed in template to non-affected cross projects
              val extendedByNonAffected = build.explodedProjects.collect {
                case (crossName, p)
                    if !affected.crossProjectNames(crossName) &&
                      crossName.crossId.contains(crossId) &&
                      p.`extends`.values.contains(templateId) =>
                  crossName
              }.toSet
              extendedByNonAffected.foreach(pushValues(change))
              (crossId, crossTemplateDef.removeAll(change))
            }
          }

          (templateId, beforeCross.copy(cross = newCross))
        case unchanged => unchanged
      }

    val newProjects = build.file.projects.map { case (name, project) =>
      val beforeCross = {
        val project1 = pushedValues.get(model.CrossProjectName(name, None)).foldLeft(project)(_ union _)

        if (!affected.projectNames(name)) project1
        else {
          val change = project1.intersect(removeValues)
          val project2 = project1.removeAll(change)

          // push values from shared project to cross projects if there are any non-affected by the removal
          project2.cross.value.foreach { case (crossId, _) =>
            val crossName = model.CrossProjectName(name, Some(crossId))
            if (!affected.crossProjectNames(crossName)) pushValues(change)(crossName)
          }
          project2
        }
      }

      val newCross = {
        // reminder that a cross pushed down from a template may not originally have appeared in the project definition
        val withPushed = {
          val pushed = pushedValues.collect { case (model.CrossProjectName(`name`, Some(crossId)), values) => (crossId, values) }

          beforeCross.cross.union(model.JsonMap(pushed.toMap))
        }

        val removed = withPushed.map { case (crossId, p) =>
          val crossName = model.CrossProjectName(name, Some(crossId))
          val p1 = if (affected.crossProjectNames(crossName)) p.removeAll(removeValues) else p
          (crossId, p1)
        }
        removed
      }
      (name, beforeCross.copy(cross = newCross))
    }

    build.mapBuildFile(file => file.copy(projects = newProjects, templates = newTemplates))
  }
}
