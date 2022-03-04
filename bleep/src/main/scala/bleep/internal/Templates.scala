package bleep
package internal

import bleep.internal.Functions.sortedExtends
import bleep.rewrites.deduplicateDependencies

import scala.annotation.tailrec
import scala.collection.immutable.{SortedMap, SortedSet}

object Templates {
  case class ProjectKeepExploded(exploded: model.Project, current: model.Project) {
    def map(f: model.Project => model.Project) = copy(current = f(current))

    def toTemplate = TemplateKeepExploded(exploded, current)
  }
  case class TemplateKeepExploded(exploded: model.Project, current: model.Project) {
    def toProject = ProjectKeepExploded(exploded, current)
  }

  implicit class ProjectOps(p: model.Project) {
    def withTemplate(t: model.TemplateId): model.Project =
      p.copy(`extends` = p.`extends` + t)
  }

  sealed trait TemplateDef {
    final lazy val templateId: model.TemplateId =
      model.TemplateId(s"template-$name")

    final lazy val allParents: List[TemplateDef] = {
      val isTest = this match {
        case TemplateDef.Test(_) => true
        case _                   => false
      }
      val isMain = this match {
        case TemplateDef.Main(_) => true
        case _                   => false
      }

      val b = List.newBuilder[TemplateDef]
      def go(p: TemplateDef): Unit = {
        p.parents.foreach(go)
        if (isTest) {
          val test = TemplateDef.Test(p)
          if (test != this) b += test
        } else if (isMain) {
          val main = TemplateDef.Main(p)
          if (main != this) b += main
        }
        b += p
      }
      parents foreach go
      b.result().distinct
    }

    protected def name: String
    protected def parents: List[TemplateDef]
    def include(p: model.Project): Boolean
  }

  object TemplateDef {
    implicit def ordering: Ordering[TemplateDef] = Ordering.by(_.name)

    def applicableForProjects(projects: List[model.Project]): List[TemplateDef] = {
      val scalas: List[ScalaVersion] =
        projects.flatMap(p => p.scala.flatMap(_.version)).distinct.map(v => ScalaVersion(v.binVersion))
      val platforms: List[Platform] =
        projects.flatMap(p => p.platform.flatMap(_.name)).distinct.map(platformId => Platform(platformId))
      val combined: List[PlatformScalaVersion] =
        scalas.flatMap(s => platforms.map(p => PlatformScalaVersion(p, s)))

      List(List(Common), scalas, platforms, combined).flatten.flatMap(t => List(t, Main(t), Test(t)))
    }

    def crossTemplates(crossProjects: Iterable[model.Project]): List[TemplateDef] = {
      val allCross = SortedSet.empty[model.CrossId] ++ crossProjects.flatMap(_.cross.value.keys)

      val crossSetups: List[CrossSetup] =
        crossProjects
          .flatMap { p =>
            if (p.cross.value.size < 2) None
            else Some(CrossSetup(SortedSet.empty[model.CrossId] ++ p.cross.value.keys, parents = Nil, all = allCross))
          }
          .toList
          .distinct

      val withParents = crossSetups.map { cs =>
        val parents = crossSetups.collect {
          case maybeParent if maybeParent.crossIds.forall(cs.crossIds) && maybeParent.crossIds != cs.crossIds => maybeParent
        }
        cs.copy(parents = parents)
      }

      withParents.flatMap(t => List(t, Main(t), Test(t)))
    }

    case object Common extends TemplateDef {
      override def name = "common"
      override def parents = Nil
      override def include(p: model.Project): Boolean = true
    }

    case class Test(parent: TemplateDef) extends TemplateDef {
      override def parents = List(parent)
      override def name = s"${parent.name}-test"
      override def include(p: model.Project): Boolean =
        parent.include(p) && !p.testFrameworks.isEmpty
    }

    case class Main(parent: TemplateDef) extends TemplateDef {
      override def parents = List(parent)
      override def name = s"${parent.name}-main"
      override def include(p: model.Project): Boolean =
        parent.include(p) && p.testFrameworks.isEmpty
    }

    case class Platform(platformName: model.PlatformId) extends TemplateDef {
      override def parents = List(Common)
      override def name = platformName.value
      override def include(p: model.Project): Boolean =
        parents.forall(_.include(p)) && p.cross.isEmpty && p.platform.exists(_.name.contains(platformName))
    }

    case class ScalaVersion(binVersion: String) extends TemplateDef {
      override def name = s"scala-$binVersion"
      override def parents = List(Common)
      override def include(p: model.Project): Boolean =
        parents.forall(_.include(p)) && p.cross.isEmpty && p.scala.flatMap(_.version).exists(_.binVersion == binVersion)
    }

    case class CrossSetup(crossIds: SortedSet[model.CrossId], parents: List[CrossSetup], all: SortedSet[model.CrossId]) extends TemplateDef {
      override def name: String = {
        val baseName =
          if (crossIds == all) "all"
          else {
            val allByPlatform = all.groupBy(_.value.takeWhile(_.isLetter))
            crossIds
              .groupBy(_.value.takeWhile(_.isLetter))
              .map { case (platform, ids) =>
                allByPlatform.get(platform) match {
                  case Some(`ids`) => s"$platform-all"
                  case _           => s"$platform-${ids.map(_.value.dropWhile(_.isLetter)).mkString("-")}"
                }
              }
              .mkString("-")
          }

        s"cross-$baseName"
      }
      override def include(p: model.Project): Boolean =
        crossIds.forall(p.cross.value.keySet)
    }

    case class PlatformScalaVersion(platform: Platform, scalaVersion: ScalaVersion) extends TemplateDef {
      override def name = s"${scalaVersion.name}-${platform.name}"
      override def parents = List(platform, scalaVersion)
      override def include(p: model.Project): Boolean = parents.forall(_.include(p))
    }
  }

  /** Takes an exploded build, infers templates and applies them. also groups cross projects
    */
  def apply(build0: ExplodedBuild, ignoreWhenInferringTemplates: model.ProjectName => Boolean): model.Build = {
    val initial: Map[model.CrossProjectName, ProjectKeepExploded] =
      build0.projects.map { case (name, exploded) => (name, ProjectKeepExploded(exploded, exploded)) }

    val templates: SortedMap[TemplateDef, Templates.TemplateKeepExploded] = {
      val projects: List[(model.ProjectName, ProjectKeepExploded)] =
        initial.toList.collect { case (crossName, p) if !ignoreWhenInferringTemplates(crossName.name) => (crossName.name, p) }

      Templates.inferFromExistingProjects(
        TemplateDef.applicableForProjects(projects.map(_._2.exploded)),
        projects
      )
    }

    val templated: Map[model.CrossProjectName, ProjectKeepExploded] =
      initial.map { case (name, p) =>
        val pp = Templates.applyTemplatesToProject(templates, p)
        (name, pp)
      }

    val groupedCrossProjects: Map[model.ProjectName, ProjectKeepExploded] =
      groupCrossProjects(templated)

    val crossTemplates: SortedMap[TemplateDef, Templates.TemplateKeepExploded] = {
      val projects = groupedCrossProjects.collect { case (name, p) if !ignoreWhenInferringTemplates(name) => (name, p) }
      Templates.inferFromExistingProjects(
        TemplateDef.crossTemplates(projects.map(_._2.exploded)),
        projects.toList
      )
    }

    val groupedTemplatedCrossProjects: Map[model.ProjectName, ProjectKeepExploded] =
      groupedCrossProjects.map { case (name, p) =>
        val pp = Templates.applyTemplatesToProject(crossTemplates, p)
        (name, pp)
      }

    val build = model.Build(
      constants.$schema,
      JsonMap((templates ++ crossTemplates).map { case (templateDef, p) => (templateDef.templateId, p.current) }.filterNot(_._2.isEmpty)),
      JsonMap(build0.scripts),
      build0.resolvers,
      JsonMap(groupedTemplatedCrossProjects.map { case (n, cp) => (n, cp.current) })
    )

    garbageCollectTemplates(inlineTrivialTemplates(build))
  }

  /** Takes an exploded build, reapplies existing templates
    */
  def reapply(build0: ExplodedBuild, unexplodedTemplates: JsonMap[model.TemplateId, model.Project]): model.Build = {

    val deduplicated: Map[model.CrossProjectName, ProjectKeepExploded] = {
      val deduplicatedProjects = deduplicateDependencies(build0).projects
      build0.projects.map { case (name, exploded) => (name, ProjectKeepExploded(exploded, deduplicatedProjects(name))) }
    }

    val allTemplates: Map[model.TemplateId, TemplateKeepExploded] =
      build0.templates.map { case (name, t) => (name, TemplateKeepExploded(t, t)) }

    val templated: Map[model.CrossProjectName, ProjectKeepExploded] =
      deduplicated.map { case (name, proj) =>
        val pp = proj.map(current => reapplyTemplates(allTemplates, current))
        (name, pp)
      }

    val groupedCrossProjects: Map[model.ProjectName, ProjectKeepExploded] =
      groupCrossProjects(templated).map { case (name, p) =>
        val retainedTemplates = build0.retainCrossTemplates.getOrElse(name, JsonList.empty)
        val pp = p.copy(current = p.current.copy(`extends` = retainedTemplates))
        (name, pp)
      }

    val groupedTemplatedCrossProjects: Map[model.ProjectName, ProjectKeepExploded] =
      groupedCrossProjects.map { case (name, project) =>
        val pp = project.map(p => reapplyTemplates(allTemplates, p))
        (name, pp)
      }

    model.Build(
      constants.$schema,
      unexplodedTemplates,
      JsonMap(build0.scripts),
      build0.resolvers,
      JsonMap(groupedTemplatedCrossProjects.map { case (n, cp) => (n, cp.current) })
    )
  }

  def reapplyTemplates(templates: Map[model.TemplateId, TemplateKeepExploded], project: model.Project): model.Project = {
    // applying a template may remove other templateIds in list
    @tailrec
    def go(project: model.Project, `extends`: List[model.TemplateId]): model.Project =
      `extends` match {
        case templateId :: restTemplateIds =>
          val pp =
            if (project.`extends`.values.contains(templateId))
              project.removeAll(templates(templateId).exploded)
            else project

          go(pp, restTemplateIds)
        case Nil => project
      }
    go(project, project.`extends`.values)
  }

  def groupCrossProjects(projects: Map[model.CrossProjectName, ProjectKeepExploded]): Map[model.ProjectName, ProjectKeepExploded] =
    projects.toSeq.groupBy(_._1.name).map {
      case (name, Seq((model.CrossProjectName(_, None), one))) => (name, one)
      case (name, crossProjects) =>
        val compressingProjectByCrossId: Seq[(model.CrossId, ProjectKeepExploded)] =
          crossProjects.map { case (crossProjectName, p) => (crossProjectName.crossId.get, p) }

        val currentCross: model.Project = {
          val common = compressingProjectByCrossId.map(_._2.current).reduce(_.intersect(_))
          val cross = compressingProjectByCrossId.map { case (projectName, p) => (projectName, p.current.removeAll(common)) }.toMap
          common.copy(cross = JsonMap(cross))
        }
        val explodedCross = {
          val common = compressingProjectByCrossId.map(_._2.exploded).reduce(_.intersect(_))
          val cross = compressingProjectByCrossId.map { case (projectName, p) => (projectName, p.exploded) }.toMap
          common.copy(cross = JsonMap(cross))
        }

        (name, ProjectKeepExploded(explodedCross, currentCross))
    }

  def garbageCollectTemplates(b: model.Build): model.Build = {
    val seen = collection.mutable.Set.empty[model.TemplateId]
    def go(p: model.Project): Unit = {
      p.`extends`.values.foreach { templateId =>
        seen += templateId
        go(b.templates.value(templateId))
      }

      p.cross.value.values.foreach(go)
    }

    b.projects.value.values.foreach(go)

    b.copy(templates = JsonMap(b.templates.value.filter { case (templateId, _) => seen(templateId) }))
  }

  def inlineTrivialTemplates(b: model.Build): model.Build = {
    val toInline: Map[model.TemplateId, List[model.TemplateId]] =
      b.templates.value.collect { case (name, p) if p.copy(`extends` = JsonList.empty).isEmpty => (name, p.`extends`.values) }

    def expand(templateId: model.TemplateId): List[model.TemplateId] =
      toInline.get(templateId) match {
        case Some(replacements) => replacements.flatMap(expand)
        case None               => List(templateId)
      }

    def go(p: model.Project): model.Project =
      p.copy(
        `extends` = JsonList(p.`extends`.values.flatMap(expand).distinct),
        cross = JsonMap(p.cross.value.map { case (crossId, p) => (crossId, go(p)) })
      )

    b.copy(
      templates = JsonMap(b.templates.value.collect { case (templateId, p) if !toInline.contains(templateId) => (templateId, go(p)) }),
      projects = JsonMap(b.projects.value.map { case (name, p) => (name, go(p)) })
    )
  }

  def inferFromExistingProjects(
      applicableTemplateDefs: List[TemplateDef],
      projects: List[(model.ProjectName, ProjectKeepExploded)]
  ): SortedMap[TemplateDef, TemplateKeepExploded] = {
    val templateDefsWithProjects: Map[TemplateDef, List[(model.ProjectName, ProjectKeepExploded)]] =
      applicableTemplateDefs.map { templateDef =>
        val projectsForTemplate = projects.collect {
          case (name, pe @ ProjectKeepExploded(exploded, _)) if templateDef.include(exploded) => (name, pe)
        }
        (templateDef, projectsForTemplate)
      }.toMap

    val maybeTemplates: SortedMap[TemplateDef, Lazy[Option[TemplateKeepExploded]]] =
      rewriteDependentData[TemplateDef, List[(model.ProjectName, ProjectKeepExploded)], Option[TemplateKeepExploded]](templateDefsWithProjects) {
        case (_, projects, _) if projects.map(_._1).distinct.sizeIs < 2 => None
        case (templateDef, projects, eval)                              =>
          // check what all the picked projects have in common
          val maybeInitialTemplate: Option[TemplateKeepExploded] = {
            val current = projects.map(_._2.current).optReduce(_.intersectDropEmpty(_))
            val exploded = projects.map(_._2.exploded).optReduce(_.intersectDropEmpty(_))
            exploded.zip(current).map { case (exploded, current) => TemplateKeepExploded(exploded, current) }
          }

          val templateAfterParents: Option[TemplateKeepExploded] =
            maybeInitialTemplate
              .map(initialTemplate => applyTemplatesToTemplate(templateDef.allParents, eval.apply, initialTemplate))

          templateAfterParents
      }

    maybeTemplates.flatMap { case (templateDef, lazyP) => lazyP.forceGet.map(p => (templateDef, p)) }
  }

  def applyTemplatesToProject(templates: Map[TemplateDef, TemplateKeepExploded], project: ProjectKeepExploded): ProjectKeepExploded = {
    def go(project: ProjectKeepExploded, templateDef: TemplateDef): ProjectKeepExploded =
      if (project.current.`extends`.values.contains(templateDef.templateId)) project
      else {
        val projectAppliedTemplateParents = templateDef.allParents.foldLeft(project)(go)

        templates.get(templateDef) match {
          case Some(templateProject) => tryApplyTemplate(templateDef.templateId, templateProject, projectAppliedTemplateParents)
          case None                  => projectAppliedTemplateParents
        }
      }

    templates.keys.foldLeft(project)(go)
  }

  /* templates can extend templates*/
  def applyTemplatesToTemplate(
      templatesDefs: List[TemplateDef],
      getTemplate: TemplateDef => Lazy[Option[TemplateKeepExploded]],
      project: TemplateKeepExploded
  ): TemplateKeepExploded = {
    def go(project: TemplateKeepExploded, templateDef: TemplateDef): TemplateKeepExploded =
      if (project.current.`extends`.values.contains(templateDef.templateId)) project
      else {
        val projectAppliedTemplateParents = templateDef.allParents.foldLeft(project)(go)

        getTemplate(templateDef).forceGet match {
          case Some(templateProject) => tryApplyTemplate(templateDef.templateId, templateProject, projectAppliedTemplateParents.toProject).toTemplate
          case None                  => projectAppliedTemplateParents
        }
      }

    templatesDefs.foldLeft(project)(go)
  }

  def tryApplyTemplate(templateId: model.TemplateId, templateProject: TemplateKeepExploded, project: ProjectKeepExploded): ProjectKeepExploded = {
    val shortened = project.current.removeAll(templateProject.exploded)
    val isShortened = shortened != project.current

    def doesntAddNew: Boolean =
      templateProject.exploded.removeAll(project.exploded).isEmpty

    // primitive values will be overridden, so including these templates would be sound. however, it is confusing.
    def doesntHaveIncompatiblePrimitives: Boolean =
      sortedExtends(project.exploded.union(templateProject.exploded)) == sortedExtends(templateProject.exploded.union(project.exploded))

    // this is needed since we disregard `extends` within cross projects, and also trim empty projects in `doesntAddNew`
    def sameCrossVersions: Boolean =
      templateProject.exploded.cross.value.keys.forall(project.exploded.cross.value.keySet)

    if (isShortened && doesntAddNew && doesntHaveIncompatiblePrimitives && sameCrossVersions) {
      project.copy(
        current = shortened.withTemplate(templateId),
        exploded = project.exploded.withTemplate(templateId)
      )
    } else project
  }
}
