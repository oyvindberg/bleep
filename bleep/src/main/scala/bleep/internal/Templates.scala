package bleep.internal

import bleep.{deduplicateDependencies, model, ExplodedBuild, JsonList, JsonMap}

import scala.collection.immutable.{SortedMap, SortedSet}

object Templates {
  case class CompressingProject(exploded: model.Project, current: model.Project) {
    def toTemplate = CompressingTemplate(exploded, current)
  }
  case class CompressingTemplate(exploded: model.Project, current: model.Project) {
    def toProject = CompressingProject(exploded, current)
  }
  case class CompressingProjects[Key](projects: Map[Key, CompressingProject]) {
    def map(f: (Key, CompressingProject) => model.Project): CompressingProjects[Key] =
      copy(projects = projects.map { case (n, cp) => (n, cp.copy(current = f(n, cp))) })
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
//        parents.forall(_.include(p)) &&
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

    val initial: CompressingProjects[model.CrossProjectName] =
      CompressingProjects(build0.projects.map { case (n, p) => (n, CompressingProject(p, p)) })

    val deduplicated: CompressingProjects[model.CrossProjectName] = {
      val build = build0.copy(projects = initial.projects.map { case (n, cp) => (n, cp.current) })
      val deduplicatedProjects = deduplicateDependencies(build).projects
      initial.map((n, _) => deduplicatedProjects(n))
    }

    val templates: SortedMap[TemplateDef, Templates.CompressingTemplate] = {
      val projects: List[CompressingProject] =
        deduplicated.projects.filterNot { case (crossName, _) => ignoreWhenInferringTemplates(crossName.name) }.values.toList

      Templates.inferFromExistingProjects(
        TemplateDef.applicableForProjects(projects.map(_.exploded)),
        projects
      )
    }

    val templated: CompressingProjects[model.CrossProjectName] = deduplicated.map { (name, cp) =>
      Templates.applyTemplatesToProject(templates, cp).current
    }

    val groupedCrossProjects: CompressingProjects[model.ProjectName] =
      CompressingProjects(
        templated.projects.toSeq.groupBy(_._1.name).map {
          case (name, Seq((model.CrossProjectName(_, None), one))) => (name, one)
          case (name, crossProjects) =>
            val compressingProjectByCrossId: Seq[(model.CrossId, CompressingProject)] =
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

            (name, CompressingProject(explodedCross, currentCross))
        }
      )

    val crossTemplates: SortedMap[TemplateDef, Templates.CompressingTemplate] = {
      val projects = groupedCrossProjects.projects.filterNot { case (name, _) => ignoreWhenInferringTemplates(name) }.values
      Templates.inferFromExistingProjects(
        TemplateDef.crossTemplates(projects.map(_.exploded)),
        projects.toList
      )
    }

    val groupedTemplatedCrossProjects: CompressingProjects[model.ProjectName] =
      groupedCrossProjects.map { case (name, project) =>
        Templates.applyTemplatesToProject(crossTemplates, project).current
      }

    val build = model.Build(
      build0.version,
      massageTemplates(templates ++ crossTemplates),
      build0.scripts,
      build0.resolvers,
      groupedTemplatedCrossProjects.projects.map { case (n, cp) => (n, cp.current) }
    )

    garbageCollectTemplates(build)
  }

  def garbageCollectTemplates(b: model.Build): model.Build = {
    val seen = collection.mutable.Set.empty[model.TemplateId]
    def go(p: model.Project): Unit = {
      p.`extends`.values.foreach { templateId =>
        seen += templateId
        go(b.templates.get(templateId))
      }

      p.cross.value.values.foreach(go)
    }

    b.projects.values.foreach(go)

    b.copy(templates = b.templates.map(_.filter { case (templateId, _) => seen(templateId) }))
  }

  def massageTemplates(templates: SortedMap[Templates.TemplateDef, CompressingTemplate]): Option[SortedMap[model.TemplateId, model.Project]] =
    Some(templates.map { case (templateDef, p) => (templateDef.templateId, p.current) }.filterNot(_._2.isEmpty))

  def inferFromExistingProjects(applicableTemplateDefs: List[TemplateDef], projects: List[CompressingProject]): SortedMap[TemplateDef, CompressingTemplate] = {
    val templateDefsWithProjects: Map[TemplateDef, List[model.Project]] =
      applicableTemplateDefs.map { templateDef =>
        val projectsForTemplate = projects.collect {
          case CompressingProject(exploded, current) if templateDef.include(exploded) => current
        }
        (templateDef, projectsForTemplate)
      }.toMap

    val maybeTemplates: SortedMap[TemplateDef, Lazy[Option[CompressingTemplate]]] =
      rewriteDependentData[TemplateDef, List[model.Project], Option[CompressingTemplate]](templateDefsWithProjects) {
        case (_, projects, _) if projects.sizeIs < 2 => None
        case (templateDef, projects, eval)           =>
          // check what all the picked projects have in common
          val maybeInitialTemplate: Option[CompressingTemplate] =
            projects.optReduce(_.intersectDropEmpty(_)).map(p => CompressingTemplate(p, p))

          val templateAfterParents: Option[CompressingTemplate] =
            maybeInitialTemplate
              .map(initialTemplate => applyTemplatesToTemplate(templateDef.allParents, eval.apply, initialTemplate))
              .filterNot(p => p.current.copy(`extends` = JsonList.empty).isEmpty)

          templateAfterParents
      }

    maybeTemplates.flatMap { case (templateDef, lazyP) => lazyP.forceGet.map(p => (templateDef, p)) }
  }

  def applyTemplatesToProject(templates: Map[TemplateDef, CompressingTemplate], project: CompressingProject): CompressingProject = {
    def go(project: CompressingProject, templateDef: TemplateDef): CompressingProject =
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
      getTemplate: TemplateDef => Lazy[Option[CompressingTemplate]],
      project: CompressingTemplate
  ): CompressingTemplate = {
    def go(project: CompressingTemplate, templateDef: TemplateDef): CompressingTemplate =
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

  def tryApplyTemplate(templateId: model.TemplateId, templateProject: CompressingTemplate, project: CompressingProject): CompressingProject = {
    val shortened = project.current.removeAll(templateProject.exploded)
    val isShortened = shortened != project.current

    def stripExtends(p: model.Project): model.Project =
      p.copy(
        `extends` = JsonList.empty,
        cross = JsonMap(p.cross.value.map { case (n, p) => (n, stripExtends(p)) }.filterNot { case (_, p) => p.isEmpty })
      )

    def doesntAddNew: Boolean =
      stripExtends(templateProject.exploded.removeAll(project.exploded)).isEmpty

    // primitive values will be overridden, so including these templates would be sound. however, it is confusing.
    def doesntHaveIncompatiblePrimitives: Boolean =
      stripExtends(project.exploded.union(templateProject.exploded)) == stripExtends(templateProject.exploded.union(project.exploded))

    // this is needed since we disregard `extends` within cross projects, and also trim empty projects in `doesntAddNew`
    def sameCrossVersions: Boolean =
      templateProject.exploded.cross.value.keys.forall(project.exploded.cross.value.keySet)

    if (isShortened && doesntAddNew && doesntHaveIncompatiblePrimitives && sameCrossVersions) {
      project.copy(
        current = shortened.copy(`extends` = shortened.`extends` + templateId),
        exploded = project.exploded.copy(`extends` = project.exploded.`extends` + templateId)
      )
    } else project
  }
}
