package bleep

import bleep.internal.{rewriteDependentData, Lazy}

import scala.collection.immutable.{SortedMap, SortedSet}

object Templates {
  case class CompressingProject(exploded: model.Project, current: model.Project) {
    def toTemplate = CompressingTemplate(exploded, current)
  }
  case class CompressingTemplate(exploded: model.Project, current: model.Project) {
    def toProject = CompressingProject(exploded, current)
  }

  sealed trait TemplateDef {
    final lazy val templateName: model.TemplateId =
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
              .map(initialTemplate => applyTemplates2(templateDef.allParents, eval.apply, initialTemplate))
              .filterNot(p => p.current.copy(`extends` = JsonList.empty).isEmpty)

          templateAfterParents
      }

    maybeTemplates.flatMap { case (templateDef, lazyP) => lazyP.forceGet.map(p => (templateDef, p)) }
  }

  def applyTemplates(templates: Map[TemplateDef, CompressingTemplate], project: CompressingProject): CompressingProject = {
    def go(project: CompressingProject, templateDef: TemplateDef): CompressingProject =
      if (project.current.`extends`.values.contains(templateDef.templateName)) project
      else {
        val projectAppliedTemplateParents = templateDef.allParents.foldLeft(project)(go)

        templates.get(templateDef) match {
          case Some(templateProject) => applyTemplate(templateDef, templateProject, projectAppliedTemplateParents)
          case None                  => projectAppliedTemplateParents
        }
      }

    templates.keys.foldLeft(project)(go)
  }

  // same as above, with different signature. refactor sometime later
  def applyTemplates2(
      templatesDefs: List[TemplateDef],
      getTemplate: TemplateDef => Lazy[Option[CompressingTemplate]],
      project: CompressingTemplate
  ): CompressingTemplate = {
    def go(project: CompressingTemplate, templateDef: TemplateDef): CompressingTemplate =
      if (project.current.`extends`.values.contains(templateDef.templateName)) project
      else {
        val projectAppliedTemplateParents = templateDef.allParents.foldLeft(project)(go)

        getTemplate(templateDef).forceGet match {
          case Some(templateProject) => applyTemplate(templateDef, templateProject, projectAppliedTemplateParents.toProject).toTemplate
          case None                  => projectAppliedTemplateParents
        }
      }

    templatesDefs.foldLeft(project)(go)
  }

  def stripExtends(p: model.Project): model.Project =
    p.copy(
      `extends` = JsonList.empty,
      cross = JsonMap(p.cross.value.map { case (n, p) => (n, stripExtends(p)) }.filterNot { case (_, p) => p.isEmpty })
    )

  def applyTemplate(templateDef: TemplateDef, templateProject: CompressingTemplate, project: CompressingProject): CompressingProject = {
    val shortened = project.current.removeAll(templateProject.exploded)
    val isShortened = shortened != project.current

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
        current = shortened.copy(`extends` = shortened.`extends` + templateDef.templateName),
        exploded = project.exploded.copy(`extends` = project.exploded.`extends` + templateDef.templateName)
      )
    } else project
  }
}
