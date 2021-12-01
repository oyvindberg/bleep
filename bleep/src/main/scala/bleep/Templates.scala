package bleep

import bleep.internal.{rewriteDependentData, Lazy}

import scala.collection.immutable.SortedMap

object Templates {
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
        projects.flatMap(p => p.platform.map(_.name)).distinct.map(platformId => Platform(platformId))
      val combined: List[PlatformScalaVersion] =
        scalas.flatMap(s => platforms.map(p => PlatformScalaVersion(p, s)))

      List(List(Common), scalas, platforms, combined).flatten.flatMap(t => List(t, Main(t), Test(t)))
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
        parents.forall(_.include(p)) && p.platform.exists(_.name == platformName)
    }

    case class ScalaVersion(binVersion: String) extends TemplateDef {
      override def name = s"scala-$binVersion"
      override def parents = List(Common)
      override def include(p: model.Project): Boolean =
        parents.forall(_.include(p)) && p.scala.flatMap(_.version).exists(_.binVersion == binVersion)
    }

    case class PlatformScalaVersion(platform: Platform, scalaVersion: ScalaVersion) extends TemplateDef {
      override def name = s"${scalaVersion.name}-${platform.name}"
      override def parents = List(platform, scalaVersion)
      override def include(p: model.Project): Boolean = parents.forall(_.include(p))
    }
  }

  def inferFromExistingProjects(projects: List[model.Project]): SortedMap[TemplateDef, model.Project] = {
    val applicableTemplateDefs: List[TemplateDef] =
      TemplateDef.applicableForProjects(projects)

    val templateDefsWithProjects: Map[TemplateDef, List[model.Project]] =
      applicableTemplateDefs.map(t => (t, projects.filter(t.include))).toMap

    val maybeTemplates: SortedMap[TemplateDef, Lazy[Option[model.Project]]] =
      rewriteDependentData[TemplateDef, List[model.Project], Option[model.Project]](templateDefsWithProjects) {
        case (_, projects, _) if projects.sizeIs < 2 => None
        case (templateDef, projects, eval)           =>
          // check what all the picked projects have in common
          val maybeInitialTemplate: Option[model.Project] =
            projects.optReduce(_.intersectDropEmpty(_))

          val templateAfterParents: Option[model.Project] =
            maybeInitialTemplate
              .map(initialTemplate => applyTemplates2(templateDef.allParents, eval.apply, initialTemplate))
              .filterNot(p => p.copy(`extends` = JsonList.empty).isEmpty)

          templateAfterParents
      }

    maybeTemplates.flatMap { case (templateDef, lazyP) => lazyP.forceGet.map(p => (templateDef, p)) }
  }

  def applyTemplates(templates: Map[TemplateDef, model.Project], project: model.Project): model.Project = {
    val applicableForProject: Map[TemplateDef, model.Project] =
      templates.filter { case (templateDef, _) => templateDef.include(project) }

    def go(project: model.Project, templateDef: TemplateDef): model.Project =
      if (project.`extends`.values.contains(templateDef.templateName)) project
      else {
        val projectAppliedTemplateParents = templateDef.allParents.foldLeft(project)(go)

        applicableForProject.get(templateDef: TemplateDef) match {
          case Some(templateProject) => applyTemplate(templateDef, templateProject, projectAppliedTemplateParents)
          case None                  => projectAppliedTemplateParents
        }
      }

    applicableForProject.keys.foldLeft(project)(go)
  }

  // same as above, with different signature. refactor sometime later
  def applyTemplates2(templatesDefs: List[TemplateDef], getTemplate: TemplateDef => Lazy[Option[model.Project]], project: model.Project): model.Project = {
    def go(project: model.Project, templateDef: TemplateDef): model.Project =
      if (project.`extends`.values.contains(templateDef.templateName)) project
      else {
        val projectAppliedTemplateParents = templateDef.allParents.foldLeft(project)(go)

        getTemplate(templateDef).forceGet match {
          case Some(templateProject) => applyTemplate(templateDef, templateProject, projectAppliedTemplateParents)
          case None                  => projectAppliedTemplateParents
        }
      }

    templatesDefs.foldLeft(project)(go)
  }

  def applyTemplate(templateDef: TemplateDef, templateProject: model.Project, project: model.Project): model.Project = {
    val shortened = project.removeAll(templateProject)
    val isShorted = shortened != project
    val isSafe = shortened.union(templateProject).copy(`extends` = project.`extends`) == project

    if (isShorted && isSafe) shortened.copy(`extends` = shortened.`extends` + templateDef.templateName)
    else project
  }
}
