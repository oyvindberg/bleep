package bleep
package templates

import bleep.internal.rewriteDependentData

import scala.collection.immutable.SortedSet

trait TemplateDefs[Name] {
  def apply(projects: Map[Name, model.ProjectWithExploded], ignoreWhenInferringTemplates: Name => Boolean): List[TemplateDef]
}

object TemplateDefs {
  val project: TemplateDefs[model.CrossProjectName] = (projects0, ignoreWhenInferringTemplates) => {
    val projects = projects0.toList.collect { case (name, p) if !ignoreWhenInferringTemplates(name) => p.exploded }

    val scalas: List[TemplateDef.ScalaTemplate] =
      projects
        .flatMap(p => p.scala.flatMap(_.version))
        .flatMap { v =>
          val scala2 = if (v.epoch == '2') Some(TemplateDef.Scala2) else None
          val binVersion = TemplateDef.ScalaBinVersion(v.binVersion, scala2)
          scala2.toList ++ List(binVersion)
        }
        .distinct

    val platforms: List[TemplateDef.Platform] =
      projects.flatMap(p => p.platform.flatMap(_.name)).distinct.map(TemplateDef.Platform.apply)

    val combined: List[TemplateDef.PlatformScalaVersion] =
      scalas.flatMap(s => platforms.map(p => TemplateDef.PlatformScalaVersion(p, s)))

    val base = List(
      List(TemplateDef.Common),
      List(TemplateDef.Main(TemplateDef.Common)),
      scalas,
      //        scalas.map(Main.apply),
      platforms,
      //        platforms.map(Main.apply),
      combined,
      //        combined.map(Main.apply),
      // test
      List(TemplateDef.Test(TemplateDef.Common))
      //        scalas.map(Test.apply),
      //        platforms.map(Test.apply),
      //        combined.map(Test.apply)
    )
    base.flatten
  }

  private implicit val x: Ordering[SortedSet[model.CrossId]] = Ordering.by(_.mkString(""))

  val crossProject: TemplateDefs[model.ProjectName] = (crossProjects0, ignoreWhenInferringTemplates) => {
    val crossGroups: List[SortedSet[model.CrossId]] =
      crossProjects0
        .collect {
          case (crossName, p) if !ignoreWhenInferringTemplates(crossName) && p.exploded.cross.value.size > 1 =>
            SortedSet.empty[model.CrossId] ++ p.exploded.cross.value.keys
        }
        .toList
        .distinct

    val crossGroupsWithParentGroups: Map[SortedSet[model.CrossId], List[SortedSet[model.CrossId]]] =
      crossGroups.map { crossGroup =>
        val parentGroups = crossGroups.collect {
          case maybeParent if maybeParent.size < crossGroup.size && maybeParent.forall(crossGroup) => maybeParent
        }
        (crossGroup, parentGroups)
      }.toMap

    val crossSetups: Map[SortedSet[model.CrossId], TemplateDef] = {
      val allCross = SortedSet.empty[model.CrossId] ++ crossGroups.flatten

      rewriteDependentData(crossGroupsWithParentGroups).eager[TemplateDef] { case (crossGroup, parentGroups, eval) =>
        val parents = parentGroups.map(eval.apply)
        TemplateDef.CrossSetup(crossGroup, parents.map(_.forceGet), all = allCross)
      }
    }

    val sorted: List[TemplateDef] =
      crossSetups.values.toList.sortWith { case (one, two) =>
        def inherits(current: TemplateDef, maybeParent: model.TemplateId): Boolean =
          current.templateId == maybeParent || current.parents.exists(parent => inherits(parent, maybeParent))

        val oneInherits = inherits(one, two.templateId)
        val twoInherits = inherits(two, one.templateId)
        if (oneInherits) false
        else if (twoInherits) true
        else if (one.parents.length < two.parents.length) true
        else if (one.parents.length > two.parents.length) false
        else one.templateId.value.compareTo(two.templateId.value) < 0
      }

    sorted
  }
}
