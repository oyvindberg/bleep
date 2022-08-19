package bleep
package templates

import bleep.model.Project

import scala.collection.immutable.SortedSet

sealed trait TemplateDef {
  final val templateId: model.TemplateId =
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

  def name: String

  def parents: List[TemplateDef]

  def include(p: model.Project): Boolean
}

object TemplateDef {
  implicit val ordering: Ordering[TemplateDef] = Ordering.by(_.name)

  case object Common extends TemplateDef {
    override def name = "common"

    override def parents = Nil

    override def include(p: model.Project): Boolean = true
  }

  case class Test(parent: TemplateDef) extends TemplateDef {
    override def parents = List(parent)

    override def name = s"${parent.name}-test"

    override def include(p: model.Project): Boolean =
      parent.include(p) && p.isTestProject.getOrElse(false)
  }

  case class Main(parent: TemplateDef) extends TemplateDef {
    override def parents = List(parent)

    override def name = s"${parent.name}-main"

    override def include(p: model.Project): Boolean =
      parent.include(p) && !p.isTestProject.contains(true)
  }

  case class Platform(platformName: model.PlatformId) extends TemplateDef {
    override def parents: List[TemplateDef] = List.empty

    override def name = platformName.value

    override def include(p: model.Project): Boolean =
      parents.forall(_.include(p)) && p.cross.isEmpty && p.platform.exists(_.name.contains(platformName))
  }

  sealed trait ScalaTemplate extends TemplateDef

  case object Scala2 extends ScalaTemplate {
    override def name = s"scala-2"

    override def parents: List[TemplateDef] = List.empty

    override def include(p: model.Project): Boolean =
      parents.forall(_.include(p)) && p.cross.isEmpty && p.scala.flatMap(_.version).exists(_.epoch == '2')
  }

  case class ScalaBinVersion(binVersion: String, scala2: Option[Scala2.type]) extends ScalaTemplate {
    override def name = s"scala-$binVersion"

    override def parents: List[TemplateDef] = scala2.toList

    override def include(p: model.Project): Boolean =
      parents.forall(_.include(p)) && p.cross.isEmpty && p.scala.flatMap(_.version).exists(_.binVersion == binVersion)
  }

  case class CrossSetup(crossIds: SortedSet[model.CrossId], parents: List[TemplateDef], all: SortedSet[model.CrossId]) extends TemplateDef {
    override def toString: String = templateId.value

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

  case class PlatformScalaVersion(platform: Platform, scalaVersion: ScalaTemplate) extends TemplateDef {
    override def name = s"${scalaVersion.name}-${platform.name}"

    override def parents: List[TemplateDef] = {
      val scala2Platform = scalaVersion match {
        case ScalaBinVersion(_, Some(_)) => Some(PlatformScalaVersion(platform, scalaVersion = Scala2))
        case _                           => None
      }
      List(platform, scalaVersion) ++ scala2Platform
    }

    override def include(p: model.Project): Boolean = parents.forall(_.include(p))
  }

  case class Existing(originalTemplateId: model.TemplateId, parents: List[TemplateDef]) extends TemplateDef {
    override def name: String = originalTemplateId.value.replace("template-", "")

    override def include(p: Project): Boolean = p.`extends`.values.contains(templateId)
  }
}
