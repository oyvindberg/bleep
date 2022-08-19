package bleep.templates

import bleep.model

object groupCrossProjects {
  def apply(projects: Map[model.CrossProjectName, model.ProjectWithExploded]): Map[model.ProjectName, model.ProjectWithExploded] =
    projects.toSeq.groupBy(_._1.name).map {
      case (name, Seq((model.CrossProjectName(_, None), one))) => (name, one)
      case (name, crossProjects) =>
        val compressingProjectByCrossId: Seq[(model.CrossId, model.ProjectWithExploded)] =
          crossProjects.map { case (crossProjectName, p) => (crossProjectName.crossId.get, p) }

        val currentCross: model.Project = {
          val common = compressingProjectByCrossId.map(_._2.current).reduce(_.intersect(_))
          val cross = compressingProjectByCrossId.map { case (crossId, p) => (crossId, p.current.removeAll(common)) }.toMap
          common.copy(cross = model.JsonMap(cross))
        }
        val explodedCross = {
          val common = compressingProjectByCrossId.map(_._2.exploded).reduce(_.intersect(_))
          val cross = compressingProjectByCrossId.map { case (crossId, p) => (crossId, p.exploded) }.toMap
          common.copy(cross = model.JsonMap(cross))
        }

        (name, model.ProjectWithExploded(explodedCross, currentCross))
    }

}
