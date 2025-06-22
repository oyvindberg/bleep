package bleep
package sbtimport

import cats.data.NonEmptyList

case class ImportFiltering(
    excludeProjects: Set[model.ProjectName],
    filterPlatforms: Option[NonEmptyList[model.PlatformId]],
    filterScalaVersions: Option[NonEmptyList[model.VersionScala]]
)

object ImportFiltering {
  val empty: ImportFiltering = ImportFiltering(
    excludeProjects = Set.empty,
    filterPlatforms = None,
    filterScalaVersions = None
  )
}
