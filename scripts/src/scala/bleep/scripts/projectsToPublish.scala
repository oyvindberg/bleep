package bleep.scripts

import bleep.model

object projectsToPublish {
  // will publish these with dependencies
  val Wanted = Set(
    model.ProjectName("bleep-cli"),
    model.ProjectName("bleep-tasks"),
    model.ProjectName("bleep-tasks-publishing")
  )

  def include(crossName: model.CrossProjectName): Boolean =
    Wanted(crossName.name)
}
