package bleep.scripts

import bleep._
import bleep.commands.PublishLocalOptions
import bleep.packaging.PublishLayout
import bleep.tasks.publishing._

object PublishLocal extends BleepScript("PublishLocal") {
  def run(started: Started, commands: Commands, args: List[String]): Unit =
    commands.publishLocal(
      PublishLocalOptions(
        groupId = "build.bleep",
        version = new DynVerPlugin(baseDirectory = started.buildPaths.buildDir.toFile, dynverSonatypeSnapshots = true).version,
        to = None,
        projects = started.build.explodedProjects.keys.toList.filter(projectsToPublish.include),
        publishLayout = PublishLayout.Ivy
      )
    )
}
