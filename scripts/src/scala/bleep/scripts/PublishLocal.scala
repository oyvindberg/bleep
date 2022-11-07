package bleep.scripts

import bleep._
import sbtdynver.DynVerPlugin

object PublishLocal extends BleepScript("PublishLocal") {
  def run(started: Started, commands: Commands, args: List[String]): Unit = {
    val dynVer = new DynVerPlugin(baseDirectory = started.buildPaths.buildDir.toFile, dynverSonatypeSnapshots = true)
    val projects = started.build.explodedProjects.keys.toList.filter(projectsToPublish.include)

    commands.publishLocal(
      bleep.commands.PublishLocal.Options(
        groupId = "build.bleep",
        version = dynVer.version,
        publishTarget = bleep.commands.PublishLocal.LocalIvy,
        projects = projects
      )
    )
  }
}
