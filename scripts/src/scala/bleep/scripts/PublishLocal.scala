package bleep.scripts

import bleep._
import bleep.internal.formatsCrossProjectName
import bleep.tasks.publishing._

import scala.collection.immutable.SortedMap

object PublishLocal extends BleepScriptRunner("PublishLocal") {
  val groupId = "build.bleep"

  def runScript(started: Started, commands: Commands, args: List[String]): Unit = {
    val dynVer = new DynVerPlugin(baseDirectory = started.buildPaths.buildDir.toFile, dynverSonatypeSnapshots = true)
    started.logger.withContext(dynVer.version).info("publishing locally")
    commands.compile(started.build.projects.keys.filter(projectsToPublish.include).toList)

    val bundledProjects: SortedMap[model.CrossProjectName, Deployable] =
      fileBundle(
        started,
        asDep = (crossName, _) => model.Dep.Scala(org = groupId, name = crossName.name.value, version = dynVer.version),
        shouldInclude = projectsToPublish.include,
        bundleLayout = fileBundle.BundleLayout.Ivy
      )

    bundledProjects.foreach { case (projectName, Deployable(asDependency, files)) =>
      FileSync.syncBytes(
        constants.ivy2Path,
        files.all,
        deleteUnknowns = FileSync.DeleteUnknowns.No,
        soft = false
      )
      started.logger.withContext("dep", asDependency.module.repr).withContext(projectName).info("Written")
    }
  }
}
