package bleep.scripts

import bleep._
import bleep.tasks.publishing._

import scala.collection.immutable.SortedMap

object PublishLocal extends BleepScript("PublishLocal") {
  val groupId = "build.bleep"

  def run(started: Started, commands: Commands, args: List[String]): Unit = {
    val dynVer = new DynVerPlugin(baseDirectory = started.buildPaths.buildDir.toFile, dynverSonatypeSnapshots = true)
    started.logger.withContext(dynVer.version).info("publishing locally")
    commands.compile(started.build.explodedProjects.keys.filter(projectsToPublish.include).toList)

    val bundledProjects: SortedMap[model.CrossProjectName, Deployable] =
      fileBundle(
        started,
        asDep = (crossName, _) => model.Dep.Scala(org = groupId, name = crossName.name.value, version = dynVer.version),
        shouldInclude = projectsToPublish.include,
        bundleLayout = fileBundle.BundleLayout.Ivy
      )

    bundledProjects.foreach { case (projectName, Deployable(asDependency, files)) =>
      val synced = FileSync.syncBytes(
        constants.ivy2Path,
        files.all,
        deleteUnknowns = FileSync.DeleteUnknowns.No,
        soft = false
      )
      val ctxLogger = started.logger.withContext(projectName)
      synced.foreach { case (path, _) => ctxLogger.debug(path) }
      ctxLogger.withContext("dep", asDependency.module.repr).withContext("version", dynVer.version).info("Written")
    }
  }
}
