package bleep.scripts

import bleep.internal.FileUtils
import bleep._
import bleep.tasks.publishing._

import java.time.ZonedDateTime
import scala.collection.immutable.SortedMap

object PublishLocal extends App {
  val groupId = "build.bleep"

  bootstrap.forScript("PublishLocal") { case (started, commands) =>
    val dynVer = new DynVerPlugin(baseDirectory = started.buildPaths.buildDir.toFile, dynverSonatypeSnapshots = true)
    started.logger.withContext(dynVer.version).info("publishing locally")
    commands.compile(started.build.projects.keys.filter(projectsToPublish.include).toList)

    val bundledProjects: SortedMap[model.CrossProjectName, Deployable] =
      fileBundle(
        started,
        asDep = (crossName, _) => Dep.Scala(org = groupId, name = crossName.name.value, version = dynVer.version),
        shouldInclude = projectsToPublish.include,
        bundleLayout = fileBundle.BundleLayout.Ivy(published = ZonedDateTime.now())
      )

    bundledProjects.foreach { case (projectName, Deployable(asDependency, files)) =>
      FileUtils.syncBytes(
        constants.ivy2Path,
        files.all,
        deleteUnknowns = FileUtils.DeleteUnknowns.No,
        soft = false
      )
      started.logger.withContext("dep", asDependency.module.repr).withContext(projectName).info("Written")
    }
  }
}
