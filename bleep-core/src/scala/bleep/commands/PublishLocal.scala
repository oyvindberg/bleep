package bleep
package commands

import bleep.internal.TransitiveProjects
import bleep.packaging.*

import java.nio.file.Path
import scala.collection.immutable.SortedMap

object PublishLocal {
  sealed trait PublishTarget {
    val path: Path
    val publishLayout: PublishLayout
  }

  case object LocalIvy extends PublishTarget {
    override val path: Path = constants.ivy2Path
    override val publishLayout: PublishLayout = PublishLayout.Ivy
  }

  case class CustomMaven(mavenRepo: model.Repository.MavenFolder) extends PublishTarget {
    val path: Path = mavenRepo.path
    override val publishLayout: PublishLayout = PublishLayout.Maven()
  }

  case class Options(
      groupId: String,
      version: String,
      publishTarget: PublishLocal.PublishTarget,
      projects: Array[model.CrossProjectName],
      manifestCreator: ManifestCreator
  )
}

case class PublishLocal(watch: Boolean, options: PublishLocal.Options, buildOpts: CommonBuildOpts) extends BleepBuildCommand {
  override def run(started: Started): Either[BleepException, Unit] =
    if (watch) WatchMode.run(started, s => TransitiveProjects(s.build, options.projects))(runOnce)
    else runOnce(started)

  private def runOnce(started: Started): Either[BleepException, Unit] =
    ReactiveBsp
      .compile(
        watch = false,
        projects = options.projects,
        displayMode = buildOpts.displayMode,
        flamegraph = buildOpts.flamegraph,
        cancel = buildOpts.cancel,
        noCache = false
      )
      .run(started)
      .map { case () =>
        val packagedLibraries: SortedMap[model.CrossProjectName, PackagedLibrary] =
          packageLibraries(
            started,
            coordinatesFor = CoordinatesFor.Default(groupId = options.groupId, version = options.version),
            shouldInclude = options.projects.toSet,
            publishLayout = options.publishTarget.publishLayout,
            manifestCreator = options.manifestCreator
          )

        packagedLibraries.foreach { case (projectName, PackagedLibrary(_, files)) =>
          FileSync
            .syncBytes(
              options.publishTarget.path,
              files.all,
              deleteUnknowns = FileSync.DeleteUnknowns.No,
              soft = false
            )
            .log(started.logger.withContext("projectName", projectName.value).withContext("version", options.version), "Published locally")
        }
        ()
      }
}
