package bleep
package commands

import bleep.internal.TransitiveProjects
import bleep.packaging.*
import bloop.rifle.BuildServer

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

case class PublishLocal(watch: Boolean, options: PublishLocal.Options) extends BleepCommandRemote(watch) with BleepCommandRemote.OnlyChanged {
  override def watchableProjects(started: Started): TransitiveProjects =
    TransitiveProjects(started.build, options.projects)

  override def onlyChangedProjects(started: Started, isChanged: model.CrossProjectName => Boolean): PublishLocal =
    copy(options = options.copy(projects = watchableProjects(started).transitiveFilter(isChanged).direct))

  override def runWithServer(started: Started, bloop: BuildServer): Either[BleepException, Unit] =
    Compile(watch = false, options.projects).runWithServer(started, bloop).map { case () =>
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
