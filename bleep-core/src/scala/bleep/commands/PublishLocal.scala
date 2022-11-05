package bleep
package commands

import bleep.packaging.{packageLibraries, CoordinatesFor, PackagedLibrary, PublishLayout}

import java.nio.file.Path
import scala.build.bloop.BloopServer
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
  case class Custom(path: Path, publishLayout: PublishLayout) extends PublishTarget

  case class Options(groupId: String, version: String, publishTarget: PublishLocal.PublishTarget, projects: List[model.CrossProjectName])
}

case class PublishLocal(started: Started, options: PublishLocal.Options) extends BleepCommandRemote(started) {
  override def runWithServer(bloop: BloopServer): Either[BleepException, Unit] =
    Compile(started, options.projects.toArray).runWithServer(bloop).map { case () =>
      val packagedLibraries: SortedMap[model.CrossProjectName, PackagedLibrary] =
        packageLibraries(
          started,
          coordinatesFor = CoordinatesFor.Default(groupId = options.groupId, version = options.version),
          shouldInclude = options.projects.toSet,
          publishLayout = options.publishTarget.publishLayout
        )

      packagedLibraries.foreach { case (projectName, PackagedLibrary(_, files)) =>
        FileSync
          .syncBytes(
            options.publishTarget.path,
            files.all,
            deleteUnknowns = FileSync.DeleteUnknowns.No,
            soft = false
          )
          .log(started.logger.withContext(projectName), "Published locally")
      }
      ()
    }
}
