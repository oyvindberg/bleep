package bleep
package commands

import bleep.packaging.{CoordinatesFor, PackagedLibrary, PublishLayout, packageLibraries}

import java.nio.file.Path
import scala.build.bloop.BloopServer
import scala.collection.immutable.SortedMap

case class PublishLocalOptions(groupId: String, version: String, to: Option[Path], projects: List[model.CrossProjectName], publishLayout: PublishLayout)

case class PublishLocal(started: Started, options: PublishLocalOptions) extends BleepCommandRemote(started) {
  override def runWithServer(bloop: BloopServer): Either[BleepException, Unit] =
    Compile(started, options.projects).runWithServer(bloop).map { case () =>
      val packagedLibraries: SortedMap[model.CrossProjectName, PackagedLibrary] =
        packageLibraries(
          started,
          coordinatesFor = CoordinatesFor.Default(groupId = options.groupId, version = options.version),
          shouldInclude = options.projects.toSet,
          publishLayout = options.publishLayout
        )

      packagedLibraries.foreach { case (projectName, PackagedLibrary(_, files)) =>
        FileSync
          .syncBytes(
            options.to.getOrElse(constants.ivy2Path),
            files.all,
            deleteUnknowns = FileSync.DeleteUnknowns.No,
            soft = false
          )
          .log(started.logger.withContext(projectName), "Published locally")
      }
      ()
    }
}
