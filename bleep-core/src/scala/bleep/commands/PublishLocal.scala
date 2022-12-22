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

  case class CustomMaven(mavenRepo: model.Repository.MavenFolder) extends PublishTarget {
    val path: Path = mavenRepo.path
    override val publishLayout: PublishLayout = PublishLayout.Maven()
  }

  case class Options(groupId: String, version: String, publishTarget: PublishLocal.PublishTarget, projects: Array[model.CrossProjectName])
}

case class PublishLocal(watch: Boolean, options: PublishLocal.Options) extends BleepCommandRemote(watch) with BleepCommandRemote.OnlyChanged {
  override def chosenProjects(started: Started): Array[model.CrossProjectName] = options.projects

  override def onlyChangedProjects(started: Started, isChanged: model.CrossProjectName => Boolean): PublishLocal = {
    val ps = options.projects.filter(p => isChanged(p) || started.build.transitiveDependenciesFor(p).keys.exists(isChanged))
    copy(options = options.copy(projects = ps))
  }

  override def runWithServer(started: Started, bloop: BloopServer): Either[BleepException, Unit] =
    Compile(watch = false, options.projects).runWithServer(started, bloop).map { case () =>
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
          .log(started.logger.withContext(projectName).withContext("version", options.version), "Published locally")
      }
      ()
    }
}
