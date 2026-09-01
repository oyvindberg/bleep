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
      version: PublishVersion,
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
        diffBase = None,
        diffOutput = OutputMode.Text
      )
      .run(started)
      .map { case () =>
        // No `--assert-release` here: publishing a snapshot into the local cache is the normal case, and the flag is about what leaves the machine.
        val version = PublishVersion.resolve(options.version, started.buildPaths.buildDir, assertRelease = false).orThrow
        val packagedLibraries: SortedMap[model.CrossProjectName, PackagedLibrary] =
          packageLibraries(
            started,
            coordinatesFor = CoordinatesFor.Default(groupId = options.groupId, version = version),
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
            .log(started.logger.withContext("projectName", projectName.value).withContext("version", version), "Published locally")
        }
        ()
      }
}
