package bleep
package scripts

import bleep.tasks.publishing._
import nosbt.InteractionService

import scala.collection.immutable.SortedMap

object Publish extends App {
  val groupId = "build.bleep"

  bootstrap.forScript("Publish") { case (started, commands) =>
    commands.compile(started.build.projects.keys.filter(projectsToPublish.include).toList)

    val dynVer = new DynVerPlugin(baseDirectory = started.buildPaths.buildDir.toFile, dynverSonatypeSnapshots = true)
    val pgp = new PgpPlugin(
      logger = started.logger,
      maybeCredentials = None,
      interactionService = InteractionService.DoesNotMaskYourPasswordExclamationOneOne
    )
    val sonatype = new Sonatype(
      logger = started.logger,
      sonatypeBundleDirectory = started.buildPaths.dotBleepDir / "sonatype-bundle",
      sonatypeProfileName = "build.bleep",
      bundleName = "bleep",
      version = dynVer.version,
      sonatypeCredentialHost = Sonatype.sonatype01
    )
    val ciRelease = new CiReleasePlugin(started.logger, sonatype, dynVer, pgp)

    started.logger.info(dynVer.version)

    val bundledProjects: SortedMap[model.CrossProjectName, Deployable] =
      fileBundle(
        started,
        asDep = (crossName, _) => Dep.Scala(groupId, dynVer.version, crossName.name.value),
        shouldInclude = projectsToPublish.include
      )

    val bundleFiles: Map[RelPath, Array[Byte]] =
      bundledProjects.flatMap { case (_, Deployable(_, files)) => files.all }

    ciRelease.ciRelease(bundleFiles)
  }
}
